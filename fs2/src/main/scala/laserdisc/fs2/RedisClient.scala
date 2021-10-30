package laserdisc
package fs2

import cats.effect.kernel.{Deferred, Ref, Temporal}
import cats.effect.syntax.all._
import cats.effect.{Async, Concurrent, Fiber, Resource, Sync}
import cats.syntax.all._
import log.effect.fs2.LogSelector
import log.effect.fs2.syntax._
import shapeless.HList

import scala.concurrent.duration._

object RedisClient {
  @inline final def apply[F[_]: Async: LogSelector]: RedisClientConnector[F] =
    new RedisClientConnector[F]()

  private[fs2] class RedisClientConnector[F[_]: Async: LogSelector]() {
    @inline final def to(
        host: Host,
        port: Port,
        receiveBufferSizeBytes: Int = 8 * 1024 * 1024
    ): Resource[F, RedisClient[F]] =
      toNodes(Set(RedisAddress(host, port)), receiveBufferSizeBytes)

    @inline final def toNodes(
        addresses: Set[RedisAddress],
        receiveBufferSizeBytes: Int = 8 * 1024 * 1024
    ): Resource[F, RedisClient[F]] = {
      def redisChannel(address: RedisAddress): Pipe[F, RESP, RESP] =
        stream =>
          Stream.eval(address.toSocketAddress).flatMap { address =>
            stream.through(RedisChannel(address, receiveBufferSizeBytes))
          }

      val establishedConnection: Resource[F, impl.Connection[F]] =
        impl.connection(redisChannel, impl.currentServer(addresses.toSeq))

      establishedConnection.flatMap(conn => impl.mkClient(conn))
    }
  }

  private[laserdisc] final object impl {
    sealed trait Connection[F[_]] {
      def run: F[Fiber[F, Throwable, Unit]]
      def shutdown: F[Unit]
      def send[In <: HList, Out <: HList](
          in: In,
          timeout: FiniteDuration
      )(implicit handler: RedisHandler.Aux[F, In, Out]): F[Out]
    }

    sealed trait Publisher[F[_]] {
      def start: F[Connection[F]]
      def shutdown: F[Unit]
      def publish[In <: HList, Out <: HList](
          in: In,
          timeout: FiniteDuration
      )(implicit handler: RedisHandler.Aux[F, In, Out]): F[Out]
    }

    def mkClient[F[_]: Concurrent](establishedConn: Connection[F]): Resource[F, RedisClient[F]] =
      Resource.make(mkPublisher(establishedConn) >>= (publ => publ.start.map(_ => publ)))(_.shutdown) map { publisher =>
        new RedisClient[F] {
          override final def send[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
              implicit handler: RedisHandler.Aux[F, In, Out]
          ): F[Out] = publisher.publish(in, timeout)
        }
      }

    def currentServer[F[_]: Sync](addresses: Seq[RedisAddress]): F[Option[RedisAddress]] =
      Stream.emits(addresses).covary[F].compile.last // FIXME yeah, well...

    def connection[F[_]: Temporal](
        redisNetChannel: RedisAddress => Pipe[F, RESP, RESP],
        leader: F[Option[RedisAddress]]
    )(implicit logSelector: LogSelector[F]): Resource[F, Connection[F]] =
      Resource.eval(Deferred[F, Throwable | Unit]).flatMap { termSignal =>
        Resource.eval(Queue.bounded[F, Request[F]](1024)).flatMap { queue =>
          Resource.eval(Ref.of[F, Vector[Request[F]]](Vector.empty)).flatMap { inFlight =>
            val log = logSelector.log
            def push(req: Request[F]): F[RESP] =
              inFlight
                .modify(in => (in :+ req) -> req.protocol.encode)

            def pop: F[Option[Request[F]]] =
              inFlight
                .modify { inFl =>
                  if (inFl.nonEmpty) inFl.tail -> Some(inFl.head)
                  else inFl                    -> None
                }

            def serverAvailable(address: RedisAddress): Stream[F, Unit] =
              log.infoS(s"Connected to server $address") >> Stream
                .fromQueueUnterminated(queue, limit = 8 * 1024)
                .evalMap(push)
                .through(redisNetChannel(address))
                .evalMap { resp =>
                  pop >>= {
                    case Some(Request(protocol, cb)) => cb(protocol.decode(resp))
                    case None                        => Concurrent[F].raiseError[Unit](NoInFlightRequest(resp))
                    case Some(_)                     => absurd
                  }
                } ++ Stream.raiseError(ServerTerminatedConnection(address))

            val serverStream: Stream[F, Option[RedisAddress]] =
              Stream.eval(leader)

            def serverUnavailable: Stream[F, RedisAddress] =
              log.errorS("Server unavailable for publishing") >>
                Stream.eval(Signal[F, Option[RedisAddress]](None)).flatMap { serverSignal =>
                  val cancelIncoming = Stream.fromQueueUnterminated(queue).evalMap(_.callback(Left(ServerUnavailable))).drain
                  val queryLeader = (Stream.awakeEvery[F](3.seconds) >> serverStream) // FIXME magic number
                    .evalMap(maybeAddress => serverSignal.update(_ => maybeAddress))
                    .drain

                  cancelIncoming
                    .mergeHaltBoth(queryLeader)
                    .covaryOutput[RedisAddress]
                    .interruptWhen(serverSignal.map(_.nonEmpty)) ++ serverSignal.discrete.head.flatMap {
                    case None          => serverUnavailable // impossible
                    case Some(address) => log.debugS(s"Publisher got address: $address") >> Stream.emit(address)
                  }
                }

            def runner(knownServer: Stream[F, Option[RedisAddress]], lastFailedServer: Option[RedisAddress] = None): Stream[F, Unit] =
              knownServer.flatMap {
                case None =>
                  serverUnavailable.flatMap(address => runner(Stream.emit(Some(address))))

                case Some(address) =>
                  lastFailedServer match {
                    case Some(failedAddress) if address === failedAddress =>
                      // this indicates that cluster sill thinks the address is same as the one that failed us, for that reason
                      // we have to suspend execution for while and retry in FiniteDuration
                      log.warnS(s"New server is same like the old one ($address): currently unavailable") >>
                        serverUnavailable.flatMap(address => runner(Stream.emit(Some(address))))

                    case _ =>
                      // connection with address will always fail with error.
                      // TODO so when that happens, all open requests are completed
                      // and runner is rerun to switch likely to serverUnavailable.
                      // as the last action runner is restarted
                      serverAvailable(address) handleErrorWith { failure =>
                        log.errorS(s"Failure of publishing connection to $address", failure) >>
                          runner(serverStream, Some(address))
                      }
                  }
              }

            val newConnection: Connection[F] = new Connection[F] {
              override final def run: F[Fiber[F, Throwable, Unit]] =
                log.debug("Starting connection") >>
                  runner(serverStream)
                    .interruptWhen(termSignal)
                    .compile
                    .drain
                    .attempt
                    .flatMap { r =>
                      log.info(
                        s"Connection terminated: ${r.fold(err => err.getMessage, _ => "No issues")}"
                      )
                    }
                    .start

              override final def shutdown: F[Unit] =
                log.debug("Shutting down connection") >>
                  termSignal.complete(().asRight) >>
                  log.debug("Shutdown complete")

              override final def send[In <: HList, Out <: HList](
                  in: In,
                  timeout: FiniteDuration
              )(implicit handler: RedisHandler.Aux[F, In, Out]): F[Out] =
                handler(queue -> timeout, in)
            }

            Resource.make(newConnection.run.map(newConnection -> _)) { case (conn, fib) =>
              conn.shutdown >> fib.joinWith(Temporal[F].unit)
            } map { case (conn, _) => conn }
          }
        }
      }

    def mkPublisher[F[_]: Concurrent](establishedConn: Connection[F]): F[Publisher[F]] = {
      sealed trait State extends Product with Serializable
      object State {
        final case class ConnectedState(established: Connection[F]) extends State
        final case object ShutDownState                             extends State
        final type ShutDownState = ShutDownState.type
        final case object InitialState extends State
        final type InitialState = ShutDownState.type

        val empty: State = InitialState

        def tryStart(s: State, established: Connection[F]): State =
          s match {
            case InitialState                      => ConnectedState(established)
            case ConnectedState(_) | ShutDownState => s
          }
      }

      Ref.of(State.empty).map { state =>
        new Publisher[F] {
          val start: F[Connection[F]] =
            state.update { currentState =>
              State.tryStart(currentState, establishedConn)
            } >> Concurrent[F].pure(establishedConn)

          val shutdown: F[Unit] =
            state.set(State.ShutDownState)

          def publish[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
              implicit ev: RedisHandler.Aux[F, In, Out]
          ): F[Out] = {
            import State._
            state.get >>= {
              case ConnectedState(conn) => conn.send(in, timeout)
              case ShutDownState        => Concurrent[F].raiseError(ClientTerminated)
              case InitialState         => Concurrent[F].raiseError(ClientNotStartedProperly)
            }
          }
        }
      }
    }
  }
}
