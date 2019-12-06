package laserdisc
package fs2

import cats.Eq
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Blocker, Concurrent, ContextShift, Fiber, Resource, Sync, Timer}
import cats.syntax.all._
import log.effect.LogWriter
import shapeless._
import cats.effect.syntax.concurrent._
import log.effect.fs2.syntax._

import scala.concurrent.duration._

object RedisClient {

  /**
    * Creates a redis client that will handle the blocking network
    * connection's operations on a cached thread pool.
    */
  @inline final def apply[F[_]: Concurrent: ContextShift: Timer: LogWriter](
      addresses: Set[RedisAddress],
      writeTimeout: Option[FiniteDuration] = Some(10.seconds),
      readMaxBytes: Int = 256 * 1024
  ): Resource[F, RedisClient[F]] =
    Blocker[F] >>= (blockingPool => blockingOn(blockingPool)(addresses, writeTimeout, readMaxBytes))

  /**
    * Creates a redis client for a single redis node
    * that will handle the blocking network connection's
    * operations on a cached thread pool.
    */
  @inline final def toNode[F[_]: Concurrent: ContextShift: Timer: LogWriter](
      host: Host,
      port: Port,
      writeTimeout: Option[FiniteDuration] = Some(10.seconds),
      readMaxBytes: Int = 256 * 1024
  ): Resource[F, RedisClient[F]] =
    Blocker[F] >>= (blockingPool => blockingOn(blockingPool)(Set(RedisAddress(host, port)), writeTimeout, readMaxBytes))

  /**
    * Creates a redis client for a single redis node
    * that will handle the blocking network connection's
    * operations on a cached thread pool.
    */
  @inline final def toNodeBlockingOn[F[_]: Concurrent: ContextShift: Timer: LogWriter](
      blockingPool: Blocker
  )(
      host: Host,
      port: Port,
      writeTimeout: Option[FiniteDuration] = Some(10.seconds),
      readMaxBytes: Int = 256 * 1024
  ): Resource[F, RedisClient[F]] =
    blockingOn(blockingPool)(Set(RedisAddress(host, port)), writeTimeout, readMaxBytes)

  /**
    * Creates a redis client allowing to specify what blocking
    * thread pool will be used to handle the blocking network
    * connection's operations.
    */
  @inline final def blockingOn[F[_]: Concurrent: ContextShift: Timer: LogWriter](
      blockingPool: Blocker
  )(
      addresses: Set[RedisAddress],
      writeTimeout: Option[FiniteDuration] = Some(10.seconds),
      readMaxBytes: Int = 256 * 1024
  ): Resource[F, RedisClient[F]] = {
    def redisConnection(address: RedisAddress): Pipe[F, RESP, RESP] =
      stream =>
        Stream.eval(address.toInetSocketAddress) >>= { address =>
          stream.through(
            RedisChannel(address, writeTimeout, readMaxBytes)(blockingPool)
          )
        }

    val establishedConnection: Resource[F, impl.Connection[F]] =
      impl.connection(redisConnection, impl.currentServer(addresses.toSeq))

    establishedConnection >>= { conn =>
      impl.mkClient(conn)
    }
  }

  private[laserdisc] final object impl {
    sealed trait Connection[F[_]] {
      def run: F[Fiber[F, Unit]]
      def shutdown: F[Unit]
      def send[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
          implicit handler: RedisHandler.Aux[F, In, Out]
      ): F[Out]
    }

    sealed trait Publisher[F[_]] {
      def start: F[Connection[F]]
      def shutdown: F[Unit]
      def publish[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
          implicit handler: RedisHandler.Aux[F, In, Out]
      ): F[Out]
    }

    def mkClient[F[_]: Concurrent: Timer: LogWriter](establishedConn: Connection[F]): Resource[F, RedisClient[F]] =
      Resource.make(mkPublisher(establishedConn) >>= { publ =>
        publ.start map (_ => publ)
      })(_.shutdown) map { publisher =>
        new RedisClient[F] {
          override final def send[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
              implicit handler: RedisHandler.Aux[F, In, Out]
          ): F[Out] = publisher.publish(in, timeout)
        }
      }

    def currentServer[F[_]: Sync](addresses: Seq[RedisAddress]): F[Option[RedisAddress]] =
      Stream.emits(addresses).covary[F].compile.last //FIXME yeah, well...

    def connection[F[_]: Concurrent: Timer: LogWriter](
        redisConnection: RedisAddress => Pipe[F, RESP, RESP],
        leader: F[Option[RedisAddress]]
    ): Resource[F, Connection[F]] =
      Resource.liftF(Deferred[F, Throwable | Unit]) >>= { termSignal =>
        Resource.liftF(Queue.unbounded[F, Request[F]]) >>= { queue =>
          Resource.liftF(Ref.of[F, Vector[Request[F]]](Vector.empty)) >>= { inFlight =>
            def push(req: Request[F]): F[RESP] =
              inFlight
                .modify { in =>
                  (in :+ req) -> req.protocol.encode
                }

            def pop: F[Option[Request[F]]] =
              inFlight
                .modify {
                  case h +: t => t     -> Some(h)
                  case other  => other -> None
                }

            def serverAvailable(address: RedisAddress): Stream[F, Unit] =
              LogWriter.infoS(s"Server available for publishing: $address") >>
                queue.dequeue
                  .evalMap(push)
                  .through(redisConnection(address))
                  .evalMap { resp =>
                    pop >>= {
                      case Some(Request(protocol, cb)) => cb(protocol.decode(resp))
                      case None                        => Concurrent[F].raiseError[Unit](NoInFlightRequest(resp))
                    }
                  } ++ Stream.raiseError(ServerTerminatedConnection(address))

            val serverStream: Stream[F, Option[RedisAddress]] =
              Stream.eval(leader)

            def serverUnavailable: Stream[F, RedisAddress] =
              LogWriter.errorS("Server unavailable for publishing") >>
                Stream.eval(Signal[F, Option[RedisAddress]](None)).flatMap { serverSignal =>
                  val cancelIncoming = queue.dequeue.evalMap(_.callback(Left(ServerUnavailable))).drain
                  val queryLeader = (Stream.awakeEvery[F](3.seconds) >> serverStream) //FIXME magic number
                    .evalMap(maybeAddress => serverSignal.update(_ => maybeAddress))
                    .drain

                  cancelIncoming
                    .mergeHaltBoth(queryLeader)
                    .covaryOutput[RedisAddress]
                    .interruptWhen(serverSignal.map(_.nonEmpty)) ++ serverSignal.discrete.head.flatMap {
                    case None          => serverUnavailable // impossible
                    case Some(address) => LogWriter.debugS(s"Publisher got address: $address") >> Stream.emit(address)
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
                      LogWriter.warnS(s"New server is same like the old one ($address): currently unavailable") >>
                        serverUnavailable.flatMap(address => runner(Stream.emit(Some(address))))

                    case _ =>
                      // connection with address will always fail with error.
                      // TODO so when that happens, all open requests are completed
                      // and runner is rerun to switch likely to serverUnavailable.
                      // as the last action runner is restarted
                      serverAvailable(address) handleErrorWith { failure =>
                        LogWriter.errorS(s"Failure of publishing connection to $address", failure) >>
                          runner(serverStream, Some(address))
                      }
                  }
              }

            val newConnection = new Connection[F] {
              override final def run: F[Fiber[F, Unit]] =
                LogWriter.info("Starting connection") >>
                  runner(serverStream)
                    .interruptWhen(termSignal)
                    .compile
                    .drain
                    .attempt
                    .flatMap { r =>
                      LogWriter.info(
                        s"Connection terminated: ${r.fold(err => err.getMessage, _ => "No issues")}"
                      )
                    }
                    .start

              override final def shutdown: F[Unit] =
                LogWriter.info("Shutting down connection") >>
                  termSignal.complete(().asRight) >>
                  LogWriter.info("Shutdown complete")

              override final def send[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
                  implicit handler: RedisHandler.Aux[F, In, Out]
              ): F[Out] = handler(queue -> timeout, in)
            }

            Resource.make(newConnection.run map (newConnection -> _)) {
              case (conn, fib) => conn.shutdown >> fib.join
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

        private[this] implicit val connectionEq: Eq[Connection[F]] =
          Eq.fromUniversalEquals

        implicit val stateEq: Eq[State] = Eq.instance {
          case (ConnectedState(c1), ConnectedState(c2))                      => c1 === c2
          case (ShutDownState, ShutDownState) | (InitialState, InitialState) => true
          case _                                                             => false
        }
      }

      Ref.of(State.empty).map { state =>
        new Publisher[F] {
          val start: F[Connection[F]] =
            (state update { currentState =>
              State.tryStart(currentState, establishedConn)
            }) >> Concurrent[F].pure(establishedConn)

          val shutdown: F[Unit] =
            state set State.ShutDownState

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
