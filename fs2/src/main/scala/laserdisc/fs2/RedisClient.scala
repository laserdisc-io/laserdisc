package laserdisc
package fs2

import java.nio.channels.AsynchronousChannelGroup

import cats.Eq
import cats.effect.{ConcurrentEffect, ContextShift, Sync, Timer}
import cats.effect.concurrent.Ref
import cats.instances.option.catsStdInstancesForOption
import cats.instances.option.catsKernelStdEqForOption
import cats.instances.boolean.catsKernelStdOrderForBoolean
import cats.syntax.all._
import log.effect.LogWriter
import log.effect.fs2.syntax._
import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.sized.ToHList

import scala.collection.LinearSeq
import scala.concurrent.duration._

trait RedisClient[F[_]] {
  import Protocol.Aux

  def defaultTimeout: FiniteDuration = 20.seconds

  def send1[A](pA: Aux[A], timeout: FiniteDuration = defaultTimeout): F[Maybe[A]]

  def send2[A, B](pA: Aux[A], pB: Aux[B], timeout: FiniteDuration = defaultTimeout): F[(Maybe[A], Maybe[B])]

  def send3[A, B, C](
      pA: Aux[A],
      pB: Aux[B],
      pC: Aux[C],
      timeout: FiniteDuration = defaultTimeout
  ): F[(Maybe[A], Maybe[B], Maybe[C])]

  def send4[A, B, C, D](
      pA: Aux[A],
      pB: Aux[B],
      pC: Aux[C],
      pD: Aux[D],
      timeout: FiniteDuration = defaultTimeout
  ): F[(Maybe[A], Maybe[B], Maybe[C], Maybe[D])]

  def send5[A, B, C, D, E](
      pA: Aux[A],
      pB: Aux[B],
      pC: Aux[C],
      pD: Aux[D],
      pE: Aux[E],
      timeout: FiniteDuration = defaultTimeout
  ): F[(Maybe[A], Maybe[B], Maybe[C], Maybe[D], Maybe[E])]

  def send[In <: HList, Out <: HList](in: In, timeout: FiniteDuration = defaultTimeout)(
      implicit protocolHandler: ProtocolHandler.Aux[F, In, Out]
  ): F[Out]

  def sendN[CC[_] <: LinearSeq[_], A, N <: Nat, In <: HList, Out <: HList](
      sizedSeq: Sized[CC[Aux[A]], N],
      timeout: FiniteDuration = defaultTimeout
  )(
      implicit toHList: ToHList.Aux[CC[Aux[A]], N, In],
      protocolHandler: ProtocolHandler.Aux[F, In, Out],
      toSized: ToSized.Aux[Out, CC, Maybe[A], N]
  ): F[Sized[CC[Maybe[A]], N]]
}

object RedisClient {
  final def apply[F[_]: ConcurrentEffect: ContextShift: Timer: LogWriter](
      addresses: Set[RedisAddress],
      writeTimeout: Option[FiniteDuration] = Some(10.seconds),
      readMaxBytes: Int = 256 * 1024
  )(
      implicit acg: AsynchronousChannelGroup
  ): Stream[F, RedisClient[F]] = {

    def redisConnection(address: RedisAddress): Pipe[F, RESP, RESP] =
      stream =>
        Stream
          .eval(address.toInetSocketAddress)
          .flatMap(socketAddress => stream.through(RedisConnection(socketAddress, writeTimeout, readMaxBytes)))

    def connection: F[impl.Connection[F]] =
      impl.connection(redisConnection, impl.currentServer(addresses.toSeq))

    Stream.bracket(impl.mkClient(connection))({ case (_, shutdown) => shutdown }).map({ case (client, _) => client })
  }

  private[laserdisc] final object impl {

    sealed trait Connection[F[_]] {
      def run: F[Unit]
      def shutdown: F[Unit]
      def send[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
          implicit protocolHandler: ProtocolHandler.Aux[F, In, Out]
      ): F[Out]
    }

    sealed trait Publisher[F[_]] {
      def shutdown: F[Unit]
      def publish[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
          implicit protocolHandler: ProtocolHandler.Aux[F, In, Out]
      ): F[Out]
    }

    def mkClient[F[_]: ConcurrentEffect: Timer: LogWriter](connection: F[Connection[F]]): F[(RedisClient[F], F[Unit])] =
      mkPublisher(connection).map { publisher =>
        new RedisClient[F] {
          import Protocol.Aux

          override final def send1[A](pA: Aux[A], timeout: FiniteDuration): F[Maybe[A]] =
            publisher.publish(pA :: HNil, timeout).map(_.head)

          override final def send2[A, B](pA: Aux[A], pB: Aux[B], timeout: FiniteDuration): F[(Maybe[A], Maybe[B])] =
            publisher.publish(pA :: pB :: HNil, timeout).map(_.tupled)

          override final def send3[A, B, C](
              pA: Aux[A],
              pB: Aux[B],
              pC: Aux[C],
              timeout: FiniteDuration
          ): F[(Maybe[A], Maybe[B], Maybe[C])] =
            publisher.publish(pA :: pB :: pC :: HNil, timeout).map(_.tupled)

          override final def send4[A, B, C, D](
              pA: Aux[A],
              pB: Aux[B],
              pC: Aux[C],
              pD: Aux[D],
              timeout: FiniteDuration
          ): F[(Maybe[A], Maybe[B], Maybe[C], Maybe[D])] =
            publisher.publish(pA :: pB :: pC :: pD :: HNil, timeout).map(_.tupled)

          override final def send5[A, B, C, D, E](
              pA: Aux[A],
              pB: Aux[B],
              pC: Aux[C],
              pD: Aux[D],
              pE: Aux[E],
              timeout: FiniteDuration
          ): F[(Maybe[A], Maybe[B], Maybe[C], Maybe[D], Maybe[E])] =
            publisher.publish(pA :: pB :: pC :: pD :: pE :: HNil, timeout).map(_.tupled)

          override final def send[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
              implicit protocolHandler: ProtocolHandler.Aux[F, In, Out]
          ): F[Out] = publisher.publish(in, timeout)

          override final def sendN[CC[_] <: LinearSeq[_], A, N <: Nat, In <: HList, Out <: HList](
              sizedSeq: Sized[CC[Aux[A]], N],
              timeout: FiniteDuration
          )(
              implicit toHList: ToHList.Aux[CC[Aux[A]], N, In],
              protocolHandler: ProtocolHandler.Aux[F, In, Out],
              toSized: ToSized.Aux[Out, CC, Maybe[A], N]
          ): F[Sized[CC[Maybe[A]], N]] =
            publisher.publish(toHList(sizedSeq), timeout).map(_.toSized)

        } -> publisher.shutdown
      }

    def currentServer[F[_]: Sync](addresses: Seq[RedisAddress]): F[Option[RedisAddress]] =
      Stream.emits(addresses).covary[F].compile.last //FIXME yeah, well...

    def connection[F[_]: ConcurrentEffect: Timer](
        redisConnection: RedisAddress => Pipe[F, RESP, RESP],
        leader: F[Option[RedisAddress]]
    )(
        implicit logger: LogWriter[F]
    ): F[Connection[F]] =
      Signal[F, Boolean](false).flatMap { termSignal =>
        Queue.unbounded[F, Request[F]].flatMap { queue =>
          Ref.of[F, Vector[Request[F]]](Vector.empty).map { inFlight =>
            def push(req: Request[F]): F[RESP] = inFlight.modify(in => (in :+ req) -> req.protocol.encode)

            def pop: F[Option[Request[F]]] =
              inFlight
                .modify {
                  case h +: t => t     -> Some(h)
                  case other  => other -> None
                }

            def serverAvailable(address: RedisAddress): Stream[F, Unit] =
              logger.infoS(s"Server available for publishing: $address") >> {
                queue.dequeue
                  .evalMap(push)
                  .through(redisConnection(address))
                  .flatMap { resp =>
                    Stream.eval(pop).flatMap {
                      case None                        => Stream.raiseError(NoInFlightRequest(resp))
                      case Some(Request(protocol, cb)) => Stream.eval_(cb(protocol.decode(resp)))
                    }
                  } ++ Stream.raiseError(ServerTerminatedConnection(address))
              }

            val serverStream: Stream[F, Option[RedisAddress]] =
              Stream.eval(leader)

            def serverUnavailable: Stream[F, RedisAddress] =
              logger.errorS("Server unavailable for publishing") >>
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
                    case Some(address) => logger.debugS(s"Publisher got address: $address") >> Stream.emit(address)
                  }
                }

            def runner(knownServer: Stream[F, Option[RedisAddress]],
                       lastFailedServer: Option[RedisAddress] = None): Stream[F, Unit] =
              knownServer.flatMap {
                case None =>
                  serverUnavailable.flatMap(address => runner(Stream.emit(Some(address))))

                case Some(address) =>
                  lastFailedServer match {
                    case Some(failedAddress) if address === failedAddress =>
                      // this indicates that cluster sill thinks the address is same as the one that failed us, for that reason
                      // we have to suspend execution for while and retry in FiniteDuration
                      logger.warnS(s"New server is same like the old one ($address): currently unavailable") >>
                        serverUnavailable.flatMap(address => runner(Stream.emit(Some(address))))

                    case _ =>
                      // connection with address will always fail with error.
                      // TODO so when that happens, all open requests are completed
                      // and runner is rerun to switch likely to serverUnavailable.
                      // as the last action runner is restarted
                      serverAvailable(address).handleErrorWith { failure =>
                        logger.errorS(s"Failure of publishing connection to $address", failure) >>
                          runner(serverStream, Some(address))
                      }
                  }
              }

            new Connection[F] {
              override final def run: F[Unit] =
                logger.info("Starting connection") *>
                  runner(serverStream).interruptWhen(termSignal).compile.drain.attempt.flatMap { r =>
                    logger.info(s"Connection terminated: $r")
                  }

              override final def shutdown: F[Unit] =
                logger.info("Shutting down connection") *> termSignal.set(true)

              override final def send[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
                  implicit protocolHandler: ProtocolHandler.Aux[F, In, Out]
              ): F[Out] = protocolHandler(in, queue -> timeout)
            }
          }
        }
      }

    def mkPublisher[F[_]](createPublisher: => F[Connection[F]])(implicit F: ConcurrentEffect[F]): F[Publisher[F]] = {

      final class State(val hasShutdown: Boolean, val maybeConnection: Option[Connection[F]]) {
        def maybeSwapConnection(connection: Connection[F]): State =
          if (hasShutdown) this else new State(hasShutdown, Some(connection))
        def shutdown: State = new State(hasShutdown = true, maybeConnection)
      }

      final object State {
        val empty: State                                           = new State(hasShutdown = false, None)
        private[this] implicit val connectionEq: Eq[Connection[F]] = Eq.fromUniversalEquals
        implicit val stateEq: Eq[State] = Eq.instance { (s1, s2) =>
          s1.hasShutdown === s2.hasShutdown && s1.maybeConnection === s2.maybeConnection
        }
      }

      Ref.of(State.empty).map { state =>
        new Publisher[F] {
          val shutdown: F[Unit] =
            state
              .modify(currentState => (currentState.shutdown, currentState))
              .flatMap(_.maybeConnection.traverse_(_.shutdown))

          def publish[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
              implicit ev: ProtocolHandler.Aux[F, In, Out]
          ): F[Out] = state.get.map(_.maybeConnection).flatMap {
            case Some(connection) => connection.send(in, timeout)
            case None =>
              createPublisher.flatMap { connection =>
                state
                  .modify { currentState =>
                    val newState = currentState.maybeSwapConnection(connection)
                    (newState, (currentState, newState))
                  }
                  .flatMap {
                    case (p, _) if p.hasShutdown => F.raiseError(ClientTerminated)
                    case (p, n) if p =!= n       => F.start(connection.run) >> publish(in, timeout)
                    case _                       => publish(in, timeout)
                  }
              }
          }
        }
      }
    }
  }
}
