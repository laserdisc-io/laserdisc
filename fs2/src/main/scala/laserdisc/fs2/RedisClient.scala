package laserdisc
package fs2

import java.nio.channels.AsynchronousChannelGroup

import _root_.fs2._
import _root_.fs2.async.Ref.Change
import cats.effect.{Effect, Sync}
import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist._
import shapeless.ops.sized.ToHList

import scala.collection.LinearSeq
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

trait RedisClient[F[_]] {
  import Protocol.Aux
  def send1[A](pA: Aux[A], timeout: FiniteDuration = 20.seconds): F[Maybe[A]]
  def send2[A, B](pA: Aux[A], pB: Aux[B], timeout: FiniteDuration = 20.seconds): F[(Maybe[A], Maybe[B])]
  def send3[A, B, C](
      pA: Aux[A],
      pB: Aux[B],
      pC: Aux[C],
      timeout: FiniteDuration = 20.seconds
  ): F[(Maybe[A], Maybe[B], Maybe[C])]
  def send4[A, B, C, D](
      pA: Aux[A],
      pB: Aux[B],
      pC: Aux[C],
      pD: Aux[D],
      timeout: FiniteDuration = 20.seconds
  ): F[(Maybe[A], Maybe[B], Maybe[C], Maybe[D])]
  def send5[A, B, C, D, E](
      pA: Aux[A],
      pB: Aux[B],
      pC: Aux[C],
      pD: Aux[D],
      pE: Aux[E],
      timeout: FiniteDuration = 20.seconds
  ): F[(Maybe[A], Maybe[B], Maybe[C], Maybe[D], Maybe[E])]
  def send[In <: HList, Out <: HList](in: In, timeout: FiniteDuration = 20.seconds)(
      implicit protocolHandler: ProtocolHandler.Aux[F, In, Out]
  ): F[Out]
  def sendN[CC[_] <: LinearSeq[_], A, N <: Nat, In <: HList, Out <: HList](
      sizedSeq: Sized[CC[Aux[A]], N],
      timeout: FiniteDuration = 20.seconds
  )(
      implicit toHList: ToHList.Aux[CC[Aux[A]], N, In],
      protocolHandler: ProtocolHandler.Aux[F, In, Out],
      toSized: ToSized.Aux[Out, CC, Maybe[A], N]
  ): F[Sized[CC[Maybe[A]], N]]
}

object RedisClient {
  final def apply[F[_]: Effect: Logger](
      addresses: Set[RedisAddress],
      writeTimeout: Option[FiniteDuration] = Some(10.seconds),
      readMaxBytes: Int = 256 * 1024
  )(
      implicit acg: AsynchronousChannelGroup,
      ec: ExecutionContext,
      scheduler: Scheduler
  ): Stream[F, RedisClient[F]] = {
    def redisConnection(address: RedisAddress): Pipe[F, RESP, RESP] =
      stream =>
        Stream
          .eval(address.toInetSocketAddress)
          .flatMap(socketAddress => stream.through(RedisConnection(socketAddress, writeTimeout, readMaxBytes)))

    def connection: F[impl.Connection[F]] = impl.connection(redisConnection, impl.currentServer(addresses.toSeq))

    Stream.bracket(impl.mkClient(connection))(
      { case (client, _)   => Stream.emit(client) },
      { case (_, shutdown) => shutdown }
    )
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

    def mkClient[F[_]: Effect: Logger](connection: F[Connection[F]])(
        implicit ec: ExecutionContext,
        scheduler: Scheduler
    ): F[(RedisClient[F], F[Unit])] = mkPublisher(connection).map { publisher =>
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
        ): F[(Maybe[A], Maybe[B], Maybe[C])] = publisher.publish(pA :: pB :: pC :: HNil, timeout).map(_.tupled)
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
        ): F[Sized[CC[Maybe[A]], N]] = publisher.publish(toHList(sizedSeq), timeout).map(_.toSized)
      } -> publisher.shutdown
    }

    def currentServer[F[_]: Sync](addresses: Seq[RedisAddress]): F[Option[RedisAddress]] =
      Stream.emits(addresses).covary[F].compile.last //FIXME yeah, well...

    def connection[F[_]](redisConnection: RedisAddress => Pipe[F, RESP, RESP], leader: F[Option[RedisAddress]])(
        implicit F: Effect[F],
        logger: Logger[F],
        scheduler: Scheduler,
        ec: ExecutionContext
    ): F[Connection[F]] = async.signalOf[F, Boolean](initialValue = false).flatMap { termSignal =>
      async.unboundedQueue[F, Request[F]].flatMap { queue =>
        async.refOf[F, Vector[Request[F]]](Vector.empty).map { inFlight =>
          def push(req: Request[F]): F[RESP] = inFlight.modify2(in => (in :+ req) -> req.protocol.encode).map(_._2)

          def pop: F[Option[Request[F]]] =
            inFlight
              .modify2 {
                case h +: t => t     -> Some(h)
                case other  => other -> None
              }
              .map(_._2)

          def serverAvailable(address: RedisAddress): Stream[F, Unit] =
            logger.infoS(s"Server available for publishing: $address") >> {
              queue.dequeue
                .evalMap(push)
                .through(redisConnection(address))
                .flatMap { resp =>
                  Stream.eval(pop).flatMap {
                    case None                        => Stream.raiseError[Unit](NoInflightRequest(resp))
                    case Some(Request(protocol, cb)) => Stream.eval_(cb(Right(protocol.decode(resp))))
                  }
                } ++ Stream.raiseError[Unit](ServerTerminatedConnection(address))
            }

          val serverStream: Stream[F, Option[RedisAddress]] = Stream.eval(leader)

          def serverUnavailable: Stream[F, RedisAddress] =
            logger.errorS("Server unavailable for publishing") >>
              Stream.eval(async.signalOf[F, Option[RedisAddress]](None)).flatMap { serverSignal =>
                val cancelIncoming = queue.dequeue.evalMap(_.callback(Left(ServerUnavailable))).drain
                val queryLeader = (scheduler.awakeEvery(3.seconds) >> serverStream) //FIXME magic number
                  .evalMap(maybeAddress => serverSignal.modify(_ => maybeAddress))
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
              case None => serverUnavailable.flatMap(address => runner(Stream.emit(Some(address))))
              case Some(address) =>
                lastFailedServer match {
                  case Some(failedAddress) if address == failedAddress =>
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
                      logger.errorS(s"Failure of publishing connection to $address", Some(failure)) >>
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

            override final def shutdown: F[Unit] = logger.info("Shutting down connection") *> termSignal.set(true)

            override final def send[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
                implicit protocolHandler: ProtocolHandler.Aux[F, In, Out]
            ): F[Out] = protocolHandler(in, queue -> timeout)
          }
        }
      }
    }

    def mkPublisher[F[_]](createPublisher: => F[Connection[F]])(
        implicit F: Effect[F],
        ec: ExecutionContext
    ): F[Publisher[F]] = {
      final case class State(hasShutdown: Boolean, maybeConnection: Option[Connection[F]]) {
        def maybeSwapConnection(connection: Connection[F]): State =
          if (hasShutdown) this else copy(maybeConnection = Some(connection))
        def shutdown: State = copy(hasShutdown = true)
      }
      val emptyState = State(hasShutdown = false, None)

      async.refOf(emptyState).map { state =>
        new Publisher[F] {
          def shutdown: F[Unit] = state.modify(_.shutdown).flatMap {
            case Change(p, _) =>
              import cats.instances.option.catsStdInstancesForOption
              p.maybeConnection.traverse_(_.shutdown)
          }

          def publish[In <: HList, Out <: HList](in: In, timeout: FiniteDuration)(
              implicit protocolHandler: ProtocolHandler.Aux[F, In, Out]
          ): F[Out] =
            state.get.map(_.maybeConnection).flatMap {
              case Some(connection) => connection.send(in, timeout)
              case None =>
                createPublisher.flatMap { connection =>
                  state
                    .modify(_.maybeSwapConnection(connection))
                    .flatMap {
                      case Change(p, _) if p.hasShutdown => F.raiseError[Out](ClientTerminated)
                      case Change(p, n) if p != n        => async.start(connection.run) >> publish(in, timeout)
                      case _                             => publish(in, timeout)
                    }
                }
            }
        }
      }
    }
  }
}
