/*
 * Copyright (c) 2018-2026 LaserDisc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package laserdisc
package fs2

import _root_.fs2.Chunk
import cats.effect.kernel.{Deferred, Ref, Temporal}
import cats.effect.syntax.all.*
import cats.effect.{Async, Concurrent, Fiber, Resource}
import cats.syntax.all.*
import log.effect.fs2.LogSelector
import log.effect.fs2.syntax.*

import scala.concurrent.duration.*

object RedisClient {
  @inline final def apply[F[_]: Async: LogSelector]: RedisClientConnector[F] =
    new RedisClientConnector[F]()

  private[fs2] class RedisClientConnector[F[_]: Async: LogSelector]() {
    private implicit val _network: Network[F] = Network.forAsync[F]

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
      def redisChannel(address: RedisAddress, onConnected: F[Unit]): Pipe[F, RESP, RESP] =
        stream =>
          Stream.eval(address.toSocketAddress).flatMap { address =>
            stream.through(RedisChannel(address, receiveBufferSizeBytes, onConnected))
          }

      val establishedConnection: Resource[F, impl.Connection[F]] =
        impl.connection(redisChannel, addresses.toSeq)

      establishedConnection.flatMap(conn => impl.mkClient(conn))
    }
  }

  private[laserdisc] object impl {
    sealed trait Connection[F[_]] {
      def run: F[Fiber[F, Throwable, Unit]]
      def shutdown: F[Unit]
      def send[In <: Tuple, Out <: Tuple](
          in: In,
          timeout: FiniteDuration
      )(implicit handler: Handler.Aux[F, Env[F], In, Out]): F[Out]
    }

    sealed trait Publisher[F[_]] {
      def start: F[Connection[F]]
      def shutdown: F[Unit]
      def publish[In <: Tuple, Out <: Tuple](
          in: In,
          timeout: FiniteDuration
      )(implicit handler: Handler.Aux[F, Env[F], In, Out]): F[Out]
    }

    def mkClient[F[_]: Concurrent](establishedConn: Connection[F]): Resource[F, RedisClient[F]] =
      Resource.make(mkPublisher(establishedConn).flatMap(publ => publ.start.map(_ => publ)))(_.shutdown).map { publisher =>
        new RedisClient[F] {
          override final def send[In <: Tuple, Out <: Tuple](in: In, timeout: FiniteDuration)(
              implicit handler: Handler.Aux[F, Env[F], In, Out]
          ): F[Out] = publisher.publish(in, timeout)
        }
      }

    def connection[F[_]: Temporal](
        redisNetChannel: (RedisAddress, F[Unit]) => Pipe[F, RESP, RESP],
        addresses: Seq[RedisAddress]
    )(implicit logSelector: LogSelector[F]): Resource[F, Connection[F]] =
      Resource.eval(Deferred[F, Either[Throwable, Unit]]).flatMap { termSignal =>
        Resource.eval(Queue.bounded[F, Batch[F]](1024)).flatMap { queue =>
          Resource.eval((Ref.of[F, Vector[Request[F]]](Vector.empty), Ref.of[F, Int](0)).tupled).flatMap { case (inFlight, nextIndex) =>
            val log = logSelector.log
            // a batch the caller gave up on while it was queued isn't sent
            def push(batch: Batch[F]): F[Chunk[RESP]] =
              batch.claim.complete(()).flatMap {
                case true  => inFlight.modify(in => (in ++ batch.requests) -> Chunk.from(batch.requests.map(_.protocol.encode)))
                case false => Chunk.empty[RESP].pure[F]
              }

            def pop: F[Option[Request[F]]] =
              inFlight
                .modify { inFl =>
                  if (inFl.nonEmpty) inFl.tail -> Some(inFl.head)
                  else inFl                    -> None
                }

            // the requests sent on a connection that's gone will never get a reply
            def failInFlight(error: Throwable): F[Unit] =
              inFlight.getAndSet(Vector.empty).flatMap(_.traverse_(_.callback(Left(error))))

            // the addresses are tried in turn, starting from the first
            val nextAddress: F[RedisAddress] =
              nextIndex.getAndUpdate(i => (i + 1) % addresses.size).map(addresses(_))

            def serverAvailable(address: RedisAddress, onConnected: F[Unit]): Stream[F, Unit] =
              Stream
                .fromQueueUnterminated(queue, limit = 8 * 1024)
                .evalMap(push)
                .unchunks
                .through(redisNetChannel(address, onConnected))
                .evalMap { resp =>
                  pop.flatMap {
                    case Some(Request(protocol, cb)) => cb(protocol.decode(resp))
                    case None                        => Concurrent[F].raiseError[Unit](NoInFlightRequest(resp))
                    case Some(_)                     => absurd
                  }
                } ++ Stream.raiseError(ServerTerminatedConnection(address))

            val serverUnavailable: Stream[F, Unit] =
              log.errorS("Server unavailable for publishing") >>
                Stream
                  .fromQueueUnterminated(queue)
                  .evalMap(_.requests.traverse_(_.callback(Left(ServerUnavailable))))
                  .drain
                  .interruptAfter(3.seconds) // FIXME magic number

            // failures counts the addresses that couldn't be reached since the last connection
            def runner(failures: Int): Stream[F, Unit] =
              if (failures >= addresses.size) serverUnavailable ++ runner(0)
              else
                Stream.eval((nextAddress, Ref.of[F, Boolean](false)).tupled).flatMap { case (address, connected) =>
                  val onConnected = connected.set(true) >> log.info(s"Connected to server $address")

                  serverAvailable(address, onConnected).handleErrorWith { failure =>
                    log.errorS(s"Failure of publishing connection to $address", failure) >>
                      Stream.eval(failInFlight(failure) >> connected.get).flatMap { wasConnected =>
                        runner(if (wasConnected) 1 else failures + 1)
                      }
                  }
                }

            val newConnection: Connection[F] = new Connection[F] {
              override final def run: F[Fiber[F, Throwable, Unit]] =
                log.debug("Starting connection") >>
                  runner(0)
                    .interruptWhen(termSignal)
                    .compile
                    .drain
                    .attempt
                    .flatMap { r =>
                      log.info(
                        s"Connection terminated: ${r.fold(err => err.getMessage, _ => "No issues")}"
                      ) >> failInFlight(ClientTerminated)
                    }
                    .start

              override final def shutdown: F[Unit] =
                log.debug("Shutting down connection") >>
                  termSignal.complete(().asRight) >>
                  log.debug("Shutdown complete")

              override final def send[In <: Tuple, Out <: Tuple](
                  in: In,
                  timeout: FiniteDuration
              )(implicit handler: Handler.Aux[F, Env[F], In, Out]): F[Out] =
                handler(queue -> timeout, in)
            }

            Resource
              .make(newConnection.run.map(newConnection -> _)) { case (conn, fib) =>
                conn.shutdown >> fib.joinWith(Temporal[F].unit)
              }
              .map { case (conn, _) => conn }
          }
        }
      }

    def mkPublisher[F[_]: Concurrent](establishedConn: Connection[F]): F[Publisher[F]] = {
      sealed trait State extends Product with Serializable
      object State {
        final case class ConnectedState(established: Connection[F]) extends State
        case object ShutDownState                                   extends State
        final type ShutDownState = ShutDownState.type
        case object InitialState extends State
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

          def publish[In <: Tuple, Out <: Tuple](in: In, timeout: FiniteDuration)(
              implicit ev: Handler.Aux[F, Env[F], In, Out]
          ): F[Out] = {
            import State.*
            state.get.flatMap {
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
