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

    // once released, the client refuses new commands
    def mkClient[F[_]: Concurrent](establishedConn: Connection[F]): Resource[F, RedisClient[F]] =
      Resource.make(Ref.of[F, Boolean](false))(_.set(true)).map { released =>
        new RedisClient[F] {
          override final def send[In <: Tuple, Out <: Tuple](in: In, timeout: FiniteDuration)(
              implicit handler: Handler.Aux[F, Env[F], In, Out]
          ): F[Out] =
            released.get.ifM(Concurrent[F].raiseError(ClientTerminated), establishedConn.send(in, timeout))
        }
      }

    def connection[F[_]: Temporal](
        redisNetChannel: (RedisAddress, F[Unit]) => Pipe[F, RESP, RESP],
        addresses: Seq[RedisAddress]
    )(implicit logSelector: LogSelector[F]): Resource[F, Connection[F]] =
      Resource.eval(Deferred[F, Either[Throwable, Unit]]).flatMap { termSignal =>
        Resource.eval(Queue.bounded[F, Batch[F]](1024)).flatMap { queue =>
          // the requests sent and waiting for their replies
          Resource.eval((Ref.of[F, Vector[InFlight[F]]](Vector.empty), Ref.of[F, Int](0)).tupled).flatMap { case (inFlight, nextIndex) =>
            val log = logSelector.log
            // a batch the caller gave up on while it was queued isn't sent
            def push(batch: Batch[F]): F[Chunk[RESP]] =
              batch.claim.complete(()).flatMap {
                case true =>
                  Temporal[F].monotonic.flatMap { now =>
                    val due    = now + batch.timeout
                    val frames = batch.requests.map(_.protocol.frames)
                    val sent   = batch.requests.zip(frames).map { case (request, fs) => InFlight(request, due, fs.size, Vector.empty) }
                    inFlight.modify(in => (in ++ sent) -> Chunk.from(frames.flatten))
                  }
                case false => Chunk.empty[RESP].pure[F]
              }

            // a request is answered once it has a reply for each frame it sent
            def receive(resp: RESP): F[Unit] =
              inFlight
                .modify {
                  case head +: rest =>
                    val received = head.replies :+ resp
                    if (received.size < head.expected) (head.copy(replies = received) +: rest) -> Right(None)
                    else rest                                                                  -> Right(Some(head.request -> received))
                  case empty => empty -> Left(NoInFlightRequest(resp))
                }
                .flatMap {
                  case Right(Some((Request(protocol, cb), replies))) => cb(protocol.decodeReplies(replies))
                  case Right(Some(_))                                => absurd
                  case Right(None)                                   => Concurrent[F].unit
                  case Left(error)                                   => Concurrent[F].raiseError[Unit](error)
                }

            // the requests sent on a connection that's gone will never get a reply
            def failInFlight(error: Throwable): F[Unit] =
              inFlight.getAndSet(Vector.empty).flatMap(_.traverse_(_.request.callback(Left(error))))

            def failQueued(error: Throwable): F[Unit] =
              queue.tryTakeN(None).flatMap(_.traverse_(_.requests.traverse_(_.callback(Left(error)))))

            // replies come in order, so when the oldest one is overdue the server has stopped answering
            def watchdog(address: RedisAddress): Stream[F, Unit] =
              Stream.awakeEvery[F](1.second).evalMap { _ =>
                (inFlight.get, Temporal[F].monotonic).flatMapN {
                  case (oldest +: _, now) if now > oldest.due => Temporal[F].raiseError[Unit](ServerNotResponding(address))
                  case _                                      => Temporal[F].unit
                }
              }

            // the addresses are tried in turn, starting from the first
            val nextAddress: F[RedisAddress] =
              nextIndex.getAndUpdate(i => (i + 1) % addresses.size).map(addresses(_))

            def serverAvailable(address: RedisAddress, onConnected: F[Unit]): Stream[F, Unit] =
              Stream
                .fromQueueUnterminated(queue, limit = 8 * 1024)
                .evalMap(push)
                .unchunks
                .through(redisNetChannel(address, onConnected))
                .evalMap(receive)
                .concurrently(watchdog(address)) ++ Stream.raiseError(ServerTerminatedConnection(address))

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
                      ) >> failInFlight(ClientTerminated) >> failQueued(ClientTerminated)
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
  }
}
