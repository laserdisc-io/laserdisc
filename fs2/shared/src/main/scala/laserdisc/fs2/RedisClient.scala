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
import laserdisc.protocol.RESPRead
import log.effect.fs2.LogSelector
import log.effect.fs2.syntax.*

import scala.concurrent.duration.*

object RedisClient {
  @inline final def apply[F[_]: Async: Network: LogSelector]: RedisClientConnector[F] =
    new RedisClientConnector[F]()

  private[fs2] class RedisClientConnector[F[_]: Async: Network: LogSelector]() {

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
          // the requests sent and waiting for their replies, the address to try next, the number of the current connection, and the number
          // of the connection a WATCH last succeeded on, until a transaction or UNWATCH ends it
          Resource
            .eval(
              (Ref.of[F, Vector[InFlight[F]]](Vector.empty), Ref.of[F, Int](0), Ref.of[F, Long](0L), Ref.of[F, Option[Long]](None)).tupled
            )
            .flatMap { case (inFlight, nextIndex, connections, watchedOn) =>
              val log = logSelector.log

              def isTransaction(request: Request[F]): Boolean  = request.protocol.isInstanceOf[Protocol.Multi[?]]
              def command(request: Request[F]): Option[String] = request.protocol.frames.headOption.collect { case Arr(Bulk(name) :: _) =>
                name.toUpperCase
              }

              def sending(due: FiniteDuration, requests: List[Request[F]]): F[Chunk[RESP]] =
                if (requests.isEmpty) Chunk.empty[RESP].pure[F]
                else {
                  val frames = requests.map(_.protocol.frames)
                  val sent   = requests.zip(frames).map { case (request, fs) => InFlight(request, due, fs.size, Vector.empty) }
                  inFlight.modify(in => (in ++ sent) -> Chunk.from(frames.flatten))
                }

              // a transaction's MULTI goes alone, and the rest only once the server has said OK to it, so that nothing runs when it refuses
              // MULTI: until then nothing else is sent, so nothing else gets into the transaction. And a transaction isn't sent at all when
              // its keys were watched on a connection that's gone, as the server dropped the watch with that connection
              def transaction(address: RedisAddress, connection: Long, due: FiniteDuration, request: Request[F]): Stream[F, RESP] =
                Stream.eval(watchedOn.get).flatMap {
                  case Some(watched) if watched != connection =>
                    Stream.exec(watchedOn.set(None) >> request.callback(Left(WatchLost(address))))
                  case _ =>
                    Stream.eval(Deferred[F, Boolean]).flatMap { accepted =>
                      val frames = request.protocol.frames
                      val multi  = Request[F, RESP](
                        Protocol("MULTI", EmptyTuple).using(RESPRead.raw),
                        {
                          case Right(Str("OK")) => accepted.complete(true).void
                          case Right(other)     =>
                            accepted.complete(false) >> request.callback(Left(RESPDecErr(s"Expected OK to MULTI but got: $other")))
                          case Left(error) => accepted.complete(false) >> request.callback(Left(error))
                        }
                      )
                      Stream.evalUnChunk(inFlight.modify(in => (in :+ InFlight(multi, due, 1, Vector.empty)) -> Chunk(frames.head))) ++
                        Stream.eval(accepted.get).flatMap {
                          case true =>
                            Stream.evalUnChunk(
                              inFlight
                                .modify(in => (in :+ InFlight(request, due, frames.size, Vector(Str("OK")))) -> Chunk.from(frames.tail))
                            )
                          case false => Stream.empty
                        }
                    }
                }

              def sendAll(address: RedisAddress, connection: Long, due: FiniteDuration, requests: List[Request[F]]): Stream[F, RESP] = {
                val (before, rest) = requests.span(request => !isTransaction(request))
                Stream.evalUnChunk(sending(due, before)) ++ (rest match {
                  case request :: after => transaction(address, connection, due, request) ++ sendAll(address, connection, due, after)
                  case Nil              => Stream.empty
                })
              }

              // a batch the caller gave up on while it was queued isn't sent
              def push(address: RedisAddress, connection: Long)(batch: Batch[F]): Stream[F, RESP] =
                Stream.eval(batch.claim.complete(())).flatMap {
                  case true =>
                    Stream
                      .eval(Temporal[F].monotonic)
                      .flatMap(now => sendAll(address, connection, now + batch.timeout, batch.requests.toList))
                  case false => Stream.empty
                }

              // what a WATCH that succeeded, an UNWATCH and a transaction that ran say about the watched keys, before the caller hears
              def watch(connection: Long, request: Request[F], replies: Vector[RESP]): F[Unit] =
                if (isTransaction(request)) watchedOn.set(None)
                else
                  command(request) match {
                    case Some("WATCH") if replies.headOption.contains(Str("OK")) => watchedOn.set(Some(connection))
                    case Some("UNWATCH")                                         => watchedOn.set(None)
                    case _                                                       => Concurrent[F].unit
                  }

              // a request is answered once it has a reply for each frame it sent
              def receive(connection: Long)(resp: RESP): F[Unit] =
                inFlight
                  .modify {
                    case head +: rest =>
                      val received = head.replies :+ resp
                      if (received.size < head.expected) (head.copy(replies = received) +: rest) -> Right(None)
                      else rest                                                                  -> Right(Some(head.request -> received))
                    case empty => empty -> Left(NoInFlightRequest(resp))
                  }
                  .flatMap {
                    case Right(Some((request, replies))) =>
                      watch(connection, request, replies) >> request.callback(request.protocol.decodeReplies(replies))
                    case Right(None) => Concurrent[F].unit
                    case Left(error) => Concurrent[F].raiseError[Unit](error)
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

              def serverAvailable(address: RedisAddress, connection: Long, onConnected: F[Unit]): Stream[F, Unit] =
                Stream
                  .fromQueueUnterminated(queue, limit = 8 * 1024)
                  .flatMap(push(address, connection))
                  .through(redisNetChannel(address, onConnected))
                  .evalMap(receive(connection))
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
                  Stream.eval((nextAddress, Ref.of[F, Boolean](false), connections.updateAndGet(_ + 1)).tupled).flatMap {
                    case (address, connected, connection) =>
                      val onConnected = connected.set(true) >> log.info(s"Connected to server $address")

                      serverAvailable(address, connection, onConnected).handleErrorWith { failure =>
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
