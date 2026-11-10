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
        receiveBufferSizeBytes: Int = 8 * 1024 * 1024,
        connectTimeout: FiniteDuration = RedisChannel.DefaultConnectTimeout
    ): Resource[F, RedisClient[F]] =
      toNodes(Set(RedisAddress(host, port)), receiveBufferSizeBytes, connectTimeout)

    @inline final def toNodes(
        addresses: Set[RedisAddress],
        receiveBufferSizeBytes: Int = 8 * 1024 * 1024,
        connectTimeout: FiniteDuration = RedisChannel.DefaultConnectTimeout
    ): Resource[F, RedisClient[F]] = {
      def redisChannel(address: RedisAddress, onConnected: F[Unit]): Pipe[F, RESP, RESP] =
        stream =>
          Stream.eval(address.toSocketAddress).flatMap { address =>
            stream.through(RedisChannel(address, receiveBufferSizeBytes, connectTimeout, onConnected))
          }

      val establishedConnection: Resource[F, impl.Connection[F]] =
        impl.connection(redisChannel, addresses.toSeq)

      establishedConnection.flatMap(conn => impl.mkClient(conn))
    }
  }

  private[laserdisc] object impl {
    // how far a connection got, each stage after the one before: it's never set back, as a send can be taken before it connects
    private[fs2] sealed abstract class Usage(private val stage: Int) {
      final def max(other: Usage): Usage = if (stage >= other.stage) this else other
    }
    private[fs2] object Usage {
      case object Connecting extends Usage(0)
      case object Idle       extends Usage(1)
      case object Used       extends Usage(2)
      case object Answered   extends Usage(3)
    }

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
          // the requests sent and waiting for their replies, those of a batch the connection went before sending, the address to try
          // next, the number of the current connection, the number of the connection a WATCH last succeeded on, until a transaction or
          // UNWATCH ends it, and the batches to send again
          Resource
            .eval(
              (
                Ref.of[F, Vector[InFlight[F]]](Vector.empty),
                Ref.of[F, Vector[Request[F]]](Vector.empty),
                Ref.of[F, Int](0),
                Ref.of[F, Long](0L),
                Ref.of[F, Option[Long]](None),
                Ref.of[F, Chunk[Batch[F]]](Chunk.empty),
                Ref.of[F, Boolean](false)
              ).tupled
            )
            .flatMap { case (inFlight, unsentRequests, nextIndex, connections, watchedOn, retries, ended) =>
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
              // MULTI: until then nothing else is sent, so nothing else gets into the transaction. By then the replies to what was sent
              // before it are in, WATCH's included, so it's then that the client checks the watch: when the keys were watched on a
              // connection that's gone, the server dropped the watch with it, so the client sends DISCARD instead of the commands, and
              // UNWATCH, as a transaction ends the watch whether it runs or not
              def transaction(
                  address: RedisAddress,
                  connection: Long,
                  due: FiniteDuration,
                  sent: Ref[F, Int],
                  request: Request[F]
              ): Stream[F, RESP] =
                Stream.eval(Deferred[F, Boolean]).flatMap { accepted =>
                  val frames = request.protocol.frames
                  val multi  = Request[F, RESP](
                    Protocol("MULTI", EmptyTuple).using(RESPRead.raw),
                    {
                      case Right(Str("OK")) => accepted.complete(true).void
                      case Right(other)     =>
                        accepted.complete(false) >> request.callback(Left(RESPDecErr(s"Expected OK to MULTI but got: $other")))
                      case Left(refused: Err) => accepted.complete(false) >> request.callback(Left(refused))
                      // the connection went before MULTI was answered, so none of the transaction's commands were sent. A watch went
                      // with this connection or an earlier one, so the caller hears it was lost, as when it's found out after MULTI's OK
                      case Left(error) =>
                        accepted.complete(false) >> watchedOn.getAndSet(None).flatMap { watched =>
                          request.callback(Left(if (watched.isDefined) WatchLost(address) else error))
                        }
                    }
                  )
                  // a DISCARD or UNWATCH the server doesn't say OK to may leave the connection inside the transaction, where what's sent
                  // next would be queued instead of run, so the connection is closed, and the client connects again
                  def ending(command: String)(answered: F[Unit]): Request[F] =
                    Request[F, RESP](
                      Protocol(command, EmptyTuple).using(RESPRead.raw),
                      {
                        case Right(Str("OK")) => answered
                        case Right(other)     => answered >> Concurrent[F].raiseError(WatchCleanupRefused(address, command, other.toString))
                        case Left(refused: Err) =>
                          answered >> Concurrent[F].raiseError(WatchCleanupRefused(address, command, refused.message))
                        case Left(_) => answered
                      }
                    )

                  val sendingMulti = inFlight.modify(in => (in :+ InFlight(multi, due, 1, Vector.empty)) -> Chunk(frames.head))

                  // the transaction counts as sent once its commands are, or the DISCARD and UNWATCH sent instead of them, in the same
                  // step: until then, the connection going leaves it to be failed with the batch's unsent requests. When MULTI fails,
                  // its caller has heard already
                  Stream.evalUnChunk(sendingMulti) ++
                    Stream.eval(accepted.get).flatMap {
                      case true =>
                        Stream.eval(watchedOn.get).flatMap {
                          case Some(watched) if watched != connection =>
                            // the caller hears once the server has ended the watch
                            val lost    = watchedOn.set(None) >> request.callback(Left(WatchLost(address)))
                            val cleanup = sending(due, List(ending("DISCARD")(Concurrent[F].unit), ending("UNWATCH")(lost)))
                            Stream.evalUnChunk((cleanup <* sent.update(_ + 1)).uncancelable)
                          case _ =>
                            val commands =
                              inFlight
                                .modify(in => (in :+ InFlight(request, due, frames.size, Vector(Str("OK")))) -> Chunk.from(frames.tail))
                            Stream.evalUnChunk((commands <* sent.update(_ + 1)).uncancelable)
                        }
                      case false => Stream.exec(sent.update(_ + 1))
                    }
                }

              // sent counts the requests of the batch sent so far
              def sendAll(
                  address: RedisAddress,
                  connection: Long,
                  due: FiniteDuration,
                  sent: Ref[F, Int],
                  requests: List[Request[F]]
              ): Stream[F, RESP] = {
                val (before, rest) = requests.span(request => !isTransaction(request))
                Stream.evalUnChunk((sending(due, before) <* sent.update(_ + before.size)).uncancelable) ++ (rest match {
                  case request :: after =>
                    transaction(address, connection, due, sent, request) ++ sendAll(address, connection, due, sent, after)
                  case Nil => Stream.empty
                })
              }

              // when the connection goes, the requests of a batch that weren't sent yet, like those after a transaction waiting for
              // MULTI's OK, are failed with those in flight, with the same error. They're sent in order, so they're the ones after those
              // counted as sent, however the batch's stream ended: it can end early without an error, when what reads it stops
              def unsent(batch: Batch[F], sent: Ref[F, Int]): F[Unit] =
                sent.get.flatMap(n => unsentRequests.update(_ ++ batch.requests.drop(n)).whenA(n < batch.requests.size))

              // a batch the caller gave up on while it was queued isn't sent. The finalizer covers the claim too, as the stream can be
              // interrupted once the batch is claimed and before it's sent. Until the connection claims it, the batch has nothing for
              // the finalizer to fail: one it doesn't claim is sent again or was given up on
              def push(address: RedisAddress, connection: Long)(batch: Batch[F]): Stream[F, RESP] =
                Stream.eval(Ref.of[F, Int](batch.requests.size)).flatMap { sent =>
                  Stream
                    .eval(batch.claim.complete(()).flatTap(sent.set(0).whenA(_)).uncancelable)
                    .flatMap {
                      case true =>
                        Stream
                          .eval(Temporal[F].monotonic)
                          .flatMap(now => sendAll(address, connection, now + batch.timeout, sent, batch.requests.toList))
                      case false => Stream.empty
                    }
                    .onFinalize(unsent(batch, sent))
                }

              // what a WATCH that succeeded, an UNWATCH and a transaction that ran say about the watched keys, before the caller hears.
              // WATCH adds keys to those already watched, so the connection the first of them was watched on is kept: watching more
              // keys on a new connection doesn't bring back the watch that went with the old one. A transaction sent as plain commands,
              // as from the CLI, ends the watch with its EXEC or DISCARD, unless the server refuses them for want of a MULTI, and so
              // does RESET
              def watch(connection: Long, request: Request[F], replies: Vector[RESP]): F[Unit] =
                if (isTransaction(request)) watchedOn.set(None)
                else
                  (command(request), replies.headOption) match {
                    case (Some("WATCH"), Some(Str("OK")))                              => watchedOn.update(_.orElse(Some(connection)))
                    case (Some("UNWATCH" | "DISCARD"), Some(Str("OK")))                => watchedOn.set(None)
                    case (Some("EXEC"), Some(Err(m))) if m.startsWith("ERR")           => Concurrent[F].unit
                    case (Some("EXEC"), Some(_)) | (Some("RESET"), Some(Str("RESET"))) => watchedOn.set(None)
                    case _                                                             => Concurrent[F].unit
                  }

              // a request is answered once it has a reply for each frame it sent. Taking it off the requests in flight and answering it
              // go together, as nothing else would answer it
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
                  .uncancelable

              // the requests sent on a connection that's gone will never get a reply, and those of its batches it didn't get to send fail
              // with them. Like failQueued, it can't be stopped once it has taken them, or those it didn't get to would never be answered
              def failInFlight(error: Throwable): F[Unit] =
                (inFlight.getAndSet(Vector.empty), unsentRequests.getAndSet(Vector.empty)).flatMapN { (sent, unsent) =>
                  sent.traverse_(_.request.callback(Left(error))) >> unsent.traverse_(_.callback(Left(error)))
                }.uncancelable

              def failBatch(error: Throwable)(batch: Batch[F]): F[Unit] = batch.requests.traverse_(_.callback(Left(error)))

              def failQueued(error: Throwable): F[Unit] =
                (retries.getAndSet(Chunk.empty), queue.tryTakeN(None)).flatMapN { (again, queued) =>
                  (again.toList ++ queued).traverse_(failBatch(error))
                }.uncancelable

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

              // the batches are taken off the queue several at a time: when the connection goes, those of them it didn't get to weren't
              // sent, so they're sent again on the next connection, before the queue and in their order. One that's claimed already was
              // sent, or its caller gave up on it
              def unclaimed(taken: Chunk[Batch[F]]): F[Unit] =
                taken.filterA(_.claim.tryGet.map(_.isEmpty)).flatMap(again => retries.update(_ ++ again).whenA(again.nonEmpty))

              def serverAvailable(address: RedisAddress, connection: Long, usage: Ref[F, Usage]): Stream[F, Unit] = {
                val again       = Stream.eval(retries.getAndSet(Chunk.empty)).filter(_.nonEmpty)
                val queued      = Stream.fromQueueUnterminated(queue, limit = 8 * 1024).chunks
                val onConnected = log.info(s"Connected to server $address") >> usage.update(_.max(Usage.Idle))
                (again ++ queued)
                  .evalTap(_ => usage.update(_.max(Usage.Used)))
                  .flatMap(taken => Stream.chunk(taken).flatMap(push(address, connection)).onFinalize(unclaimed(taken)))
                  .through(redisNetChannel(address, onConnected))
                  .evalMap(resp => receive(connection)(resp) >> usage.set(Usage.Answered))
                  .concurrently(watchdog(address)) ++ Stream.raiseError(ServerTerminatedConnection(address))
              }

              // the batches are taken one at a time, each failed in the same step, so that stopping after 3 seconds leaves the next ones
              // queued
              val serverUnavailable: Stream[F, Unit] = {
                val again  = retries.getAndSet(Chunk.empty).flatMap(_.traverse_(failBatch(ServerUnavailable))).uncancelable
                val queued = Concurrent[F].uncancelable(poll => poll(queue.take).flatMap(failBatch(ServerUnavailable)))
                log.errorS("Server unavailable for publishing") >>
                  (Stream.exec(again) ++ Stream.repeatEval(queued).drain).interruptAfter(3.seconds) // FIXME magic number
              }

              // a connection that closed with nothing sent on it, as a server closes idle clients, is opened again only once there's
              // something to send: the send waiting for it goes first on the new connection
              val nextSend: F[Unit] =
                Concurrent[F].uncancelable(poll => poll(queue.take).flatMap(batch => retries.update(Chunk.singleton(batch) ++ _)))

              // failures counts the addresses that couldn't be reached, or that closed the connection before answering what was sent on
              // it, since the last connection that answered. When that one goes, every address is tried again, its own last, before the
              // client gives up: a server that answered may only have dropped the connection, and one that closes each connection at once,
              // as when it has too many clients, isn't tried again and again
              def runner(failures: Int): Stream[F, Unit] =
                if (failures >= addresses.size) serverUnavailable ++ runner(0)
                else
                  Stream.eval((nextAddress, Ref.of[F, Usage](Usage.Connecting), connections.updateAndGet(_ + 1)).tupled).flatMap {
                    case (address, usage, connection) =>
                      serverAvailable(address, connection, usage).handleErrorWith { failure =>
                        val idle = log.info(s"Connection to $address ended with nothing sent on it: ${failure.getMessage}") >> nextSend
                        Stream.eval(failInFlight(failure) >> usage.get).flatMap {
                          case Usage.Idle => Stream.exec(idle) ++ runner(failures)
                          case reached    =>
                            log.errorS(s"Failure of publishing connection to $address", failure) >>
                              runner(if (reached == Usage.Answered) 0 else failures + 1)
                        }
                      }
                  }

              // once the connection has ended, nothing takes from the queue, so a send queued after it fails at once, whether it came
              // before the queue was emptied or after
              val queueing: Batch[F] => F[Unit] =
                batch => queue.offer(batch) >> ended.get.ifM(failQueued(ClientTerminated), Temporal[F].unit)

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
                        ) >> ended.set(true) >> failInFlight(ClientTerminated) >> failQueued(ClientTerminated)
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
                  handler(queueing -> timeout, in)
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
