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
import cats.effect.std.Mutex
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

    /** With credentials, each connection logs in before anything else, and with a database it selects it, so a client that reconnects is
      * logged in and in the same database. A server refusing either stops the client with [[ConnectionSetupRefused]], unless it refuses
      * only for now, as while it loads its data or has too many clients: then the client connects again. With [[Tls]], each connection is
      * encrypted, and the server's certificate checked
      */
    @inline final def to(
        host: Host,
        port: Port,
        receiveBufferSizeBytes: Int = 8 * 1024 * 1024,
        connectTimeout: FiniteDuration = RedisChannel.DefaultConnectTimeout,
        credentials: Option[Credentials] = None,
        database: Option[DbIndex] = None,
        tls: Option[Tls[F]] = None
    ): Resource[F, RedisClient[F]] =
      toNodes(Set(RedisAddress(host, port)), receiveBufferSizeBytes, connectTimeout, credentials, database, tls)

    /** Like `to`, trying each address in turn, with the same credentials, database and TLS for each */
    @inline final def toNodes(
        addresses: Set[RedisAddress],
        receiveBufferSizeBytes: Int = 8 * 1024 * 1024,
        connectTimeout: FiniteDuration = RedisChannel.DefaultConnectTimeout,
        credentials: Option[Credentials] = None,
        database: Option[DbIndex] = None,
        tls: Option[Tls[F]] = None
    ): Resource[F, RedisClient[F]] =
      connection(addresses.toSeq, receiveBufferSizeBytes, connectTimeout, credentials, database, tls).flatMap(impl.mkClient(_))

    /** A client of its own, for one caller at a time: it sends one command at a time, each once the one before it has returned, so it can
      * also send the commands under `blocking.forever`, which wait for as long as it takes. Cancelling one of those once it's sent closes
      * the connection, as the server is still waiting on it, and the next command connects again. Otherwise like `to`
      */
    @inline final def dedicated(
        host: Host,
        port: Port,
        receiveBufferSizeBytes: Int = 8 * 1024 * 1024,
        connectTimeout: FiniteDuration = RedisChannel.DefaultConnectTimeout,
        credentials: Option[Credentials] = None,
        database: Option[DbIndex] = None,
        tls: Option[Tls[F]] = None
    ): Resource[F, DedicatedClient[F]] =
      connection(Seq(RedisAddress(host, port)), receiveBufferSizeBytes, connectTimeout, credentials, database, tls)
        .flatMap(impl.mkDedicated(_))

    private def connection(
        addresses: Seq[RedisAddress],
        receiveBufferSizeBytes: Int,
        connectTimeout: FiniteDuration,
        credentials: Option[Credentials],
        database: Option[DbIndex],
        tls: Option[Tls[F]]
    ): Resource[F, impl.Connection[F]] = {
      def redisChannel(address: RedisAddress, onConnected: F[Unit]): Pipe[F, RESP, RESP] =
        stream =>
          Stream.eval(address.toSocketAddress).flatMap { socketAddress =>
            val secure = (socket: _root_.fs2.io.net.Socket[F]) =>
              tls.fold(Resource.pure[F, _root_.fs2.io.net.Socket[F]](socket))(_.secure(socket, address.host.value))
            stream.through(RedisChannel(socketAddress, receiveBufferSizeBytes, connectTimeout, onConnected, secure))
          }

      impl.connection(redisChannel, addresses, impl.setup(credentials, database))
    }
  }

  private[laserdisc] object impl {
    // how far a connection got, each stage after the one before: it's never set back, as a send can be taken before it connects
    private[fs2] sealed abstract class Usage(private val stage: Int) {
      final def max(other: Usage): Usage = if (stage >= other.stage) this else other
    }
    private[fs2] object Usage {
      case object Connecting extends Usage(0)
      case object SettingUp  extends Usage(1)
      case object Idle       extends Usage(2)
      case object Used       extends Usage(3)
      case object Answered   extends Usage(4)
    }

    // how long a request that waits for as long as it takes is given: the watchdog never finds it overdue
    private[fs2] final val never: FiniteDuration = FiniteDuration(Long.MaxValue, NANOSECONDS)

    private[fs2] def dueBy(now: FiniteDuration, timeout: FiniteDuration): FiniteDuration = if (timeout == never) never else now + timeout

    // what ends a connection closed to stop a command waiting on the server, which isn't a failure of the connection
    private[fs2] object ConnectionReset
        extends RuntimeException("Connection closed to stop a command waiting on the server", null, true, false)

    sealed trait Connection[F[_]] {
      def run: F[Fiber[F, Throwable, Unit]]
      def shutdown: F[Unit]
      // sends a command that waits for as long as it takes: with no timeout, and closing the connection when cancelled once sent
      def sendForever[A](protocol: Protocol[A]): F[Maybe[A]]
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

    // a dedicated client takes one command at a time, so a command waiting on the server holds up only its own caller's next one
    def mkDedicated[F[_]: Concurrent](establishedConn: Connection[F]): Resource[F, DedicatedClient[F]] =
      Resource.eval(Mutex[F]).flatMap { mutex =>
        Resource.make(Ref.of[F, Boolean](false))(_.set(true)).map { released =>
          new DedicatedClient[F] {
            override final def send[In <: Tuple, Out <: Tuple](in: In, timeout: FiniteDuration)(
                implicit handler: Handler.Aux[F, Env[F], In, Out]
            ): F[Out] =
              released.get.ifM(Concurrent[F].raiseError(ClientTerminated), mutex.lock.surround(establishedConn.send(in, timeout)))

            override final def send[A](forever: Forever[A]): F[Maybe[A]] =
              released.get
                .ifM(Concurrent[F].raiseError(ClientTerminated), mutex.lock.surround(establishedConn.sendForever(forever.protocol)))
          }
        }
      }

    // what each connection sends before anything else: AUTH to log in, then SELECT for the database
    def setup(credentials: Option[Credentials], database: Option[DbIndex]): List[Protocol[RESP]] =
      credentials.toList.map { case Credentials(user, password) =>
        Protocol("AUTH", user.map(_.value).toList :+ password.value).using(RESPRead.raw)
      } ++ database.toList.map(index => Protocol("SELECT", index).using(RESPRead.raw))

    private final val setupTimeout = 30.seconds

    // a server loading its data, running a script, cut off from its master or with too many clients refuses for now only: the client
    // connects again, as to a server it can't reach, instead of stopping
    def refusedForNow(refused: Err): Boolean =
      refused.message == "ERR max number of clients reached" ||
        List("LOADING", "BUSY", "MASTERDOWN", "TRYAGAIN").exists(code => refused.message.startsWith(s"$code "))

    def connection[F[_]: Temporal](
        redisNetChannel: (RedisAddress, F[Unit]) => Pipe[F, RESP, RESP],
        addresses: Seq[RedisAddress],
        setup: List[Protocol[RESP]] = Nil
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
                Ref.of[F, Option[Throwable]](None),
                Ref.of[F, Option[(Deferred[F, Unit], Deferred[F, Unit])]](None)
              ).tupled
            )
            .flatMap { case (inFlight, unsentRequests, nextIndex, connections, watchedOn, retries, stopped, resetting) =>
              val log = logSelector.log

              def isTransaction(request: Request[F]): Boolean  = request.protocol.isInstanceOf[Protocol.Multi[?]]
              def command(request: Request[F]): Option[String] = request.protocol.frames.headOption.collect { case Arr(Bulk(name) :: _) =>
                name.toUpperCase
              }

              // replies come in order, so a request isn't due before those sent ahead of it, like a blocking command still waiting
              def dueAfter(in: Vector[InFlight[F]], due: FiniteDuration): FiniteDuration = in.lastOption.fold(due)(_.due max due)

              def sending(due: FiniteDuration, requests: List[Request[F]]): F[Chunk[RESP]] =
                if (requests.isEmpty) Chunk.empty[RESP].pure[F]
                else {
                  val frames = requests.map(_.protocol.frames)
                  inFlight.modify { in =>
                    val sent =
                      requests.zip(frames).map { case (request, fs) => InFlight(request, dueAfter(in, due), fs.size, Vector.empty) }
                    (in ++ sent) -> Chunk.from(frames.flatten)
                  }
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

                  val sendingMulti =
                    inFlight.modify(in => (in :+ InFlight(multi, dueAfter(in, due), 1, Vector.empty)) -> Chunk(frames.head))

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
                                .modify(in =>
                                  (in :+ InFlight(request, dueAfter(in, due), frames.size, Vector(Str("OK")))) -> Chunk.from(frames.tail)
                                )
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
                          .flatMap(now => sendAll(address, connection, dueBy(now, batch.timeout), sent, batch.requests.toList))
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

              def isSetup(request: Request[F]): Boolean = setup.exists(_ eq request.protocol)

              // a request is answered once it has a reply for each frame it sent. Taking it off the requests in flight and answering it
              // go together, as nothing else would answer it. Says whether the reply was to the connection's setup
              def receive(connection: Long)(resp: RESP): F[Boolean] =
                inFlight
                  .modify[Either[NoInFlightRequest, (Request[F], Option[Vector[RESP]])]] {
                    case head +: rest =>
                      val received = head.replies :+ resp
                      if (received.size < head.expected) (head.copy(replies = received) +: rest) -> Right(head.request -> None)
                      else rest                                                                  -> Right(head.request -> Some(received))
                    case empty => empty -> Left(NoInFlightRequest(resp))
                  }
                  .flatMap {
                    case Right((request, Some(replies))) =>
                      watch(connection, request, replies) >> request.callback(request.protocol.decodeReplies(replies)).as(isSetup(request))
                    case Right((request, None)) => isSetup(request).pure[F]
                    case Left(error)            => Concurrent[F].raiseError[Boolean](error)
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

              // a server refusing to set up a connection would refuse the next ones too, so the client stops, unless it refuses for now
              def stop(error: Throwable): F[Unit] = termSignal.complete(Left(error)).void

              // each connection logs in and selects the database before anything else, and sends nothing more until the server agrees, so
              // that no command runs as another user or in another database. Until then, the connection closing counts as a failure
              def settingUp(address: RedisAddress, usage: Ref[F, Usage]): Stream[F, RESP] =
                if (setup.isEmpty) Stream.empty
                else
                  Stream.eval(Deferred[F, Unit]).flatMap { ready =>
                    val last     = setup.size - 1
                    val requests = setup.zipWithIndex.map { case (protocol, i) =>
                      Request[F, RESP](
                        protocol,
                        {
                          case Right(Str("OK")) => (ready.complete(()) >> usage.update(_.max(Usage.Idle))).whenA(i == last)
                          case Right(other)     => stop(ConnectionSetupRefused(address, RESPDecErr(s"Expected OK but got: $other")))
                          case Left(refused: Err) if refusedForNow(refused) =>
                            Temporal[F].raiseError(ConnectionSetupRefused(address, refused))
                          case Left(refused: Err) => stop(ConnectionSetupRefused(address, refused))
                          // the connection went, and the next one is set up again
                          case Left(_) => Temporal[F].unit
                        }
                      )
                    }
                    Stream.evalUnChunk(Temporal[F].monotonic.flatMap(now => sending(now + setupTimeout, requests))) ++
                      Stream.exec(ready.get)
                  }

              def serverAvailable(address: RedisAddress, connection: Long, usage: Ref[F, Usage]): Stream[F, Unit] = {
                val again       = Stream.eval(retries.getAndSet(Chunk.empty)).filter(_.nonEmpty)
                val queued      = Stream.fromQueueUnterminated(queue, limit = 8 * 1024).chunks
                val connected   = if (setup.isEmpty) Usage.Idle else Usage.SettingUp
                val onConnected = log.info(s"Connected to server $address") >> usage.update(_.max(connected))
                (settingUp(address, usage) ++ (again ++ queued)
                  .evalTap(_ => usage.update(_.max(Usage.Used)))
                  .flatMap(taken => Stream.chunk(taken).flatMap(push(address, connection)).onFinalize(unclaimed(taken))))
                  .through(redisNetChannel(address, onConnected))
                  .evalMap(resp => receive(connection)(resp).flatMap(toSetup => usage.set(Usage.Answered).unlessA(toSetup)))
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
                  Stream
                    .eval(
                      (
                        nextAddress,
                        Ref.of[F, Usage](Usage.Connecting),
                        connections.updateAndGet(_ + 1),
                        Deferred[F, Unit],
                        Deferred[F, Unit]
                      ).tupled
                    )
                    .flatMap { case (address, usage, connection, reset, ended) =>
                      // a connection closed to stop a command waiting on the server isn't a failure: the next send connects again. ended
                      // is completed once what was sent on the connection has failed, so that nothing more is sent on it
                      val resettable = Stream.exec(resetting.set(Some(reset -> ended))) ++
                        serverAvailable(address, connection, usage).concurrently(
                          Stream.exec(reset.get) ++ Stream.raiseError[F](ConnectionReset)
                        )
                      resettable.handleErrorWith { failure =>
                        val idle = log.info(s"Connection to $address ended with nothing sent on it: ${failure.getMessage}") >> nextSend
                        Stream.eval(failInFlight(failure) >> ended.complete(()) >> usage.get).flatMap {
                          case _ if failure == ConnectionReset =>
                            Stream.exec(log.info(s"Connection to $address closed to stop a command waiting on it") >> nextSend) ++ runner(
                              failures
                            )
                          case Usage.Idle => Stream.exec(idle) ++ runner(failures)
                          case reached    =>
                            log.errorS(s"Failure of publishing connection to $address", failure) >>
                              runner(if (reached == Usage.Answered) 0 else failures + 1)
                        }
                      }
                    }

              // once the connection has ended, nothing takes from the queue, so a send queued after it fails at once, whether it came
              // before the queue was emptied or after, with the reason the client stopped
              val queueing: Batch[F] => F[Unit] =
                batch => queue.offer(batch) >> stopped.get.flatMap(_.traverse_(failQueued))

              val newConnection: Connection[F] = new Connection[F] {
                override final def run: F[Fiber[F, Throwable, Unit]] =
                  log.debug("Starting connection") >>
                    runner(0)
                      .interruptWhen(termSignal)
                      .compile
                      .drain
                      .attempt
                      .flatMap { r =>
                        val reason = r.fold(identity, _ => ClientTerminated)
                        log.info(
                          s"Connection terminated: ${r.fold(err => err.getMessage, _ => "No issues")}"
                        ) >> stopped.set(Some(reason)) >> failInFlight(reason) >> failQueued(reason)
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

                // cancelled once the connection has sent it, the command is still waiting on the server, holding the connection, so the
                // connection is closed, unless the reply came in the meantime. Cancelling ends once it's closed, so that the next command
                // doesn't go on it
                override final def sendForever[A](protocol: Protocol[A]): F[Maybe[A]] =
                  (Deferred[F, Maybe[A]], Deferred[F, Unit]).flatMapN { (reply, claim) =>
                    val request = Request[F, A](protocol, reply.complete(_).void)
                    (queueing(Batch(Vector(request), claim, never)) >> reply.get).onCancel {
                      claim.complete(()).flatMap { unsent =>
                        (reply.tryGet, resetting.get)
                          .flatMapN { (replied, current) =>
                            current.traverse_ { case (reset, ended) => reset.complete(()) >> ended.get }.whenA(replied.isEmpty)
                          }
                          .unlessA(unsent)
                      }
                    }
                  }
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
