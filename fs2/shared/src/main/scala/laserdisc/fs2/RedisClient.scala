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

import _root_.fs2.{Chunk, CompositeFailure, Pipe, Stream}
import _root_.fs2.io.net.Network
import cats.data.NonEmptySet
import cats.effect.kernel.{Deferred, Ref, Temporal}
import cats.effect.syntax.all.*
import cats.effect.std.{Mutex, Queue, Random}
import cats.effect.{Async, Concurrent, Fiber, Resource}
import cats.syntax.all.*
import laserdisc.protocol.RESPRead
import log.effect.fs2.LogSelector
import log.effect.fs2.syntax.*

import java.io.IOException
import scala.concurrent.duration.*

object RedisClient {
  @inline final def apply[F[_]: Async: Network: LogSelector]: RedisClientConnector[F] =
    new RedisClientConnector[F]()

  /** What `RedisClient[F]` gives, to open a client with `to`, `toNodes` or `dedicated` */
  final class RedisClientConnector[F[_]: Async: Network: LogSelector] private[fs2] () {

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
        tls: Option[Tls[F]] = None,
        reconnect: Reconnect = Reconnect.default
    ): Resource[F, RedisClient[F]] =
      toNodes(NonEmptySet.one(RedisAddress(host, port)), receiveBufferSizeBytes, connectTimeout, credentials, database, tls, reconnect)

    /** Like `to`, trying each address in turn, in the set's order, with the same credentials, database and TLS for each */
    @inline final def toNodes(
        addresses: NonEmptySet[RedisAddress],
        receiveBufferSizeBytes: Int = 8 * 1024 * 1024,
        connectTimeout: FiniteDuration = RedisChannel.DefaultConnectTimeout,
        credentials: Option[Credentials] = None,
        database: Option[DbIndex] = None,
        tls: Option[Tls[F]] = None,
        reconnect: Reconnect = Reconnect.default
    ): Resource[F, RedisClient[F]] =
      connection(addresses.toSortedSet.toList, receiveBufferSizeBytes, connectTimeout, credentials, database, tls, reconnect)
        .flatMap(impl.mkClient(_))

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
        tls: Option[Tls[F]] = None,
        reconnect: Reconnect = Reconnect.default
    ): Resource[F, DedicatedClient[F]] =
      connection(Seq(RedisAddress(host, port)), receiveBufferSizeBytes, connectTimeout, credentials, database, tls, reconnect)
        .flatMap(impl.mkDedicated(_))

    private def connection(
        addresses: Seq[RedisAddress],
        receiveBufferSizeBytes: Int,
        connectTimeout: FiniteDuration,
        credentials: Option[Credentials],
        database: Option[DbIndex],
        tls: Option[Tls[F]],
        reconnect: Reconnect
    ): Resource[F, impl.Connection[F]] = {
      def redisChannel(address: RedisAddress, onConnected: F[Unit]): Pipe[F, RESP, RESP] =
        stream =>
          Stream.eval(address.toSocketAddress).flatMap { socketAddress =>
            val secure = (socket: _root_.fs2.io.net.Socket[F]) =>
              tls.fold(Resource.pure[F, _root_.fs2.io.net.Socket[F]](socket))(_.secure(socket, address.host.value))
            stream.through(RedisChannel(socketAddress, receiveBufferSizeBytes, connectTimeout, onConnected, secure))
          }

      impl.connection(redisChannel, addresses, impl.setup(credentials, database), reconnect)
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

    // a duration added to another, as long as never at most: FiniteDuration fails on a sum past it, as a timeout that long can make
    private[fs2] def upToNever(a: FiniteDuration, b: FiniteDuration): FiniteDuration = if (b >= never - a) never else a + b

    private[fs2] def dueBy(now: FiniteDuration, timeout: FiniteDuration): FiniteDuration = upToNever(now, timeout)

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

    // the MULTI that begins a transaction sent with Protocol.multi, told apart from one typed as a plain command
    private final val transactionMulti: Protocol[RESP] = Protocol("MULTI", EmptyTuple).using(RESPRead.raw)

    // the connection a transaction typed as plain commands is on when it was lost before it began, for want of its watch: none is
    private final val LostBeforeItBegan = -1L

    // what a connection was set to by a command the server agreed to, which each new connection is set to again after its setup: its name,
    // reading from a replica and tracking, each as it was last set. One set back to how a connection starts isn't kept
    private[fs2] final case class Kept(name: Option[Protocol[RESP]], readonly: Option[Protocol[RESP]], tracking: Option[Protocol[RESP]]) {
      def all: List[Protocol[RESP]] = name.toList ++ readonly ++ tracking

      def without(protocol: Protocol[RESP]): Kept =
        Kept(name.filterNot(_ eq protocol), readonly.filterNot(_ eq protocol), tracking.filterNot(_ eq protocol))
    }
    private[fs2] object Kept {
      final val none: Kept = Kept(None, None, None)
    }

    // a server loading its data, running a script, cut off from its master or with too many clients refuses for now only: the client
    // connects again, as to a server it can't reach, instead of stopping
    private final val MaxClients = "ERR max number of clients reached"

    // a connection in use that the server closed or reset fails with an error of the socket's, such as a broken pipe, or with one on
    // each side at once: what was waiting on it fails with ServerTerminatedConnection, with that error as its cause
    private[fs2] def terminated(address: RedisAddress)(failure: Throwable): Throwable =
      failure match {
        case socket: IOException                                                    => ServerTerminatedConnection(address, Some(socket))
        case both: CompositeFailure if both.all.forall(_.isInstanceOf[IOException]) => ServerTerminatedConnection(address, Some(both))
        case other                                                                  => other
      }

    def refusedForNow(refused: Err): Boolean =
      refused.message == MaxClients ||
        List("LOADING", "BUSY", "MASTERDOWN", "TRYAGAIN").exists(code => refused.message.startsWith(s"$code "))

    def connection[F[_]: Async](
        redisNetChannel: (RedisAddress, F[Unit]) => Pipe[F, RESP, RESP],
        addresses: Seq[RedisAddress],
        setup: List[Protocol[RESP]] = Nil,
        reconnect: Reconnect = Reconnect.default
    )(implicit logSelector: LogSelector[F]): Resource[F, Connection[F]] =
      Resource.eval((Deferred[F, Either[Throwable, Unit]], Random.scalaUtilRandom[F]).tupled).flatMap { case (termSignal, random) =>
        Resource.eval(Queue.bounded[F, Batch[F]](1024)).flatMap { queue =>
          // the requests sent and waiting for their replies, those of a batch the connection went before sending, the address to try
          // next, the number of the current connection, the number of the connection a WATCH last succeeded on, until a transaction or
          // UNWATCH ends it, the batches to send again, what each new connection is set to again and what the current one was, and the
          // connection a MULTI typed as a plain command was sent on, until EXEC or DISCARD ends its transaction
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
                Ref.of[F, Option[(Deferred[F, Unit], Deferred[F, Unit])]](None),
                Ref.of[F, Kept](Kept.none),
                Ref.of[F, List[Protocol[RESP]]](Nil),
                Ref.of[F, Option[Long]](None)
              ).tupled
            )
            .flatMap {
              case (inFlight, unsentRequests, nextIndex, connections, watchedOn, retries, stopped, resetting, kept, setAgain, typedMulti) =>
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
                      transactionMulti,
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
                          case Right(other) => answered >> Concurrent[F].raiseError(WatchCleanupRefused(address, command, other.toString))
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
                  Stream.evalUnChunk(
                    (guarded(address, connection, before).flatMap(sending(due, _)) <* sent.update(_ + before.size)).uncancelable
                  ) ++ (rest match {
                    case request :: after =>
                      transaction(address, connection, due, sent, request) ++ sendAll(address, connection, due, sent, after)
                    case Nil => Stream.empty
                  })
                }

                // a transaction typed as plain commands, as at the CLI, is on the connection its MULTI was sent on: once that one's gone, the
                // server has dropped it, so what's typed until EXEC or DISCARD ends it fails without being sent, instead of running outside
                // it. A MULTI typed after a WATCH on a connection that's gone fails too, as its transaction would run without the watch
                def guarded(address: RedisAddress, connection: Long, requests: List[Request[F]]): F[List[Request[F]]] =
                  requests.traverseFilter { request =>
                    (typedMulti.get, watchedOn.get).flatMapN { (multi, watched) =>
                      val name = command(request)
                      multi match {
                        case Some(on) if on != connection =>
                          val ends = name.exists(n => n == "EXEC" || n == "DISCARD")
                          (typedMulti.set(None) >> watchedOn.set(None)).whenA(ends) >>
                            request.callback(Left(TransactionLost(address))).as(Option.empty[Request[F]])
                        case _ if name.contains("MULTI") && watched.exists(_ != connection) =>
                          watchedOn.set(None) >> typedMulti.set(Some(LostBeforeItBegan)) >>
                            request.callback(Left(WatchLost(address))).as(Option.empty[Request[F]])
                        case _ => Concurrent[F].pure(Option(request))
                      }
                    }
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
                  if (isTransaction(request)) watchedOn.set(None) >> typedMulti.set(None)
                  else
                    (command(request), replies.headOption) match {
                      case (Some("MULTI"), Some(Str("OK"))) if !(request.protocol eq transactionMulti) => typedMulti.set(Some(connection))
                      case (Some("WATCH"), Some(Str("OK")))                              => watchedOn.update(_.orElse(Some(connection)))
                      case (Some("UNWATCH"), Some(Str("OK")))                            => watchedOn.set(None)
                      case (Some("DISCARD"), Some(Str("OK")))                            => watchedOn.set(None) >> typedMulti.set(None)
                      case (Some("EXEC"), Some(Err(m))) if m.startsWith("ERR")           => typedMulti.set(None)
                      case (Some("EXEC"), Some(_)) | (Some("RESET"), Some(Str("RESET"))) => watchedOn.set(None) >> typedMulti.set(None)
                      case _                                                             => Concurrent[F].unit
                    }

                // what a CLIENT TRACKING, READONLY, READWRITE, CLIENT SETNAME and HELLO naming the connection set it to, once the server
                // has agreed, which a new connection is set to again. HELLO's name is set again with CLIENT SETNAME
                def keep(request: Request[F], replies: Vector[RESP]): F[Unit] =
                  if (isTransaction(request)) Concurrent[F].unit
                  else {
                    val words                         = wordsOf(request.protocol)
                    val upper                         = words.map(_.toUpperCase)
                    def again(ws: List[String])       = Some(Protocol(ws.head, ws.tail).using(RESPRead.raw))
                    def set(f: Kept => Kept): F[Unit] = kept.update(f)
                    (upper.take(3), replies.headOption) match {
                      case ("CLIENT" :: "TRACKING" :: "ON" :: Nil, Some(Str("OK")))  => set(_.copy(tracking = again(words)))
                      case ("CLIENT" :: "TRACKING" :: "OFF" :: Nil, Some(Str("OK"))) => set(_.copy(tracking = None))
                      case ("READONLY" :: Nil, Some(Str("OK")))                      => set(_.copy(readonly = again(words)))
                      case ("READWRITE" :: Nil, Some(Str("OK")))                     => set(_.copy(readonly = None))
                      case ("CLIENT" :: "SETNAME" :: _, Some(Str("OK")))             => set(_.copy(name = again(words)))
                      case ("HELLO" :: _, Some(Arr(_)))                              =>
                        val at = upper.indexOf("SETNAME")
                        if (at >= 0 && at + 1 < words.size) set(_.copy(name = again(List("CLIENT", "SETNAME", words(at + 1)))))
                        else Concurrent[F].unit
                      case _ => Concurrent[F].unit
                    }
                  }

                def wordsOf(protocol: Protocol[?]): List[String] =
                  protocol.frames.headOption.toList.flatMap {
                    case Arr(elements) => elements.collect { case Bulk(word) => word }
                    case _             => Nil
                  }

                def isSetup(request: Request[F]): F[Boolean] =
                  setAgain.get.map(again => setup.exists(_ eq request.protocol) || again.exists(_ eq request.protocol))

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
                      // a reader that throws, as one of the caller's own can, fails only its own request
                      case Right((request, Some(replies))) =>
                        val decoded = Either.catchNonFatal(request.protocol.decodeReplies(replies)).flatten
                        isSetup(request).flatTap { toSetup =>
                          watch(connection, request, replies) >> keep(request, replies).unlessA(toSetup) >> request.callback(decoded)
                        }
                      case Right((request, None)) => isSetup(request)
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

                def giveBack(current: Ref[F, Chunk[Batch[F]]]): F[Unit] = current.getAndSet(Chunk.empty).flatMap(unclaimed).uncancelable

                // a server refusing to set up a connection would refuse the next ones too, so the client stops, unless it refuses for now
                def stop(error: Throwable): F[Unit] = termSignal.complete(Left(error)).void

                // each connection logs in and selects the database before anything else, and sends nothing more until the server has agreed
                // to all of it, so that no command runs as another user or in another database. Until then, the connection closing counts as
                // a failure
                def settingUp(address: RedisAddress, usage: Ref[F, Usage], again: List[Protocol[RESP]]): Stream[F, RESP] =
                  if (setup.isEmpty && again.isEmpty) Stream.empty
                  else
                    Stream.eval((Deferred[F, Unit], Ref.of[F, Int](0)).tupled).flatMap { case (ready, agreed) =>
                      val done = agreed.updateAndGet(_ + 1).flatMap { n =>
                        (ready.complete(()) >> usage.update(_.max(Usage.Idle))).whenA(n == setup.size + again.size)
                      }
                      val requests = setup.map { protocol =>
                        Request[F, RESP](
                          protocol,
                          {
                            case Right(Str("OK")) => done
                            case Right(other)     => stop(ConnectionSetupRefused(address, RESPDecErr(s"Expected OK but got: $other")))
                            case Left(refused: Err) if refusedForNow(refused) =>
                              Temporal[F].raiseError(ConnectionSetupRefused(address, refused))
                            case Left(refused: Err) => stop(ConnectionSetupRefused(address, refused))
                            // the connection went, and the next one is set up again
                            case Left(_) => Temporal[F].unit
                          }
                        )
                      }
                      // what the connection is set to again isn't kept when the server refuses it, as when the connection tracking sends
                      // its notices to has gone: the connection carries on without it
                      def dropped(protocol: Protocol[RESP], reason: String): F[Unit] =
                        log.error(
                          s"Connection to $address refused to be set again with ${wordsOf(protocol).mkString(" ")}, which isn't sent again: $reason"
                        ) >>
                          kept.update(_.without(protocol)) >> done
                      val settingAgain = again.map { protocol =>
                        Request[F, RESP](
                          protocol,
                          {
                            case Right(Str("OK"))                             => done
                            case Right(other)                                 => dropped(protocol, s"Expected OK but got: $other")
                            case Left(refused: Err) if refusedForNow(refused) =>
                              Temporal[F].raiseError(ConnectionSetupRefused(address, refused))
                            case Left(refused: Err) => dropped(protocol, refused.message)
                            case Left(_)            => Temporal[F].unit
                          }
                        )
                      }
                      Stream.evalUnChunk(Temporal[F].monotonic.flatMap(now => sending(now + setupTimeout, requests ++ settingAgain))) ++
                        Stream.exec(ready.get)
                    }

                // the batches are taken all at once, as many as are queued, which the queue's size bounds. They're kept in current from the
                // moment they're taken, in the same step, so that a connection that goes while they're on their way to being sent leaves them
                // to be sent again: the chunk's finalizer gives them back, or if the connection went before there was one, the end of the
                // connection does. Whichever comes first takes them
                def serverAvailable(
                    address: RedisAddress,
                    connection: Long,
                    usage: Ref[F, Usage],
                    current: Ref[F, Chunk[Batch[F]]]
                ): Stream[F, Unit] = {
                  val again  = Stream.eval(retries.getAndSet(Chunk.empty).flatTap(current.set).uncancelable).filter(_.nonEmpty)
                  val queued = Stream.repeatEval(Concurrent[F].uncancelable { poll =>
                    poll(queue.take)
                      .flatMap(first => queue.tryTakeN(None).map(rest => Chunk.from(first :: rest)))
                      .flatTap(current.set)
                  })
                  Stream.eval(kept.get.map(_.all).flatTap(setAgain.set)).flatMap { setTo =>
                    val connected   = if (setup.isEmpty && setTo.isEmpty) Usage.Idle else Usage.SettingUp
                    val onConnected = log.info(s"Connected to server $address") >> usage.update(_.max(connected))
                    (settingUp(address, usage, setTo) ++ (again ++ queued)
                      .evalTap(_ => usage.update(_.max(Usage.Used)))
                      .flatMap(taken => Stream.chunk(taken).flatMap(push(address, connection)).onFinalize(giveBack(current))))
                      .through(redisNetChannel(address, onConnected))
                      .evalMap {
                        // a server with too many clients says so to the first command, without running it, and closes the connection: that's
                        // a connection refused for now, as when it says so to the setup, not one that answered
                        case full: Err if full.message == MaxClients =>
                          usage.get.flatMap { reached =>
                            if (reached == Usage.Answered) receive(connection)(full).void
                            else Temporal[F].raiseError[Unit](ConnectionSetupRefused(address, full))
                          }
                        case resp => receive(connection)(resp).flatMap(toSetup => usage.set(Usage.Answered).unlessA(toSetup))
                      }
                      .concurrently(watchdog(address))
                  } ++ Stream.raiseError(ServerTerminatedConnection(address))
                }

                // the batches are taken one at a time, each failed in the same step, so that stopping once the wait is over leaves the next
                // ones queued. Each fails with why the last connection failed
                def serverUnavailable(cause: Throwable, wait: FiniteDuration): Stream[F, Unit] = {
                  val unavailable = ServerUnavailable(cause)
                  val again       = retries.getAndSet(Chunk.empty).flatMap(_.traverse_(failBatch(unavailable))).uncancelable
                  val queued      = Concurrent[F].uncancelable(poll => poll(queue.take).flatMap(failBatch(unavailable)))
                  log.errorS("Server unavailable for publishing") >>
                    (Stream.exec(again) ++ Stream.repeatEval(queued).drain).interruptAfter(wait)
                }

                // a connection that closed with nothing sent on it, as a server closes idle clients, is opened again only once there's
                // something to send: the send waiting for it goes first on the new connection. What it was given to send and didn't is
                // something to send, so then it's opened again at once
                val nextSend: F[Unit] =
                  Concurrent[F].uncancelable { poll =>
                    retries.get.flatMap { again =>
                      if (again.nonEmpty) Concurrent[F].unit
                      else poll(queue.take).flatMap(batch => retries.update(Chunk.singleton(batch) ++ _))
                    }
                  }

                // failures counts the addresses that couldn't be reached, or that closed the connection before answering what was sent on
                // it, since the last connection that answered. When that one goes with commands waiting on it, every address is tried again,
                // its own last, before the client gives up: a server that answered may only have dropped the connection, and one that closes each connection at once,
                // as when it has too many clients, isn't tried again and again. last is why the last connection failed, and waits how many
                // times in a row the client has waited for want of a server since one last answered, which makes each wait longer. again is
                // the address to connect to next, when the last connection's server is still fine: it closed a connection nothing was waiting
                // on, as a server closes idle clients, or the client closed it to stop a command
                def runner(failures: Int, last: Option[Throwable], waits: Int, again: Option[RedisAddress] = None): Stream[F, Unit] =
                  if (failures >= addresses.size) {
                    val cause = last.getOrElse(new NoSuchElementException("No address to connect to"))
                    val wait  = random.betweenDouble(0.5d, 1d).map(cut => (reconnect.after(waits).toNanos * cut).toLong.nanos)
                    Stream.eval(wait).flatMap(serverUnavailable(cause, _)) ++ runner(0, last, waits + 1)
                  } else
                    Stream
                      .eval(
                        (
                          again.fold(nextAddress)(Concurrent[F].pure),
                          Ref.of[F, Usage](Usage.Connecting),
                          connections.updateAndGet(_ + 1),
                          Deferred[F, Unit],
                          Deferred[F, Unit],
                          Ref.of[F, Chunk[Batch[F]]](Chunk.empty)
                        ).tupled
                      )
                      .flatMap { case (address, usage, connection, reset, ended, current) =>
                        // a connection closed to stop a command waiting on the server isn't a failure: the next send connects again. ended
                        // is completed once what was sent on the connection has failed, so that nothing more is sent on it. What it took and
                        // didn't send is given back however it ends, also when the client is closed, which interrupts it without an error
                        val resettable = (Stream.exec(resetting.set(Some(reset -> ended))) ++
                          serverAvailable(address, connection, usage, current).concurrently(
                            Stream.exec(reset.get) ++ Stream.raiseError[F](ConnectionReset)
                          )).onFinalize(giveBack(current))
                        resettable.handleErrorWith { error =>
                          // a socket's error once connected means the server closed or reset the connection; before, it says why connecting
                          // failed, as it's refused or the TLS handshake fails, and is kept as it is
                          Stream
                            .eval(usage.get.map(reached => if (reached == Usage.Connecting) error else terminated(address)(error)))
                            .flatMap { failure =>
                              val idle =
                                log.info(s"Connection to $address ended with nothing waiting on it: ${failure.getMessage}") >> nextSend
                              val waiting = (inFlight.get, unsentRequests.get).mapN(_.nonEmpty || _.nonEmpty)
                              Stream
                                .eval(
                                  waiting
                                    .flatMap(w =>
                                      giveBack(current) >> failInFlight(failure) >> ended.complete(()) >> usage.get.tupleRight(w)
                                    )
                                )
                                .flatMap {
                                  case _ if failure == ConnectionReset =>
                                    Stream.exec(
                                      log.info(s"Connection to $address closed to stop a command waiting on it") >> nextSend
                                    ) ++ runner(failures, last, waits, Some(address))
                                  case (Usage.Idle, _)         => Stream.exec(idle) ++ runner(failures, last, waits, Some(address))
                                  case (Usage.Answered, false) => Stream.exec(idle) ++ runner(0, last, 0, Some(address))
                                  case (reached, _)            =>
                                    log.errorS(s"Failure of publishing connection to $address", failure) >>
                                      (if (reached == Usage.Answered) runner(0, Some(failure), 0)
                                       else runner(failures + 1, Some(failure), waits))
                                }
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
                      runner(0, None, 0)
                        .interruptWhen(termSignal)
                        .compile
                        .drain
                        .attempt
                        .flatMap { r =>
                          // what the last connection had in flight has failed, so a command cancelled while waiting on it can stop
                          val endLast = resetting.get.flatMap(_.traverse_ { case (_, ended) => ended.complete(()).void })
                          val reason  = r.fold(identity, _ => ClientTerminated)
                          log.info(
                            s"Connection terminated: ${r.fold(err => err.getMessage, _ => "No issues")}"
                          ) >> stopped.set(Some(reason)) >> failInFlight(reason) >> endLast >> failQueued(reason)
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
                        // the connection is read before the reply: a connection fails what's waiting on it before the next one takes its
                        // place, so if it's already the next one, the reply has come, and that one isn't closed for nothing
                        claim.complete(()).flatMap { unsent =>
                          (resetting.get, reply.tryGet)
                            .flatMapN { (current, replied) =>
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
