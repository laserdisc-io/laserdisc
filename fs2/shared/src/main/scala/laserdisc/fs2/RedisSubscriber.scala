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

import _root_.fs2.Stream
import _root_.fs2.io.net.{Network, Socket}
import cats.effect.std.Queue as CatsQueue
import cats.effect.syntax.all.*
import cats.effect.{Async, Deferred, Ref, Resource}
import cats.syntax.all.*
import laserdisc.protocol.{Err, Feed, RESP}
import laserdisc.protocol.Feed.Unreadable
import log.effect.fs2.LogSelector

import scala.concurrent.duration.*

/** Opens a connection of its own for a [[protocol.Feed]], like `subscribe` or `monitor` of a version's import. Once the resource is open
  * the server has confirmed it, so what's published from then on arrives. Closing it closes the connection, and with it the subscription.
  * Each message comes as a `Right`, or as a `Left` of [[protocol.Feed.Unreadable]] when it can't be read as asked, and the stream goes on.
  * The stream fails if the connection drops: messages published in the meantime are lost, so whether to open another one is up to you. It
  * also fails if the server says nothing for 30 seconds after being pinged, except for `monitor`, whose connection isn't pinged, and if the
  * server replies with an error
  */
object RedisSubscriber {
  @inline final def apply[F[_]: Async: Network: LogSelector]: RedisSubscriberConnector[F] = new RedisSubscriberConnector[F](30.seconds)

  private final val receiveBufferSizeBytes = 8 * 1024 * 1024
  private final val confirmationTimeout    = 30.seconds
  private final val queueSize              = 1024

  /** What `RedisSubscriber[F]` gives, to open a subscription with `to` or `identified` */
  // keepAlive: how often a subscribed connection is pinged, and how long the server has to say something before it's taken for gone
  final class RedisSubscriberConnector[F[_]: Async: Network: LogSelector] private[fs2] (
      keepAlive: FiniteDuration,
      tls: Option[Tls[F]] = None
  ) {

    /** Opens each connection with TLS, checking the server's certificate */
    final def withTls(tls: Tls[F]): RedisSubscriberConnector[F] = new RedisSubscriberConnector[F](keepAlive, Some(tls))

    final def to[A](host: Host, port: Port, feed: Feed[A]): Resource[F, Stream[F, Either[Unreadable, A]]] =
      open(RedisAddress(host, port), Nil, feed).map(_._2)

    /** Logs in first, as a new connection isn't logged in */
    final def to[A](host: Host, port: Port, credentials: Credentials, feed: Feed[A]): Resource[F, Stream[F, Either[Unreadable, A]]] =
      open(RedisAddress(host, port), List(auth(credentials)), feed).map(_._2)

    /** Like `to`, also giving the connection's id, which CLIENT TRACKING's REDIRECT asks for */
    final def identified[A](host: Host, port: Port, feed: Feed[A]): Resource[F, (NonNegLong, Stream[F, Either[Unreadable, A]])] =
      withId(RedisAddress(host, port), Nil, feed)
    final def identified[A](
        host: Host,
        port: Port,
        credentials: Credentials,
        feed: Feed[A]
    ): Resource[F, (NonNegLong, Stream[F, Either[Unreadable, A]])] =
      withId(RedisAddress(host, port), List(auth(credentials)), feed)

    private def secure(address: RedisAddress)(socket: Socket[F]): Resource[F, Socket[F]] =
      tls.fold(Resource.pure[F, Socket[F]](socket))(_.secure(socket, address.host.value))

    private def auth(credentials: Credentials): RESP =
      protocol.Arr(protocol.Bulk("AUTH") :: (credentials.username.toList :+ credentials.password).map(key => protocol.Bulk(key.value)))

    private def withId[A](
        address: RedisAddress,
        before: List[RESP],
        feed: Feed[A]
    ): Resource[F, (NonNegLong, Stream[F, Either[Unreadable, A]])] =
      open(address, before :+ protocol.Arr(protocol.Bulk("CLIENT"), protocol.Bulk("ID")), feed).evalMap { case (answers, stream) =>
        answers.lastOption match {
          case Some(protocol.Num(NonNegLong(id))) => (id -> stream).pure[F]
          case other                              =>
            Async[F].raiseError[(NonNegLong, Stream[F, Either[Unreadable, A]])](
              protocol.RESPDecErr(s"Expected the connection's id but got: $other")
            )
        }
      }

    private def open[A](
        address: RedisAddress,
        before: List[RESP],
        feed: Feed[A]
    ): Resource[F, (Vector[RESP], Stream[F, Either[Unreadable, A]])] =
      for {
        socketAddress <- Resource.eval(address.toSocketAddress[F])
        socket        <- RedisChannel.connectedSocket(socketAddress, receiveBufferSizeBytes, secure = secure(address))
        replies       <- Resource.eval(CatsQueue.bounded[F, Either[Throwable, RESP]](queueSize))
        heard         <- Resource.eval(Ref.of[F, Boolean](true))
        blocked       <- Resource.eval(Ref.of[F, Boolean](false))
        // a full queue holds up reading, so while it's full the server isn't expected to be heard
        _ <- (socket.reads.through(RedisChannel.impl.receiveResp).attempt ++ Stream.emit(Left(ServerTerminatedConnection(address))))
          .evalMap {
            case Right(reply) if feed.keptAlive && Feed.isPong(reply) => heard.set(true)
            case reply => heard.set(true) >> (blocked.set(true) >> replies.offer(reply)).guarantee(blocked.set(false))
          }
          .compile
          .drain
          .background
        write = (commands: List[RESP]) => Stream.emits(commands).through(RedisChannel.impl.send(socket.write)).compile.drain
        _         <- Resource.eval(write(before :+ feed.command))
        confirmed <- Resource.eval(
          confirm(address, replies, before.size, feed.confirmations, feed, Vector.empty, Vector.empty)
            .timeoutTo(
              confirmationTimeout,
              Async[F].raiseError[(Vector[RESP], Vector[Either[Unreadable, A]])](ServerNotResponding(address))
            )
        )
        // nothing heard for a whole round, with nothing holding up reading, means the server is gone, even if the connection isn't closed
        pinging = (heard.set(false) >> write(List(Feed.ping)) >> Async[F].sleep(keepAlive) >> (heard.get, blocked.get).tupled)
          .iterateWhile { case (wasHeard, wasBlocked) =>
            wasHeard || wasBlocked
          } >> replies.offer(Left(ServerNotResponding(address)))
        _     <- (if (feed.keptAlive) pinging else Async[F].unit).background
        early <- Resource.eval(Ref.of[F, Vector[Either[Unreadable, A]]](confirmed._2))
        ended <- Resource.eval(Deferred[F, Throwable])
        // closing the subscriber ends every run, those waiting for a message included, as nothing more will come. It comes first among
        // what's closed, so a run sees it before the connection's end
        _ <- Resource.onFinalize(ended.complete(ClientTerminated).void)
        // the stream may be run more than once: what came during confirmation goes to the first run only, and each message is taken from
        // the queue on its own, so that a run stopping early leaves the next ones there. The connection's failure comes after the messages
        // before it; the run that takes it ends the others, and every run after them fails with it, as nothing more will come
        next = Async[F].race(ended.get, replies.take).flatMap {
          case Right(Left(failure)) => ended.complete(failure) >> ended.get.flatMap(Async[F].raiseError[RESP])
          case Right(Right(reply))  => reply.pure[F]
          case Left(failure)        => Async[F].raiseError[RESP](failure)
        }
      } yield confirmed._1 -> Stream.eval(ended.tryGet).flatMap {
        case Some(failure) => Stream.raiseError[F](failure)
        case None          =>
          Stream.eval(early.getAndSet(Vector.empty)).flatMap(Stream.emits) ++
            Stream.repeatEval(next).flatMap(reply => Stream.fromEither[F](feed.received(reply)).flatMap(Stream.fromOption[F](_)))
      }

    // what arrives while the rest is being confirmed is kept for the stream. An error to what's sent before the feed's command, as to
    // AUTH, means the server refused to set up the connection
    private def confirm[A](
        address: RedisAddress,
        replies: CatsQueue[F, Either[Throwable, RESP]],
        before: Int,
        pending: Set[String],
        feed: Feed[A],
        answers: Vector[RESP],
        early: Vector[Either[Unreadable, A]]
    ): F[(Vector[RESP], Vector[Either[Unreadable, A]])] =
      if (before == 0 && pending.isEmpty) (answers -> early).pure[F]
      else
        replies.take.flatMap {
          case Left(failure)                   => Async[F].raiseError[(Vector[RESP], Vector[Either[Unreadable, A]])](failure)
          case Right(error: Err) if before > 0 =>
            Async[F].raiseError[(Vector[RESP], Vector[Either[Unreadable, A]])](ConnectionSetupRefused(address, error))
          case Right(error: Err)          => Async[F].raiseError[(Vector[RESP], Vector[Either[Unreadable, A]])](error)
          case Right(reply) if before > 0 => confirm(address, replies, before - 1, pending, feed, answers :+ reply, early)
          case Right(reply)               =>
            feed.confirmed(reply) match {
              case Some(name) => confirm(address, replies, before, pending - name, feed, answers, early)
              case None       =>
                Async[F]
                  .fromEither(feed.received(reply))
                  .flatMap(a => confirm(address, replies, before, pending, feed, answers, early ++ a))
            }
        }
  }
}
