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

import cats.effect.std.{Queue => CatsQueue}
import cats.effect.syntax.all.*
import cats.effect.{Async, Resource}
import cats.syntax.all.*
import laserdisc.protocol.{Err, Feed, RESP}
import log.effect.fs2.LogSelector

import scala.concurrent.duration.*

/** Opens a connection of its own for a [[protocol.Feed]], like `subscribe` or `monitor` of a version's import. Once the resource is open
  * the server has confirmed it, so what's published from then on arrives. Closing it closes the connection, and with it the subscription.
  * The stream fails if the connection drops: messages published in the meantime are lost, so whether to open another one is up to you
  */
object RedisSubscriber {
  @inline final def apply[F[_]: Async: Network: LogSelector]: RedisSubscriberConnector[F] = new RedisSubscriberConnector[F]()

  private final val receiveBufferSizeBytes = 8 * 1024 * 1024
  private final val confirmationTimeout    = 30.seconds
  private final val queueSize              = 1024

  private[fs2] final class RedisSubscriberConnector[F[_]: Async: Network: LogSelector]() {

    final def to[A](host: Host, port: Port, feed: Feed[A]): Resource[F, Stream[F, A]] = open(RedisAddress(host, port), Nil, feed)

    /** Logs in first, as a new connection isn't logged in */
    final def to[A](host: Host, port: Port, password: Key, feed: Feed[A]): Resource[F, Stream[F, A]] =
      open(RedisAddress(host, port), List(protocol.Arr(protocol.Bulk("AUTH"), protocol.Bulk(password.value))), feed)

    private def open[A](address: RedisAddress, before: List[RESP], feed: Feed[A]): Resource[F, Stream[F, A]] =
      for {
        socketAddress <- Resource.eval(address.toSocketAddress[F])
        socket        <- RedisChannel.connectedSocket(socketAddress, receiveBufferSizeBytes)
        replies       <- Resource.eval(CatsQueue.bounded[F, Either[Throwable, RESP]](queueSize))
        _             <- (socket.reads.through(RedisChannel.impl.receiveResp).attempt ++
          Stream.emit(Left(ServerTerminatedConnection(address)))).evalMap(replies.offer).compile.drain.background
        _     <- Resource.eval(Stream.emits(before :+ feed.command).through(RedisChannel.impl.send(socket.write)).compile.drain)
        early <- Resource.eval(
          confirm(replies, before.size, feed.confirmations, feed, Vector.empty)
            .timeoutTo(confirmationTimeout, Async[F].raiseError[Vector[A]](ServerNotResponding(address)))
        )
      } yield Stream.emits(early) ++ Stream.fromQueueUnterminated(replies).flatMap {
        case Left(failure) => Stream.raiseError[F](failure)
        case Right(reply)  => Stream.fromEither[F](feed.message(reply)).flatMap(Stream.fromOption[F](_))
      }

    // what arrives while the rest is being confirmed is kept for the stream
    private def confirm[A](
        replies: CatsQueue[F, Either[Throwable, RESP]],
        before: Int,
        pending: Set[String],
        feed: Feed[A],
        early: Vector[A]
    ): F[Vector[A]] =
      if (before == 0 && pending.isEmpty) early.pure[F]
      else
        replies.take.flatMap {
          case Left(failure)          => Async[F].raiseError[Vector[A]](failure)
          case Right(error: Err)      => Async[F].raiseError[Vector[A]](error)
          case Right(_) if before > 0 => confirm(replies, before - 1, pending, feed, early)
          case Right(reply)           =>
            feed.confirmed(reply) match {
              case Some(name) => confirm(replies, before, pending - name, feed, early)
              case None       =>
                Async[F].fromEither(feed.message(reply)).flatMap(a => confirm(replies, before, pending, feed, early ++ a))
            }
        }
  }
}
