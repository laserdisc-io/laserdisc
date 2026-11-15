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

import com.comcast.ip4s.SocketAddress

import scala.concurrent.duration.FiniteDuration

// why an error happened: its message, or the first of its causes' when it has none, as fs2's TLS errors on Node.js, or else what it is
private object Reason {
  def of(error: Throwable): String =
    Iterator.iterate(error)(_.getCause).takeWhile(_ != null).take(16).map(_.getMessage).find(_ != null).getOrElse(error.getClass.getName)
}

/** The connection closed, or failed, before the reply came: `cause` is the socket's error when there was one, such as a reset */
final case class ServerTerminatedConnection(redisAddress: RedisAddress, cause: Option[Throwable] = None)
    extends RuntimeException(
      s"Server $redisAddress terminated client connection${cause.fold("")(c => s": ${Reason.of(c)}")}",
      cause.orNull,
      true,
      false
    )
final case class ServerNotResponding(redisAddress: RedisAddress)
    extends RuntimeException(s"Server $redisAddress stopped replying", null, true, false)
final case class ConnectTimedOut(address: SocketAddress[com.comcast.ip4s.Host], after: FiniteDuration)
    extends RuntimeException(s"Connecting to $address timed out after $after", null, true, false)
final case class WatchLost(redisAddress: RedisAddress)
    extends RuntimeException(
      s"The connection to $redisAddress that the keys were watched on was closed, and the server dropped the watch with it, so none of the transaction's commands were sent",
      null,
      true,
      false
    )

/** A transaction typed as plain commands, as at the CLI, was on a connection that's gone, or lost the watch it needed, and the server
  * dropped it: what's sent until EXEC or DISCARD ends it isn't sent, so that nothing runs outside it
  */
final case class TransactionLost(redisAddress: RedisAddress)
    extends RuntimeException(
      s"The transaction begun with MULTI on the connection to $redisAddress is gone, with that connection or with the watch it needed, so this wasn't sent: EXEC or DISCARD ends it",
      null,
      true,
      false
    )
final case class WatchCleanupRefused(redisAddress: RedisAddress, command: String, reply: String)
    extends RuntimeException(
      s"Server $redisAddress didn't say OK to the $command ending a transaction whose watch was lost, so the connection was closed. It said: ${protocol.RESPDecErr
          .capped(reply)}",
      null,
      true,
      false
    )
final case class ConnectionSetupRefused(redisAddress: RedisAddress, reason: Throwable)
    extends RuntimeException(s"Server $redisAddress refused to set up the connection: ${Reason.of(reason)}", reason, true, false)

/** No server could be reached, or each closed the connection before answering: `cause` is why the last attempt failed */
final case class ServerUnavailable(cause: Throwable)
    extends RuntimeException(s"No server available: ${Reason.of(cause)}", cause, true, false)
object ClientTerminated extends RuntimeException("Client terminated connection", null, true, false)
final case class NoInFlightRequest(resp: RESP)
    extends RuntimeException(protocol.RESPDecErr.capped(s"Got unsolicited message from server: $resp"), null, true, false)
final case class InvalidSocketAddress(host: Host, port: Port)
    extends RuntimeException(s"SocketAddress creation failed. Invalid Host or Port: ${host.value}, ${port.value}", null, true, false)
final case class RequestTimedOut[A](protocol: Protocol[A]) extends RuntimeException(s"The request $protocol timed-out", null, true, false)
