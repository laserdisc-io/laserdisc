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

final case class ServerTerminatedConnection(redisAddress: RedisAddress)
    extends RuntimeException(s"Server $redisAddress terminated client connection", null, true, false)
object ServerUnavailable        extends RuntimeException("No server available", null, true, false)
object ClientTerminated         extends RuntimeException("Client terminated connection", null, true, false)
object ClientNotStartedProperly extends RuntimeException("Client trying to publish to a connection not yet established", null, true, false)
final case class NoInFlightRequest(resp: RESP) extends RuntimeException(s"Got unsolicited message from server: $resp", null, true, false)
final case class InvalidSocketAddress(host: Host, port: Port)
    extends RuntimeException(s"SocketAddress creation failed. Invalid Host or Port: ${host.value}, ${port.value}", null, true, false)
final case class RequestTimedOut[A](protocol: Protocol.Aux[A])
    extends RuntimeException(s"The request $protocol timed-out", null, true, false)
