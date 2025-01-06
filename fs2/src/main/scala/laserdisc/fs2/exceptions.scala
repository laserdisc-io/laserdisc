/*
 * Copyright (c) 2018-2025 LaserDisc
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
    extends Platform.LaserDiscRuntimeError(s"Server $redisAddress terminated client connection")
object ServerUnavailable        extends Platform.LaserDiscRuntimeError("No server available")
object ClientTerminated         extends Platform.LaserDiscRuntimeError("Client terminated connection")
object ClientNotStartedProperly extends Platform.LaserDiscRuntimeError("Client trying to publish to a connection not yet established")
final case class NoInFlightRequest(resp: RESP) extends Platform.LaserDiscRuntimeError(s"Got unsolicited message from server: $resp")
final case class InvalidSocketAddress(host: Host, port: Port)
    extends Platform.LaserDiscRuntimeError(s"SocketAddress creation failed. Invalid Host or Port: ${host.value}, ${port.value}")
final case class RequestTimedOut[A](protocol: Protocol.Aux[A]) extends Platform.LaserDiscRuntimeError(s"The request $protocol timed-out")
