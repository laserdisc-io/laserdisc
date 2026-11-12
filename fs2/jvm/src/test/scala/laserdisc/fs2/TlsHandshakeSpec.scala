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

import cats.effect.IO
import cats.syntax.all.*
import com.comcast.ip4s
import munit.CatsEffectSuite

import scala.concurrent.duration.*

final class TlsHandshakeSpec extends CatsEffectSuite {

  // takes the connection and never answers, as a port that doesn't speak TLS can
  private val silent = FakeRedisServer((_, _) => Stream.never[IO])

  test("connecting with TLS gives up within the connect timeout when the server never answers the handshake") {
    (silent, Tls.system[IO]).tupled.use { case (port, tls) =>
      val address = ip4s.SocketAddress(ip4s.Host.fromString("localhost").get, ip4s.Port.fromInt(port.value).get)
      RedisChannel
        .connectedSocket[IO](address, 1024, 1.second, tls.secure(_, "localhost"))
        .use_
        .attempt
        .timeout(10.seconds)
        .map(outcome => assert(outcome.left.exists(_.isInstanceOf[ConnectTimedOut]), outcome.toString))
    }
  }
}
