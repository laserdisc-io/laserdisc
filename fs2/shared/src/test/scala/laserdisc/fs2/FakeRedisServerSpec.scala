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
import cats.effect.IO
import com.comcast.ip4s
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.*

final class FakeRedisServerSpec extends CatsEffectSuite {
  private val ping = Chunk.array("*1\r\n$4\r\nPING\r\n".getBytes(UTF_8))

  test("a connection the client resets leaves the server answering its other connections") {
    FakeRedisServer { (n, commands) =>
      if (n == 1) commands.flatMap(_ => Stream.constant("+" + ("x" * 1000) + "\r\n"))
      else commands.map(_ => "+PONG\r\n")
    }.use { port =>
      val address = ip4s.SocketAddress(ip4s.Ipv4Address.fromString("127.0.0.1").get, ip4s.Port.fromInt(port.value).get)
      // closed with replies still coming, so the server's side of it is reset
      val resetting = Network[IO].connect(address).use(socket => socket.write(ping) >> socket.read(10).void)
      val answered  = Network[IO].connect(address).use(socket => socket.write(ping) >> socket.read(7))

      (resetting >> IO.sleep(500.millis) >> answered.timeout(5.seconds))
        .map(_.map(chunk => new String(chunk.toArray, UTF_8)))
        .assertEquals(Some("+PONG\r\n"))
    }
  }
}
