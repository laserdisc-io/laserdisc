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
import cats.syntax.eq.*
import laserdisc.auto.*
import munit.CatsEffectSuite

final class RedisAddressSpec extends CatsEffectSuite {
  private val address = RedisAddress("localhost", 6379)

  test("an address gives its socket address") {
    address.toSocketAddress[IO].map(_.toString).assertEquals("localhost:6379")
  }

  test("a host valid for Redis but too long for a socket address fails") {
    val tooLong = Host.unsafeFrom(List.fill(4)("a" * 63).mkString("."))
    RedisAddress(tooLong, 6379).toSocketAddress[IO].attempt.map { outcome =>
      assertEquals(outcome.left.map(_.getClass.getSimpleName), Left("InvalidSocketAddress"))
    }
  }

  test("addresses are equal when their host and port are") {
    assert(address === RedisAddress("localhost", 6379))
    assert(address =!= RedisAddress("localhost", 6380))
    assert(address =!= RedisAddress("127.0.0.1", 6379))
  }

  test("an address shows as host:port") {
    assertEquals(address.toString, "localhost:6379")
  }
}
