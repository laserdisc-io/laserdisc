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
import cats.syntax.traverse.*
import laserdisc.auto.*
import munit.CatsEffectSuite

final class RedisAddressSpec extends CatsEffectSuite {
  private val address = RedisAddress("localhost", 6379)

  test("an address gives its socket address") {
    address.toSocketAddress[IO].map(_.toString).assertEquals("localhost:6379")
  }

  test("a hostname ending with a dot gives the socket address of the name without it") {
    RedisAddress("localhost.", 6379).toSocketAddress[IO].map(_.toString).assertEquals("localhost:6379")
  }

  test("every kind of host gives a socket address") {
    val hosts = List(
      "1.1.1.1",
      "::",
      "::1",
      "1:2:3:4:5:6:7:8",
      "1:2:3:4:5:6:7::",
      "::ffff:192.0.2.1",
      "1:2:3:4:5:6:192.0.2.1",
      "fe80::1%eth0",
      "redis.example.com",
      "redis.example.com.",
      List.fill(3)("a" * 63).mkString(".") + "." + "a" * 61
    )
    hosts.traverse(h => RedisAddress(Host.unsafeFrom(h), 6379).toSocketAddress[IO].attempt.map(h -> _)).map { outcomes =>
      assertEquals(outcomes.collect { case (h, Left(_)) => h }, Nil)
    }
  }

  test("addresses are equal when their host and port are") {
    assert(address === RedisAddress("localhost", 6379))
    assert(address =!= RedisAddress("localhost", 6380))
    assert(address =!= RedisAddress("127.0.0.1", 6379))
  }

  test("addresses are ordered by host, then port") {
    val ordered =
      List(RedisAddress("b", 1), RedisAddress("a", 2), RedisAddress("a", 1)).sorted(using RedisAddress.redisAddressOrder.toOrdering)
    assertEquals(ordered, List(RedisAddress("a", 1), RedisAddress("a", 2), RedisAddress("b", 1)))
  }

  test("an address shows as host:port") {
    assertEquals(address.toString, "localhost:6379")
  }
}
