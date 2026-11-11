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
package protocol

import scala.concurrent.duration.*

final class BlockingSpec extends BaseSpec {
  import redis_8_10.streamtypes.*

  private val b = redis_8_10.blocking

  // as typed at the CLI
  private def typed(words: String*): Protocol[RESP] = Protocol(words.head, words.tail.toList).using(RESPRead.raw)

  test("A blocking command's wait is read from what's sent, in seconds or milliseconds") {
    assertEquals(Blocking.waitOf(b.blpop[String](Key("k"), BlockingSeconds.unsafeFrom(1.5d))), 1500.millis)
    assertEquals(Blocking.waitOf(b.brpoplpush[String](Key("a"), Key("b"), BlockingSeconds.unsafeFrom(2d))), 2.seconds)
    assertEquals(Blocking.waitOf(b.xread[String](OneOrMore.unsafeFrom(List(Key("x") -> StreamFrom.latest)), NonNegLong(250L))), 250.millis)
    assertEquals(Blocking.waitOf(redis_8_10.wait(PosInt(1), PosLong(300L))), 300.millis)
    assertEquals(Blocking.waitOf(typed("bzmpop", "0.002", "1", "z", "MIN")), 2.millis)
    assertEquals(Blocking.waitOf(typed("BLMOVE", "a", "b", "LEFT", "RIGHT", "3")), 3.seconds)
    assertEquals(Blocking.waitOf(typed("waitaof", "0", "1", "40")), 40.millis)
  }

  test("A wait of 0, a command that doesn't block and a transaction add nothing") {
    assertEquals(Blocking.waitOf(b.blpop[String](Key("k"))), Duration.Zero)
    assertEquals(Blocking.waitOf(redis_8_10.wait(PosInt(1))), Duration.Zero)
    assertEquals(Blocking.waitOf(typed("XREAD", "STREAMS", "x", "$")), Duration.Zero)
    assertEquals(Blocking.waitOf(typed("GET", "k")), Duration.Zero)
    assertEquals(Blocking.waitOf(typed("BLPOP", "k", "soon")), Duration.Zero)
    assertEquals(Blocking.waitOf(redis_8_10.multi(b.blpop[String](Key("k"), BlockingSeconds.unsafeFrom(5d)))), Duration.Zero)
  }

  test("XREAD and XREADGROUP's BLOCK is only an option before STREAMS, not a group, consumer or stream named BLOCK") {
    assertEquals(Blocking.waitOf(typed("XREAD", "COUNT", "2", "BLOCK", "70", "STREAMS", "x", "$")), 70.millis)
    assertEquals(Blocking.waitOf(typed("XREAD", "STREAMS", "BLOCK", "5", "$", "$")), Duration.Zero)
    assertEquals(Blocking.waitOf(typed("XREADGROUP", "GROUP", "BLOCK", "BLOCK", "STREAMS", "x", ">")), Duration.Zero)
    assertEquals(Blocking.waitOf(typed("XREADGROUP", "GROUP", "g", "BLOCK", "block", "90", "NOACK", "STREAMS", "x", ">")), 90.millis)
  }

  test("The waits of a send add up, and stop at the longest wait there is") {
    assertEquals(Blocking.waitOf(List(typed("BLPOP", "k", "1"), typed("GET", "k"), typed("BRPOP", "k", "2"))), 3.seconds)
    assertEquals(
      Blocking.waitOf(List(typed("BLPOP", "k", "1e300"), typed("XREAD", "BLOCK", "9223372036854775807", "STREAMS", "x", "$"))),
      4294967.seconds
    )
  }
}
