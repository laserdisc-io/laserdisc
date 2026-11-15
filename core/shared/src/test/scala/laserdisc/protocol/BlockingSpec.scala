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
  private def typed(words: String*): Protocol.Single[RESP] = Protocol(words.head, words.tail.toList).using(RESPRead.raw)

  test("REPLTAKEOVER and CLUSTER SETSLOT with TIMEOUT get their wait on top of the timeout") {
    assertEquals(Blocking.waitOf(laserdisc.dragonfly_2_0.repltakeover(PosInt(60))), 60.seconds)
    assertEquals(Blocking.waitOf(laserdisc.valkey_8_0.setslot(Slot(1), NonNegLong(30000L))), 30.seconds)
    assertEquals(
      Blocking.waitOf(laserdisc.valkey_8_0.setslot(Slot(1), ClusterP.SetSlotMode.node, NodeId.unsafeFrom("a" * 40), NonNegLong(2500L))),
      2500.millis
    )
    assertEquals(Blocking.waitOf(laserdisc.valkey_8_0.setslot(Slot(1))), Duration.Zero)
  }

  test("A blocking command's wait is read from what's sent, in seconds or milliseconds") {
    assertEquals(Blocking.waitOf(b.blpop[String](Key("k"), BlockingSeconds.unsafeFrom(1.5d))), 1500.millis)
    assertEquals(Blocking.waitOf(b.brpoplpush[String](Key("a"), Key("b"), BlockingSeconds.unsafeFrom(2d))), 2.seconds)
    assertEquals(Blocking.waitOf(b.xread[String](OneOrMore.unsafeFrom(List(Key("x") -> StreamFrom.latest)), PosLong(250L))), 250.millis)
    assertEquals(Blocking.waitOf(redis_8_10.wait(PosInt(1), PosLong(300L))), 300.millis)
    assertEquals(Blocking.waitOf(typed("bzmpop", "0.002", "1", "z", "MIN")), 2.millis)
    assertEquals(Blocking.waitOf(typed("BLMOVE", "a", "b", "LEFT", "RIGHT", "3")), 3.seconds)
    assertEquals(Blocking.waitOf(typed("BLMOVEM", "a", "b", "LEFT", "RIGHT", "4", "COUNT", "2", "OBO")), 4.seconds)
    val left = ListP.Side.left
    assertEquals(Blocking.waitOf(b.blmovem[String](Key("a"), Key("b"), left, left, BlockingSeconds.unsafeFrom(2.5d))), 2500.millis)
    assertEquals(
      Blocking.waitOf(
        b.blmovem[String](
          Key("a"),
          Key("b"),
          left,
          left,
          ListP.Amount.upTo(PosInt(2)),
          ListP.Order.oneByOne,
          BlockingSeconds.unsafeFrom(6d)
        )
      ),
      6.seconds
    )
    assertEquals(Blocking.waitOf(typed("waitaof", "0", "1", "40")), 40.millis)
  }

  test("A wait of 0, a command that doesn't block and a transaction add nothing") {
    assertEquals(Blocking.waitOf(typed("BLPOP", "k", "0")), Duration.Zero)
    assertEquals(Blocking.waitOf(typed("WAIT", "1", "0")), Duration.Zero)
    assertEquals(Blocking.waitOf(typed("XREAD", "STREAMS", "x", "$")), Duration.Zero)
    assertEquals(Blocking.waitOf(typed("GET", "k")), Duration.Zero)
    assertEquals(Blocking.waitOf(typed("BLPOP", "k", "soon")), Duration.Zero)
    assertEquals(Blocking.waitOf(redis_8_10.multi(b.blpop[String](Key("k"), BlockingSeconds.unsafeFrom(5d)))), Duration.Zero)
  }

  test("A command typed with a wait of 0 waits for as long as it takes, and is sent as typed") {
    val forever = List(
      typed("BLPOP", "a", "b", "0"),
      typed("brpop", "k", "0.0"),
      typed("BZMPOP", "0", "1", "z", "MIN"),
      typed("BLMOVEM", "a", "b", "LEFT", "RIGHT", "0", "COUNT", "2", "OBO"),
      typed("WAIT", "1", "0"),
      typed("WAITAOF", "0", "1", "0"),
      typed("XREAD", "BLOCK", "0", "STREAMS", "x", "$"),
      typed("XREADGROUP", "GROUP", "g", "c", "BLOCK", "0", "STREAMS", "x", ">")
    )
    forever.foreach { protocol =>
      assert(Blocking.waitsForever(protocol), protocol.encode)
      assertEquals(Forever.from(protocol).map(_.protocol.encode), Some(protocol.encode))
    }
    val not = List(
      typed("BLPOP", "k", "1"),
      typed("BLPOP", "k", "soon"),
      typed("BLPOP"),
      typed("WAIT", "1", "5"),
      typed("XREAD", "STREAMS", "x", "$"),
      typed("XREAD", "STREAMS", "BLOCK", "0", "$", "$"),
      typed("GET", "0")
    )
    not.foreach { protocol =>
      assert(!Blocking.waitsForever(protocol), protocol.encode)
      assertEquals(Forever.from(protocol), None)
    }
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

  test("MIGRATE waits up to its timeout, in milliseconds, or a second when it's 0, which isn't a wait for as long as it takes") {
    import laserdisc.redis_6_2.migrate
    val (host, port, db) = (Host("10.0.0.1"), Port(6379), DbIndex(0))
    assertEquals(Blocking.waitOf(migrate(Key("k"), host, port, db, NonNegInt(30000))), 30.seconds)
    assertEquals(Blocking.waitOf(migrate(TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))), host, port, db, NonNegInt(2500))), 2500.millis)
    assertEquals(Blocking.waitOf(migrate(Key("k"), host, port, db, NonNegInt(0))), 1.second)
    assert(!Blocking.waitsForever(migrate(Key("k"), host, port, db, NonNegInt(0))))
  }
}
