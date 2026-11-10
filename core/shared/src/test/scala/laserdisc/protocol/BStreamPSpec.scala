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

final class BStreamPSpec extends BaseSpec with BStreamP {
  import StreamP.{ClaimedEntry, Entry, From, GroupFrom, Id, NoAck, ReadLimits}

  private def bulks(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)
  private val id                      = Id(NonNegLong(1L), NonNegLong(1L))

  test("The blocking Stream protocol using xread sends COUNT and BLOCK before STREAMS, and reads None when the wait ran out") {
    val streams = OneOrMore.unsafeFrom(List(Key("s") -> From.after(id), Key("t") -> From.latest))
    assertEquals(
      xread[Int](streams, NonNegLong(5000L), PosLong(10L)).encode,
      bulks("XREAD", "COUNT", "10", "BLOCK", "5000", "STREAMS", "s", "t", "1-1", "$")
    )
    assertEquals(xread[Int](streams, NonNegLong(0L)).encode, bulks("XREAD", "BLOCK", "0", "STREAMS", "s", "t", "1-1", "$"))
    assertEquals(xread[Int](streams, NonNegLong(5000L)).decode(NilArr), None)
    assertEquals(
      xread[Int](streams, NonNegLong(5000L)).decode(Arr(Arr(Bulk("s"), Arr(Arr(Bulk("1-1"), bulks("a", "1")))))),
      Some(Seq(KV(Key("s"), Seq(Entry(id, Seq(Key("a") -> 1))))))
    )
  }

  test("The blocking Stream protocol using xreadgroup sends the group, COUNT, BLOCK and NOACK before STREAMS") {
    val streams = OneOrMore.unsafeFrom(List(Key("s") -> GroupFrom.newEntries))
    assertEquals(
      xreadgroup[Int](Key("g"), Key("c"), streams, NonNegLong(100L), PosLong(5L), NoAck).encode,
      bulks("XREADGROUP", "GROUP", "g", "c", "COUNT", "5", "BLOCK", "100", "NOACK", "STREAMS", "s", ">")
    )
    assertEquals(xreadgroup[Int](Key("g"), Key("c"), streams, NonNegLong(100L)).decode(NilArr), None)
  }

  private val keys = OneOrMoreKeys.unsafeFrom(List(Key("s")))

  test("The blocking Stream protocol using xreadgroupclaim sends COUNT, BLOCK, CLAIM and NOACK before STREAMS") {
    assertEquals(
      xreadgroupclaim[Int](Key("g"), Key("c"), keys, NonNegLong(50L), NonNegLong(100L), PosLong(5L), NoAck).encode,
      bulks("XREADGROUP", "GROUP", "g", "c", "COUNT", "5", "BLOCK", "100", "CLAIM", "50", "NOACK", "STREAMS", "s", ">")
    )
    assertEquals(
      xreadgroupclaim[Int](Key("g"), Key("c"), keys, NonNegLong(50L), NonNegLong(100L)).encode,
      bulks("XREADGROUP", "GROUP", "g", "c", "BLOCK", "100", "CLAIM", "50", "STREAMS", "s", ">")
    )
    assertEquals(xreadgroupclaim[Int](Key("g"), Key("c"), keys, NonNegLong(50L), NonNegLong(100L)).decode(NilArr), None)
    assertEquals(
      xreadgroupclaim[Int](Key("g"), Key("c"), keys, NonNegLong(50L), NonNegLong(100L))
        .decode(Arr(Arr(Bulk("s"), Arr(Arr(Bulk("1-1"), bulks("a", "1"), Num(0L), Num(0L)))))),
      Some(Seq(KV(Key("s"), Seq(ClaimedEntry(id, Seq(Key("a") -> 1), NonNegLong(0L), NonNegLong(0L))))))
    )
  }

  test("The blocking Stream protocol sends MAXCOUNT and MAXSIZE between COUNT and BLOCK") {
    val limits = ReadLimits.maxCount(PosLong(2L))
    val from   = OneOrMore.unsafeFrom(List(Key("s") -> From.latest))
    val group  = OneOrMore.unsafeFrom(List(Key("s") -> GroupFrom.newEntries))
    assertEquals(
      xread[Int](from, NonNegLong(100L), limits).encode,
      bulks("XREAD", "MAXCOUNT", "2", "BLOCK", "100", "STREAMS", "s", "$")
    )
    assertEquals(
      xread[Int](from, NonNegLong(100L), PosLong(1L), limits).encode,
      bulks("XREAD", "COUNT", "1", "MAXCOUNT", "2", "BLOCK", "100", "STREAMS", "s", "$")
    )
    assertEquals(
      xreadgroup[Int](Key("g"), Key("c"), group, NonNegLong(100L), PosLong(1L), limits, NoAck).encode,
      bulks("XREADGROUP", "GROUP", "g", "c", "COUNT", "1", "MAXCOUNT", "2", "BLOCK", "100", "NOACK", "STREAMS", "s", ">")
    )
    assertEquals(
      xreadgroupclaim[Int](Key("g"), Key("c"), keys, NonNegLong(50L), NonNegLong(100L), PosLong(1L), limits, NoAck).encode,
      bulks("XREADGROUP", "GROUP", "g", "c", "COUNT", "1", "MAXCOUNT", "2", "BLOCK", "100", "CLAIM", "50", "NOACK", "STREAMS", "s", ">")
    )
  }
}
