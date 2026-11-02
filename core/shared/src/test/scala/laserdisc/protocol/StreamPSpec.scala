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

final class StreamPSpec extends BaseSpec with StreamP {
  import streamtypes.*

  private def bulks(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)
  private val id1                     = StreamId(NonNegLong(1793590457192L), NonNegLong(0L))
  private val id2                     = StreamId(NonNegLong(9999999999999L), NonNegLong(1L))
  private val fields                  = OneOrMore.unsafeFrom(List(Key("a") -> 1, Key("b") -> 2))

  test("The Stream protocol using xadd sends NOMKSTREAM, the trim, the id and the fields in Redis's order") {
    assertEquals(xadd(Key("s"), fields).encode, bulks("XADD", "s", "*", "a", "1", "b", "2"))
    assertEquals(xadd(Key("s"), id2, fields).encode, bulks("XADD", "s", "9999999999999-1", "a", "1", "b", "2"))
    assertEquals(
      xadd(Key("s"), StreamNoMkStream, StreamTrim.maxLenApprox(NonNegLong(10L), PosLong(5L)), fields).encode,
      bulks("XADD", "s", "NOMKSTREAM", "MAXLEN", "~", "10", "LIMIT", "5", "*", "a", "1", "b", "2")
    )
    assertEquals(
      xadd(Key("s"), StreamTrim.minId(id1), fields).encode,
      bulks("XADD", "s", "MINID", "1793590457192-0", "*", "a", "1", "b", "2")
    )
    assertEquals(xadd(Key("s"), fields).decode(Bulk("1793590457192-0")), id1)
    assertEquals(xadd(Key("s"), StreamNoMkStream, fields).decode(NullBulk), None)
  }

  test("The Stream protocol using xrange and xrevrange sends the bounds and reads the entries") {
    assertEquals(xrange[Int](Key("s"), StreamBound.exclusive(id1), StreamBound.max).encode, bulks("XRANGE", "s", "(1793590457192-0", "+"))
    assertEquals(
      xrevrange[Int](Key("s"), StreamBound.max, StreamBound.min, PosLong(1L)).encode,
      bulks("XREVRANGE", "s", "+", "-", "COUNT", "1")
    )
    val reply = Arr(Arr(Bulk("1793590457192-0"), bulks("a", "1", "b", "2")), Arr(Bulk("9999999999999-1"), bulks("a", "4")))
    assertEquals(
      xrange[Int](Key("s"), StreamBound.min, StreamBound.max).decode(reply),
      Seq(StreamEntry(id1, Seq(Key("a") -> 1, Key("b") -> 2)), StreamEntry(id2, Seq(Key("a") -> 4)))
    )
  }

  test("The Stream protocol using xread sends every key and then every id, and reads the entries by stream, or None") {
    val streams = OneOrMore.unsafeFrom(List(Key("s") -> StreamFrom.after(id1), Key("t") -> StreamFrom.latest))
    assertEquals(xread[Int](streams, PosLong(1L)).encode, bulks("XREAD", "COUNT", "1", "STREAMS", "s", "t", "1793590457192-0", "$"))
    val reply = Arr(Arr(Bulk("s"), Arr(Arr(Bulk("9999999999999-1"), bulks("a", "4")))))
    assertEquals(xread[Int](streams).decode(reply), Some(Seq(KV(Key("s"), Seq(StreamEntry(id2, Seq(Key("a") -> 4)))))))
    assertEquals(xread[Int](streams).decode(NilArr), None)
  }

  test("The Stream protocol using xdel, xlen and xtrim sends and reads them") {
    assertEquals(xdel(Key("s"), OneOrMore.unsafeFrom(List(id1, id2))).encode, bulks("XDEL", "s", "1793590457192-0", "9999999999999-1"))
    assertEquals(xlen(Key("s")).decode(Num(3L)), NonNegLong(3L))
    assertEquals(xtrim(Key("s"), StreamTrim.maxLen(NonNegLong(1L))).encode, bulks("XTRIM", "s", "MAXLEN", "1"))
    assertEquals(xtrim(Key("s"), StreamTrim.minIdApprox(id1)).encode, bulks("XTRIM", "s", "MINID", "~", "1793590457192-0"))
  }
}
