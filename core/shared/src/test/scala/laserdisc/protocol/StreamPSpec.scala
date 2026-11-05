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

  test("The Stream protocol using xgroupcreate, xgroupsetid and the consumer commands sends their arguments") {
    assertEquals(
      xgroupcreate(Key("s"), Key("g"), StreamFrom.latest, StreamMkStream).encode,
      bulks("XGROUP", "CREATE", "s", "g", "$", "MKSTREAM")
    )
    assertEquals(
      xgroupcreate(Key("s"), Key("g"), StreamFrom.after(id1), NonNegLong(5L)).encode,
      bulks("XGROUP", "CREATE", "s", "g", "1793590457192-0", "ENTRIESREAD", "5")
    )
    assertEquals(xgroupsetid(Key("s"), Key("g"), StreamFrom.latest).encode, bulks("XGROUP", "SETID", "s", "g", "$"))
    assertEquals(xgroupcreateconsumer(Key("s"), Key("g"), Key("c")).decode(Num(1L)), true)
    assertEquals(xgroupdelconsumer(Key("s"), Key("g"), Key("c")).decode(Num(2L)), NonNegLong(2L))
    assertEquals(xgroupdestroy(Key("s"), Key("g")).decode(Num(0L)), false)
    assertEquals(
      xack(Key("s"), Key("g"), OneOrMore.unsafeFrom(List(id1, id2))).encode,
      bulks("XACK", "s", "g", "1793590457192-0", "9999999999999-1")
    )
  }

  test("The Stream protocol using xreadgroup sends the group, the options and the streams, and reads deleted entries with no fields") {
    val streams = OneOrMore.unsafeFrom(List(Key("s") -> StreamGroupFrom.newEntries))
    assertEquals(
      xreadgroup[Int](Key("g"), Key("c"), streams, PosLong(5L), StreamNoAck).encode,
      bulks("XREADGROUP", "GROUP", "g", "c", "COUNT", "5", "NOACK", "STREAMS", "s", ">")
    )
    val history = Arr(Arr(Bulk("s"), Arr(Arr(Bulk("1-1"), NilArr), Arr(Bulk("2-1"), bulks("a", "2")))))
    val one     = StreamId(NonNegLong(1L), NonNegLong(1L))
    val two     = StreamId(NonNegLong(2L), NonNegLong(1L))
    assertEquals(
      xreadgroup[Int](
        Key("g"),
        Key("c"),
        OneOrMore.unsafeFrom(List(Key("s") -> StreamGroupFrom.pendingAfter(StreamId(NonNegLong(0L), NonNegLong(0L)))))
      ).decode(history),
      Some(Seq(KV(Key("s"), Seq(StreamEntry(one, Nil), StreamEntry(two, Seq(Key("a") -> 2))))))
    )
  }

  test("The Stream protocol using xpending reads the summary, empty or not, and the pending entries") {
    val one = StreamId(NonNegLong(1L), NonNegLong(1L))
    val two = StreamId(NonNegLong(2L), NonNegLong(1L))
    assertEquals(
      xpending(Key("s"), Key("g")).decode(Arr(Num(2L), Bulk("1-1"), Bulk("2-1"), Arr(bulks("c1", "2")))),
      StreamPendingSummary(NonNegLong(2L), Some(one), Some(two), Seq(Key("c1") -> NonNegLong(2L)))
    )
    assertEquals(
      xpending(Key("s"), Key("g")).decode(Arr(Num(0L), NullBulk, NullBulk, NilArr)),
      StreamPendingSummary(NonNegLong(0L), None, None, Nil)
    )
    val detail = xpending(Key("s"), Key("g"), NonNegLong(0L), StreamBound.min, StreamBound.max, PosLong(10L), Key("c1"))
    assertEquals(detail.encode, bulks("XPENDING", "s", "g", "IDLE", "0", "-", "+", "10", "c1"))
    assertEquals(
      detail.decode(Arr(Arr(Bulk("1-1"), Bulk("c1"), Num(19L), Num(1L)))),
      Seq(StreamPending(one, Some(Key("c1")), Some(NonNegLong(19L)), NonNegLong(1L)))
    )
  }

  private def id(ms: Long): StreamId = StreamId(NonNegLong.unsafeFrom(ms), NonNegLong(1L))

  test("The Stream protocol using xclaim sends the options and reads the claimed entries, leaving out nils on Redis 6.2's import only") {
    val ids = OneOrMore.unsafeFrom(List(id(1L), id(2L)))
    assertEquals(
      xclaim[Int](
        Key("s"),
        Key("g"),
        Key("c"),
        NonNegLong(0L),
        ids,
        StreamClaimOptions.none.idle(NonNegLong(5L)).retryCount(NonNegLong(2L)).force
      ).encode,
      bulks("XCLAIM", "s", "g", "c", "0", "1-1", "2-1", "IDLE", "5", "RETRYCOUNT", "2", "FORCE")
    )
    val withNil = Arr(Arr(Bulk("1-1"), bulks("a", "1")), NullBulk)
    assertEquals(
      redis_6_2.xclaim[Int](Key("s"), Key("g"), Key("c"), NonNegLong(0L), ids).decode(withNil),
      Seq(StreamEntry(id(1L), Seq(Key("a") -> 1)))
    )
    assert(redis_7_0.xclaim[Int](Key("s"), Key("g"), Key("c"), NonNegLong(0L), ids).decode(withNil).isLeft)
    assertEquals(
      redis_7_0.xclaim[Int](Key("s"), Key("g"), Key("c"), NonNegLong(0L), ids).decode(Arr(Arr(Bulk("1-1"), bulks("a", "1")))),
      Seq(StreamEntry(id(1L), Seq(Key("a") -> 1)))
    )
    assertEquals(
      xclaimjustid(Key("s"), Key("g"), Key("c"), NonNegLong(0L), ids).encode,
      bulks("XCLAIM", "s", "g", "c", "0", "1-1", "2-1", "JUSTID")
    )
    assertEquals(xclaimjustid(Key("s"), Key("g"), Key("c"), NonNegLong(0L), ids).decode(bulks("3-1")), Seq(id(3L)))
  }

  test(
    "The Stream protocol using xautoclaim reads either reply on Redis 6.2's import, and only the one with the deleted ids from Redis 7.0"
  ) {
    val start   = StreamId(NonNegLong(0L), NonNegLong(0L))
    val redis62 = Arr(Bulk("0-0"), Arr(Arr(Bulk("1-1"), bulks("a", "1")), NullBulk))
    val redis70 = Arr(Bulk("0-0"), Arr(Arr(Bulk("1-1"), bulks("a", "1"))), bulks("2-1"))
    val older   = redis_6_2.xautoclaim[Int](Key("s"), Key("g"), Key("c"), NonNegLong(0L), start, PosLong(10L))
    val newer   = redis_7_0.xautoclaim[Int](Key("s"), Key("g"), Key("c"), NonNegLong(0L), start, PosLong(10L))
    assertEquals(newer.encode, bulks("XAUTOCLAIM", "s", "g", "c", "0", "0-0", "COUNT", "10"))
    assertEquals(older.decode(redis62), StreamP.AutoClaimed(start, Seq(StreamEntry(id(1L), Seq(Key("a") -> 1))), Nil))
    assertEquals(older.decode(redis70), StreamP.AutoClaimed(start, Seq(StreamEntry(id(1L), Seq(Key("a") -> 1))), Seq(id(2L))))
    assertEquals(newer.decode(redis70), StreamP.AutoClaimed(start, Seq(StreamEntry(id(1L), Seq(Key("a") -> 1))), Seq(id(2L))))
    assert(newer.decode(redis62).isLeft)
    val justIds = Arr(Bulk("0-0"), bulks("1-1", "3-1"), Arr(Nil))
    assertEquals(
      redis_7_0.xautoclaimjustid(Key("s"), Key("g"), Key("c"), NonNegLong(0L), start).decode(justIds),
      StreamP.AutoClaimed(start, Seq(id(1L), id(3L)), Nil)
    )
    assertEquals(
      redis_6_2.xautoclaimjustid(Key("s"), Key("g"), Key("c"), NonNegLong(0L), start).decode(justIds),
      StreamP.AutoClaimed(start, Seq(id(1L), id(3L)), Nil)
    )
    assert(redis_7_0.xautoclaimjustid(Key("s"), Key("g"), Key("c"), NonNegLong(0L), start).decode(Arr(Bulk("0-0"), bulks("1-1"))).isLeft)
  }

  test("The Stream protocol using xinfo reads the stream, its groups and its consumers, with the newer fields when there") {
    val stream = Arr(
      Bulk("length") :: Num(2L) :: Bulk("radix-tree-keys") :: Num(1L) :: Bulk("radix-tree-nodes") :: Num(2L) :: Bulk("last-generated-id") ::
        Bulk("3-1") :: Bulk("entries-added") :: Num(3L) :: Bulk("groups") :: Num(1L) :: Bulk("first-entry") :: Arr(
          Bulk("1-1"),
          bulks("a", "1")
        ) ::
        Bulk("last-entry") :: NullBulk :: Nil
    )
    assertEquals(
      xinfostream[Int](Key("s")).decode(stream),
      StreamP.StreamInfo(
        NonNegLong(2L),
        NonNegLong(1L),
        NonNegLong(2L),
        id(3L),
        NonNegLong(1L),
        Some(StreamEntry(id(1L), Seq(Key("a") -> 1))),
        None,
        Map("entries-added" -> "3")
      )
    )
    val keydbGroup = Arr(
      Bulk("name") :: Bulk("g") :: Bulk("consumers") :: Num(3L) :: Bulk("pending") :: Num(3L) :: Bulk("last-delivered-id") :: Bulk(
        "3-1"
      ) :: Nil
    )
    val redisGroup = Arr(keydbGroup.elements ::: List(Bulk("entries-read"), Num(3L), Bulk("lag"), NullBulk))
    assertEquals(
      xinfogroups(Key("s")).decode(Arr(keydbGroup, redisGroup)),
      Seq(
        StreamP.GroupInfo(Key("g"), NonNegLong(3L), NonNegLong(3L), id(3L), None, None),
        StreamP.GroupInfo(Key("g"), NonNegLong(3L), NonNegLong(3L), id(3L), Some(NonNegLong(3L)), None)
      )
    )
    val consumer =
      Arr(Bulk("name") :: Bulk("c1") :: Bulk("pending") :: Num(0L) :: Bulk("idle") :: Num(29L) :: Bulk("inactive") :: Num(-1L) :: Nil)
    assertEquals(
      xinfoconsumers(Key("s"), Key("g")).decode(Arr(consumer)),
      Seq(StreamP.ConsumerInfo(Key("c1"), NonNegLong(0L), NonNegLong(29L), Some(-1L)))
    )
    assertEquals(
      xsetid(Key("s"), id(9L), NonNegLong(3L), id(2L)).encode,
      bulks("XSETID", "s", "9-1", "ENTRIESADDED", "3", "MAXDELETEDID", "2-1")
    )
  }

  test("The Stream protocol using Redis 8.2's reference policies sends them and reads what XDELEX and XACKDEL did to each entry") {
    val ids = OneOrMore.unsafeFrom(List(id(1L), id(9L)))
    assertEquals(
      xadd(Key("s"), StreamTrim.maxLen(NonNegLong(1L)), StreamRefPolicy.keepRef, fields).encode,
      bulks("XADD", "s", "KEEPREF", "MAXLEN", "1", "*", "a", "1", "b", "2")
    )
    assertEquals(
      xtrim(Key("s"), StreamTrim.maxLen(NonNegLong(0L)), StreamRefPolicy.acked).encode,
      bulks("XTRIM", "s", "MAXLEN", "0", "ACKED")
    )
    assertEquals(xdelex(Key("s"), ids, StreamRefPolicy.delRef).encode, bulks("XDELEX", "s", "DELREF", "IDS", "2", "1-1", "9-1"))
    assertEquals(xackdel(Key("s"), Key("g"), ids).encode, bulks("XACKDEL", "s", "g", "IDS", "2", "1-1", "9-1"))
    assertEquals(
      xdelex(Key("s"), ids).decode(Arr(Num(1L), Num(-1L), Num(2L))),
      Seq(StreamDeleteResult.Deleted, StreamDeleteResult.NotFound, StreamDeleteResult.Referenced)
    )
  }

  test("The Stream protocol using Redis 8.6's idempotent XADD and XCFGSET sends them") {
    assertEquals(
      xadd(Key("s"), StreamIdmp.of(Key("p1"), Key("i1")), fields).encode,
      bulks("XADD", "s", "IDMP", "p1", "i1", "*", "a", "1", "b", "2")
    )
    assertEquals(
      xadd(Key("s"), StreamIdmp.auto(Key("p2")), StreamTrim.maxLenApprox(NonNegLong(10L)), fields).encode,
      bulks("XADD", "s", "IDMPAUTO", "p2", "MAXLEN", "~", "10", "*", "a", "1", "b", "2")
    )
    assertEquals(
      xcfgset(Key("s"), StreamIdmpConfig.duration(PosLong(50L)).maxSize(PosLong(10L))).encode,
      bulks("XCFGSET", "s", "IDMP-DURATION", "50", "IDMP-MAXSIZE", "10")
    )
  }

  test("The Stream protocol using Redis 8.8's XNACK sends the mode, the ids and the options, and reads how many it gave back") {
    val ids = OneOrMore.unsafeFrom(List(StreamId(NonNegLong(1L), NonNegLong(1L)), StreamId(NonNegLong(9L), NonNegLong(1L))))
    assertEquals(xnack(Key("s"), Key("g"), StreamNackMode.silent, ids).encode, bulks("XNACK", "s", "g", "SILENT", "IDS", "2", "1-1", "9-1"))
    assertEquals(xnack(Key("s"), Key("g"), StreamNackMode.fail, ids).encode, bulks("XNACK", "s", "g", "FAIL", "IDS", "2", "1-1", "9-1"))
    assertEquals(
      xnack(Key("s"), Key("g"), StreamNackMode.fatal, ids, StreamNackOptions.retryCount(NonNegLong(5L)).force).encode,
      bulks("XNACK", "s", "g", "FATAL", "IDS", "2", "1-1", "9-1", "RETRYCOUNT", "5", "FORCE")
    )
    assertEquals(xnack(Key("s"), Key("g"), StreamNackMode.fail, ids).decode(Num(1L)), NonNegLong(1L))
    assertEquals(
      xpending(Key("s"), Key("g"), StreamBound.min, StreamBound.max, PosLong(1L))
        .decode(Arr(Arr(Bulk("1-1"), Bulk(""), Num(-1L), Num(2L)))),
      Seq(StreamPending(StreamId(NonNegLong(1L), NonNegLong(1L)), None, None, NonNegLong(2L)))
    )
  }
}
