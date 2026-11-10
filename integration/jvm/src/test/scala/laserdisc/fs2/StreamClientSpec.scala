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

import laserdisc.auto.*

final class RedisStreamClientSpec extends StreamClientSpec(LiveServers.port(6379), "redis") {
  since[Redis7_0]("a redis client reads the replies of Redis 7.0's import, which takes only what's replied from that version on")(v =>
    repliesFrom7_0(v)
  )

  since[Redis8_2]("a redis client deletes with reference policies, and trims") { v =>
    import v.*
    import streamtypes.*

    send(del("x:r"))
    send(xgroupcreate("x:r", "grp", StreamFrom.latest, StreamMkStream))
    val first  = send(xadd("x:r", OneOrMore.unsafeFrom(List(Key("a") -> 1))))
    val second = send(xadd("x:r", OneOrMore.unsafeFrom(List(Key("a") -> 2))))
    send(xreadgroup[Int]("grp", "c", OneOrMore.unsafeFrom(List(Key("x:r") -> StreamGroupFrom.newEntries))))

    assertEquals(send(xdelex("x:r", OneOrMore.unsafeFrom(List(first)), StreamRefPolicy.acked)), Seq(StreamDeleteResult.Referenced))
    assertEquals(
      send(xackdel("x:r", "grp", OneOrMore.unsafeFrom(List(first, StreamId(9999999999999L, 0L))))),
      Seq(StreamDeleteResult.Deleted, StreamDeleteResult.NotFound)
    )
    assertEquals(send(xtrim("x:r", StreamTrim.maxLen(0L), StreamRefPolicy.acked)).value, 0L)
    assertEquals(send(xtrim("x:r", StreamTrim.maxLen(0L), StreamRefPolicy.delRef)).value, 1L)
    assert(second.milliseconds.toBigInt > 0)

    // the server keeps both parts of an id unsigned, up to 2^64 - 1
    val big = StreamId(StreamIdPart.max, StreamIdPart.max)
    send(del("x:big"))
    assertEquals(send(xadd("x:big", big, OneOrMore.unsafeFrom(List(Key("a") -> 1)))), big)
    assertEquals(send(xrange[Int]("x:big", StreamBound.min, StreamBound.max)).map(_.id), Seq(big))
    send(del("x:big"))

    send(del("x:at"))
    assertEquals(send(xadd("x:at", StreamIdAt(5L), OneOrMore.unsafeFrom(List(Key("a") -> 1)))), StreamId(5L, 0L))
    assertEquals(
      send(xadd("x:at", StreamIdAt(5L), StreamTrim.maxLen(10L), StreamRefPolicy.keepRef, OneOrMore.unsafeFrom(List(Key("a") -> 1)))),
      StreamId(5L, 1L)
    )
  }

  since[Redis8_6]("a redis client adds idempotently") { v =>
    import v.*
    import streamtypes.*

    send(del("x:r"))
    val once  = send(xadd("x:r", StreamIdmp.of("producer", "order-1"), OneOrMore.unsafeFrom(List(Key("a") -> 3))))
    val again = send(xadd("x:r", StreamIdmp.of("producer", "order-1"), OneOrMore.unsafeFrom(List(Key("a") -> 3))))
    assertEquals(again, once)
    assertEquals(send(xlen("x:r")).value, 1L)
    assertEquals(send(xcfgset("x:r", StreamIdmpConfig.duration(60L).maxSize(100L))), OK)

    val idmp = StreamIdmp.of("producer", "order-2")
    assertEquals(send(xadd("x:missing", StreamNoMkStream, idmp, OneOrMore.unsafeFrom(List(Key("a") -> 3)))), None)
    assert(
      send(
        xadd("x:r", StreamNoMkStream, idmp, StreamTrim.maxLen(10L), StreamRefPolicy.keepRef, OneOrMore.unsafeFrom(List(Key("a") -> 3)))
      ).isDefined
    )
    assertEquals(
      send(xadd("x:r", idmp, StreamTrim.maxLen(10L), StreamRefPolicy.keepRef, OneOrMore.unsafeFrom(List(Key("a") -> 3)))),
      send(xadd("x:r", idmp, OneOrMore.unsafeFrom(List(Key("a") -> 3))))
    )
  }

  since[Redis8_4]("a redis client takes over idle entries with XREADGROUP's CLAIM") { v =>
    import v.*
    import streamtypes.*

    send(del("x:m", "x:m2"))
    send(xgroupcreate("x:m", "grp", StreamFrom.latest, StreamMkStream))
    val first  = send(xadd("x:m", OneOrMore.unsafeFrom(List(Key("a") -> 1))))
    val second = send(xadd("x:m", OneOrMore.unsafeFrom(List(Key("a") -> 2))))
    send(xreadgroup[Int]("grp", "c1", OneOrMore.unsafeFrom(List(Key("x:m") -> StreamGroupFrom.newEntries)), 1L))

    val keys    = OneOrMoreKeys.unsafeFrom(List(Key("x:m")))
    val claimed = send(xreadgroupclaim[Int]("grp", "c2", keys, 0L)).toList.flatten.flatMap(_.value)
    assertEquals(claimed.map(e => e.id -> e.deliveries.value), List(first -> 1L, second -> 0L))
    assertEquals(claimed.map(_.fields), List(Seq(Key("a") -> 1), Seq(Key("a") -> 2)))

    val claimedAgain = send(blocking.xreadgroupclaim[Int]("grp", "c3", keys, 0L, 100L, 1L)).toList.flatten.flatMap(_.value)
    assertEquals(claimedAgain.map(e => e.id -> e.deliveries.value), List(first -> 2L))
  }

  since[Redis8_10]("a redis client limits whole reads with MAXCOUNT and MAXSIZE") { v =>
    import v.*
    import streamtypes.*

    send(del("x:m", "x:m2"))
    send(xgroupcreate("x:m", "grp", StreamFrom.latest, StreamMkStream))
    val first  = send(xadd("x:m", OneOrMore.unsafeFrom(List(Key("a") -> 1))))
    val second = send(xadd("x:m", OneOrMore.unsafeFrom(List(Key("a") -> 2))))

    send(xadd("x:m2", OneOrMore.unsafeFrom(List(Key("a") -> 3))))
    val both =
      OneOrMore.unsafeFrom(List(Key("x:m") -> StreamFrom.after(StreamId(0L, 0L)), Key("x:m2") -> StreamFrom.after(StreamId(0L, 0L))))
    def read(reply: Option[Seq[KV[Seq[StreamEntry[Int]]]]]) = reply.toList.flatten.flatMap(_.value.map(_.id))
    assertEquals(read(send(xread[Int](both))).size, 3)
    assertEquals(read(send(xread[Int](both, StreamReadLimits.maxCount(2L)))), List(first, second))
    assertEquals(read(send(blocking.xread[Int](both, 100L, StreamReadLimits.maxCount(1L)))), List(first))
    assertEquals(read(send(xread[Int](both, StreamReadLimits.maxSize(1L)))), List(first))

    send(
      xreadgroup[Int](
        "grp",
        "c4",
        OneOrMore.unsafeFrom(List(Key("x:m") -> StreamGroupFrom.pendingAfter(StreamId(0L, 0L)))),
        StreamReadLimits.maxCount(1L)
      )
    )
    val info = send(xinfostreamfull[Int]("x:m"))
    assert(info.others.contains("entries-added"))
    assert(info.groups.head.others.contains("nacked-count"))
  }

  since[Redis8_8]("a redis client gives entries back to the group") { v =>
    import v.*
    import streamtypes.*

    send(del("x:n"))
    send(xgroupcreate("x:n", "grp", StreamFrom.latest, StreamMkStream))
    val first = send(xadd("x:n", OneOrMore.unsafeFrom(List(Key("a") -> 1))))
    send(xreadgroup[Int]("grp", "c", OneOrMore.unsafeFrom(List(Key("x:n") -> StreamGroupFrom.newEntries))))

    val ids = OneOrMore.unsafeFrom(List(first))
    assertEquals(send(xnack("x:n", "grp", StreamNackMode.fail, ids)).value, 1L)
    assertEquals(send(xpending("x:n", "grp", StreamBound.min, StreamBound.max, 10L)), Seq(StreamPending(first, None, None, 1L)))

    assertEquals(send(xnack("x:n", "grp", StreamNackMode.silent, ids, StreamNackOptions.retryCount(5L))).value, 1L)
    assertEquals(send(xpending("x:n", "grp", StreamBound.min, StreamBound.max, 10L)), Seq(StreamPending(first, None, None, 5L)))

    send(xack("x:n", "grp", ids))
    assertEquals(send(xnack("x:n", "grp", StreamNackMode.fail, ids)).value, 0L)
    assertEquals(send(xnack("x:n", "grp", StreamNackMode.fail, ids, StreamNackOptions.force)).value, 1L)
  }
}
final class KeyDbStreamClientSpec  extends StreamClientSpec(LiveServers.port(6380), "keyDb")
final class ValkeyStreamClientSpec extends StreamClientSpec(LiveServers.port(6381), "valkey") {
  since[Valkey7_2]("a valkey client reads the replies of Redis 7.0's import, which takes only what's replied from that version on")(v =>
    repliesFrom7_0(v)
  )

  since[Valkey9_1]("a valkey client leaves an entry's sequence to the server") { v =>
    import v.*
    import streamtypes.*

    send(del("x:at"))
    assertEquals(send(xadd("x:at", StreamIdAt(5L), OneOrMore.unsafeFrom(List(Key("a") -> 1)))), StreamId(5L, 0L))
    assertEquals(send(xadd("x:at", StreamNoMkStream, StreamIdAt(5L), OneOrMore.unsafeFrom(List(Key("a") -> 1)))), Some(StreamId(5L, 1L)))
  }
}

abstract class StreamClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  // XCLAIM and XAUTOCLAIM's deleted entries, LPOP and RPOP with a count, ACL GETUSER: the replies that changed in Redis 7.0
  protected final def repliesFrom7_0(version: Redis7_0): Unit = {
    import version.{acl, del, lpop, rpop, xadd, xautoclaim, xautoclaimjustid, xclaim, xdel, xgroupcreate, xreadgroup}
    import version.streamtypes.*

    send(del("x:v", "x:none"))
    send(xgroupcreate("x:v", "grp", StreamFrom.latest, StreamMkStream))
    val first  = send(xadd("x:v", OneOrMore.unsafeFrom(List(Key("a") -> 1))))
    val second = send(xadd("x:v", OneOrMore.unsafeFrom(List(Key("a") -> 2))))
    val third  = send(xadd("x:v", OneOrMore.unsafeFrom(List(Key("a") -> 3))))
    send(xreadgroup[Int]("grp", "c1", OneOrMore.unsafeFrom(List(Key("x:v") -> StreamGroupFrom.newEntries))))
    send(xdel("x:v", OneOrMore.unsafeFrom(List(second))))

    assertEquals(send(xclaim[Int]("x:v", "grp", "c2", 0L, OneOrMore.unsafeFrom(List(first)))).map(_.id), Seq(first))
    val auto = send(xautoclaim[Int]("x:v", "grp", "c3", 0L, StreamId(0L, 0L)))
    assertEquals(auto.claimed.map(_.id), Seq(first, third))
    assertEquals(auto.deleted, Seq(second))
    assertEquals(send(xautoclaimjustid("x:v", "grp", "c4", 0L, StreamId(0L, 0L))).claimed, Seq(first, third))

    assertEquals(send(lpop[String]("x:none", 1)), None)
    assertEquals(send(rpop[String]("x:none", 1)), None)

    val default = send(acl.getuser("default")).getOrElse(fail("no default user"))
    assert(default.keys.startsWith("~"), default.keys)
    assertEquals(default.selectors, Seq.empty)
    assertEquals(send(acl.getuser("ld-none")), None)
  }

  test(s"a $dest client adds to a stream, reads it by range and from ids, and trims it") {
    import laserdisc.redis_6_2.*
    import streamtypes.*

    send(del("x:s", "x:none"))
    val first  = send(xadd("x:s", OneOrMore.unsafeFrom(List(Key("a") -> 1, Key("b") -> 2))))
    val second = send(xadd("x:s", OneOrMore.unsafeFrom(List(Key("a") -> 3))))
    val third  = send(xadd("x:s", StreamTrim.maxLenApprox(100L), OneOrMore.unsafeFrom(List(Key("a") -> 4))))
    assertEquals(send(xadd("x:none", StreamNoMkStream, OneOrMore.unsafeFrom(List(Key("a") -> 1)))), None)
    assertEquals(send(xlen("x:s")).value, 3L)

    assertEquals(send(xrange[Int]("x:s", StreamBound.min, StreamBound.max)).map(_.id), Seq(first, second, third))
    assertEquals(send(xrange[Int]("x:s", StreamBound.min, StreamBound.max, 1L)).map(_.fields), Seq(Seq(Key("a") -> 1, Key("b") -> 2)))
    assertEquals(send(xrange[Int]("x:s", StreamBound.exclusive(first), StreamBound.inclusive(second))).map(_.id), Seq(second))
    assertEquals(send(xrevrange[Int]("x:s", StreamBound.max, StreamBound.min, 1L)).map(_.id), Seq(third))

    val read = send(xread[Int](OneOrMore.unsafeFrom(List(Key("x:s") -> StreamFrom.after(second)))))
    assertEquals(read.map(_.map(s => s.key -> s.value.map(_.id))), Some(Seq(Key("x:s") -> Seq(third))))
    assertEquals(send(xread[Int](OneOrMore.unsafeFrom(List(Key("x:s") -> StreamFrom.after(third))))), None)

    assertEquals(send(xdel("x:s", OneOrMore.unsafeFrom(List(first)))).value, 1L)
    assertEquals(send(xtrim("x:s", StreamTrim.maxLen(1L))).value, 1L)
    assertEquals(send(xtrim("x:s", StreamTrim.minId(third))).value, 0L)
    assertEquals(send(xrange[Int]("x:s", StreamBound.min, StreamBound.max)).map(_.id), Seq(third))

    val fourth = send(xadd("x:s", StreamTrim.maxLenExact(1L), OneOrMore.unsafeFrom(List(Key("a") -> 5))))
    assertEquals(send(xrange[Int]("x:s", StreamBound.min, StreamBound.max)).map(_.id), Seq(fourth))
    assertEquals(send(xtrim("x:s", StreamTrim.minIdExact(fourth))).value, 0L)
  }

  test(s"a $dest client reads a stream as a consumer group, acknowledges entries and finds the pending ones") {
    import laserdisc.redis_6_2.*
    import streamtypes.*

    send(del("x:g"))
    assertEquals(send(xgroupcreate("x:g", "grp", StreamFrom.latest, StreamMkStream)), OK)
    val first  = send(xadd("x:g", OneOrMore.unsafeFrom(List(Key("a") -> 1))))
    val second = send(xadd("x:g", OneOrMore.unsafeFrom(List(Key("a") -> 2))))
    assert(send(xgroupcreateconsumer("x:g", "grp", "idle")))

    val fresh = OneOrMore.unsafeFrom(List(Key("x:g") -> StreamGroupFrom.newEntries))
    assertEquals(send(xreadgroup[Int]("grp", "c1", fresh, 5L)).map(_.flatMap(_.value.map(_.id))), Some(Seq(first, second)))
    assertEquals(send(xreadgroup[Int]("grp", "c1", fresh)), None)

    val summary = send(xpending("x:g", "grp"))
    assertEquals(summary.count.value, 2L)
    assertEquals(summary.consumers, Seq(Key("c1") -> NonNegLong(2L)))
    assertEquals(send(xpending("x:g", "grp", StreamBound.min, StreamBound.max, 10L, "c1")).map(_.id), Seq(first, second))

    send(xdel("x:g", OneOrMore.unsafeFrom(List(first))))
    val history = OneOrMore.unsafeFrom(List(Key("x:g") -> StreamGroupFrom.pendingAfter(StreamId(0L, 0L))))
    assertEquals(send(xreadgroup[Int]("grp", "c1", history)).map(_.flatMap(_.value.map(_.fields.isEmpty))), Some(Seq(true, false)))

    assertEquals(send(xack("x:g", "grp", OneOrMore.unsafeFrom(List(first, second)))).value, 2L)
    assertEquals(send(xpending("x:g", "grp")).count.value, 0L)
    assertEquals(send(xgroupdelconsumer("x:g", "grp", "c1")).value, 0L)
    assertEquals(send(xgroupsetid("x:g", "grp", StreamFrom.latest)), OK)
    assert(send(xgroupdestroy("x:g", "grp")))
  }

  test(s"a $dest client claims pending entries and reads the stream's, groups' and consumers' information") {
    import laserdisc.redis_6_2.*
    import streamtypes.*

    send(del("x:c"))
    send(xgroupcreate("x:c", "grp", StreamFrom.latest, StreamMkStream))
    val first  = send(xadd("x:c", OneOrMore.unsafeFrom(List(Key("a") -> 1))))
    val second = send(xadd("x:c", OneOrMore.unsafeFrom(List(Key("a") -> 2))))
    val third  = send(xadd("x:c", OneOrMore.unsafeFrom(List(Key("a") -> 3))))
    send(xreadgroup[Int]("grp", "c1", OneOrMore.unsafeFrom(List(Key("x:c") -> StreamGroupFrom.newEntries))))
    send(xdel("x:c", OneOrMore.unsafeFrom(List(second))))

    assertEquals(send(xclaim[Int]("x:c", "grp", "c2", 0L, OneOrMore.unsafeFrom(List(first, second)))).map(_.id), Seq(first))
    assertEquals(
      send(xclaimjustid("x:c", "grp", "c2", 0L, OneOrMore.unsafeFrom(List(third)), StreamClaimOptions.none.retryCount(5L))),
      Seq(third)
    )
    val auto = send(xautoclaim[Int]("x:c", "grp", "c3", 0L, StreamId(0L, 0L), 10L))
    assertEquals(auto.claimed.map(_.id), Seq(first, third))
    assertEquals(auto.next, StreamId(0L, 0L))

    val info = send(xinfostream[Int]("x:c"))
    assertEquals(info.length.value, 2L)
    assertEquals(info.lastGeneratedId, third)
    assertEquals(info.firstEntry.map(_.id), Some(first))
    // from Redis 7.0, claiming drops a deleted entry from the pending list; Redis 6.2 (and KeyDB) keeps it
    val pending = imported match {
      case _: Redis7_0 => 2L
      case _           => 3L
    }
    assertEquals(send(xinfogroups("x:c")).map(g => g.name -> g.pending.value), Seq(Key("grp") -> pending))
    assertEquals(
      send(xinfoconsumers("x:c", "grp")).map(c => c.name -> c.pending.value).toSet,
      Set(Key("c1") -> 0L, Key("c2") -> 0L, Key("c3") -> pending)
    )

    val full = send(xinfostreamfull[Int]("x:c"))
    assertEquals(full.length.value, 2L)
    assertEquals(full.lastGeneratedId, third)
    assertEquals(full.entries.map(e => e.id -> e.fields), Seq(first -> Seq(Key("a") -> 1), third -> Seq(Key("a") -> 3)))
    assertEquals(send(xinfostreamfull[Int]("x:c", 1L)).entries.map(_.id), Seq(first))
    val fullGroup = full.groups.map(g => g.name -> g.pendingCount.value)
    assertEquals(fullGroup, Seq(Key("grp") -> pending))
    assertEquals(full.groups.flatMap(_.pending.flatMap(_.consumer)).toSet, Set(Key("c3")))
    assertEquals(
      full.groups.flatMap(_.consumers.map(c => c.name -> c.pending.size)).toSet,
      Set(Key("c1") -> 0, Key("c2") -> 0, Key("c3") -> pending.toInt)
    )

    assertEquals(send(xsetid("x:c", StreamId(9999999999999L, 0L))), OK)
  }

  test(s"a $dest client waits for new entries with the blocking reads, and gets None when the wait runs out") {
    import laserdisc.redis_6_2.*
    import streamtypes.*

    send(del("x:b"))
    send(xgroupcreate("x:b", "grp", StreamFrom.latest, StreamMkStream))
    val latest = OneOrMore.unsafeFrom(List(Key("x:b") -> StreamFrom.latest))
    assertEquals(send(blocking.xread[Int](latest, 100L)), None)
    assertEquals(
      send(blocking.xreadgroup[Int]("grp", "c", OneOrMore.unsafeFrom(List(Key("x:b") -> StreamGroupFrom.newEntries)), 100L)),
      None
    )

    val added = send(xadd("x:b", OneOrMore.unsafeFrom(List(Key("a") -> 1))))
    val all   = OneOrMore.unsafeFrom(List(Key("x:b") -> StreamFrom.after(StreamId(0L, 0L))))
    assertEquals(send(blocking.xread[Int](all, 1000L, 10L)).map(_.flatMap(_.value.map(_.id))), Some(Seq(added)))
    assertEquals(
      send(blocking.xreadgroup[Int]("grp", "c", OneOrMore.unsafeFrom(List(Key("x:b") -> StreamGroupFrom.newEntries)), 1000L, StreamNoAck))
        .map(_.flatMap(_.value.map(_.id))),
      Some(Seq(added))
    )
    assertEquals(send(xpending("x:b", "grp")).count.value, 0L)
  }
}
