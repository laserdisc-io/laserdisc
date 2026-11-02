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

final class RedisStreamClientSpec  extends StreamClientSpec(6379, "redis")
final class KeyDbStreamClientSpec  extends StreamClientSpec(6380, "keyDb")
final class ValkeyStreamClientSpec extends StreamClientSpec(6381, "valkey")

abstract class StreamClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

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
}
