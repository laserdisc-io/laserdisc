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

final class RedisLoopClientSpec extends LoopClientSpec(6379, "redis") {
  test("a redis client scans a hash's fields alone with the loops of Redis 8.10") {
    import laserdisc.redis_8_10.*

    send(del("l:n"))
    send(hset("l:n", OneOrMore.unsafeFrom((1 to 20).toList.map(i => Key.unsafeFrom(s"f$i") -> i))))
    assertEquals(run(hscannovaluesAll("l:n", 5)).toSet, (1 to 20).map(i => Key.unsafeFrom(s"f$i")).toSet)
  }
}
final class KeyDbLoopClientSpec  extends LoopClientSpec(6380, "keyDb")
final class ValkeyLoopClientSpec extends LoopClientSpec(6381, "valkey") {
  test("a valkey client scans hash fields and sorted set members alone with the loops of Valkey 9.1") {
    import laserdisc.valkey_9_1.*

    send(del("l:n", "l:m"))
    send(hset("l:n", OneOrMore.unsafeFrom((1 to 20).toList.map(i => Key.unsafeFrom(s"f$i") -> i))))
    (1 to 20).foreach(i => send(zadd("l:m", s"m$i", ValidDouble.unsafeFrom(i.toDouble))))
    assertEquals(run(hscannovaluesAll("l:n", 5)).toSet, (1 to 20).map(i => Key.unsafeFrom(s"f$i")).toSet)
    assertEquals(run(zscannoscoresAll[String]("l:m", 5)).toSet, (1 to 20).map(i => s"m$i").toSet)
  }
}

final class DragonflyLoopClientSpec extends LoopClientSpec(6382, "dragonfly")

abstract class LoopClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  protected final def run[A](loop: Loop[A], take: Long = Long.MaxValue): List[A] =
    client.stream(loop).take(take).compile.toList.unsafeRunSync()(using runtime)

  test(s"a $dest client scans whole keyspaces, hashes, sets and sorted sets with the cursor loops") {
    import laserdisc.redis_6_2.*

    val members = (1 to 50).toList.map(i => s"m$i")
    send(del("l:h", "l:s", "l:z"))
    members.foreach(m => send(set(Key.unsafeFrom(s"l:k:$m"), m)))
    send(hset("l:h", OneOrMore.unsafeFrom(members.map(m => Key.unsafeFrom(m) -> m))))
    send(sadd("l:s", OneOrMore.unsafeFrom(members)))
    members.zipWithIndex.foreach { case (m, i) => send(zadd("l:z", m, ValidDouble.unsafeFrom(i.toDouble))) }

    assertEquals(run(scanAll("l:k:*", 10)).distinct.size, 50)
    assertEquals(run(hscanAll("l:h", 10)).map(_.value).toSet, members.toSet)
    assertEquals(run(sscanAll[String]("l:s", 10)).toSet, members.toSet)
    assertEquals(run(zscanAll[String]("l:z", "m1*")).map(_._1).toSet, members.filter(_.startsWith("m1")).toSet)
  }

  test(s"a $dest client pages through a stream with xrangeAll and xrevrangeAll") {
    import laserdisc.redis_6_2.*

    send(del("l:x"))
    val ids = (1 to 7).toList.map(i => send(xadd("l:x", OneOrMore.unsafeFrom(List(Key("a") -> i)))))
    assertEquals(run(xrangeAll[Int]("l:x", 3L)).map(_.id), ids)
    assertEquals(run(xrangeAll[Int]("l:x", 7L)).map(_.id), ids)
    assertEquals(run(xrevrangeAll[Int]("l:x", 3L)).map(_.id), ids.reverse)
  }

  test(s"a $dest client follows streams with xreadFollow and xreadgroupFollow") {
    import laserdisc.redis_6_2.*
    import streamtypes.*

    send(del("l:f"))
    send(xgroupcreate("l:f", "grp", StreamFrom.latest, StreamMkStream))
    val ids = (1 to 3).toList.map(i => send(xadd("l:f", OneOrMore.unsafeFrom(List(Key("a") -> i)))))

    val fromStart = OneOrMore.unsafeFrom(List(Key("l:f") -> StreamFrom.after(StreamId(0L, 0L))))
    assertEquals(run(blocking.xreadFollow[Int](fromStart, 100L, 2L), take = 3).map(_.value.id), ids)

    // the consumer's history first, which is empty, then the new entries
    val history = OneOrMore.unsafeFrom(List(Key("l:f") -> StreamGroupFrom.pendingAfter(StreamId(0L, 0L))))
    assertEquals(run(blocking.xreadgroupFollow[Int]("grp", "c", history, 100L), take = 3).map(_.value.id), ids)
    // now they're pending, so the history has them
    assertEquals(run(blocking.xreadgroupFollow[Int]("grp", "c", history, 100L, 1L), take = 3).map(_.value.id), ids)
  }
}
