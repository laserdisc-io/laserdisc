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

final class RedisSortedSetClientSpec extends SortedSetClientSpec(6379, "redis") {
  test("a redis client counts, pops and ranks with the commands of Redis 8.10") {
    import laserdisc.redis_8_10.*

    send(del("ss:x", "ss:y"))
    send(zadd("ss:x", "a", 1.0, "b", 2.0, "c", 3.0))
    send(zadd("ss:y", "b", 10.0, "c", 20.0, "d", 30.0))

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("ss:x"), Key("ss:y")))
    assertEquals(send(zintercard(ks)).value, 2)
    assertEquals(send(zintercard(ks, 1)).value, 1)
    assertEquals(send(zinterwithscores[String](ks, sortedsettypes.aggregate.count)), Seq("b" -> 2.0, "c" -> 2.0))
    assertEquals(send(zrankwithscore("ss:y", "c")).map { case (rank, score) => rank.value -> score }, Some(1 -> 20.0))
    assertEquals(send(zrevrankwithscore("ss:y", "zz")), None)

    val x = OneOrMoreKeys.unsafeFrom(List(Key("ss:x")))
    assertEquals(send(zmpop[String](x, sortedsettypes.pop.min)), Some(KV(Key("ss:x"), Seq("a" -> 1.0))))
    assertEquals(send(blocking.bzmpop[String](x, sortedsettypes.pop.max, 2, 1)), Some(KV(Key("ss:x"), Seq("c" -> 3.0, "b" -> 2.0))))
    assertEquals(send(zmpop[String](x, sortedsettypes.pop.min, 1)), None)
  }
}

final class KeyDbSortedSetClientSpec extends SortedSetClientSpec(6380, "keyDb")

final class ValkeySortedSetClientSpec extends SortedSetClientSpec(6381, "valkey") {
  test("a valkey client scans without scores and pops with the commands of Valkey 9.1") {
    import laserdisc.valkey_9_1.*

    send(del("ss:x"))
    send(zadd("ss:x", "a", 1.0, "b", 2.0))

    assertEquals(send(zscannoscores[String]("ss:x", 0L)).values, Some(Seq("a", "b")))
    assertEquals(send(zrankwithscore("ss:x", "b")).map { case (rank, score) => rank.value -> score }, Some(1 -> 2.0))
    assertEquals(
      send(zmpop[String](OneOrMoreKeys.unsafeFrom(List(Key("ss:x"))), sortedsettypes.pop.max)),
      Some(KV(Key("ss:x"), Seq("b" -> 2.0)))
    )
  }
}

abstract class SortedSetClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  test(s"a $dest client adds with INCR, NX, XX, GT and LT") {
    import laserdisc.redis_6_2.*

    send(del("ss:a"))
    assertEquals(send(zadd("ss:a", "x", 1.0, "y", 2.0)).value, 2)
    assertEquals(send(zaddincr("ss:a", "x", 2.0)), 3.0)
    assertEquals(send(zaddincr("ss:a", sortedsettypes.flag.nx, "x", 1.0)), None)
    assertEquals(send(zaddincr("ss:a", sortedsettypes.flag.xx, "x", 1.0)), Some(4.0))
    assertEquals(send(zadd("ss:a", sortedsettypes.flag.gt, "y", 1.0)).value, 0)
    assertEquals(send(zscore("ss:a", "y")), Some(2.0))
    send(zadd("ss:a", sortedsettypes.flag.xxLt, "y", 1.0, "z", 1.0))
    assertEquals(send(zmscore("ss:a", OneOrMore.unsafeFrom(List("y", "z")))), Seq(Some(1.0), None))
  }

  test(s"a $dest client pops by score") {
    import laserdisc.redis_6_2.*

    send(del("ss:b"))
    send(zadd("ss:b", "a", 1.0, "b", 2.0, "c", 3.0))
    assertEquals(send(zpopmin[String]("ss:b")), Some("a" -> 1.0))
    assertEquals(send(zpopmax[String]("ss:b", 2)), Seq("c" -> 3.0, "b" -> 2.0))
    assertEquals(send(zpopmin[String]("ss:b")), None)

    send(zadd("ss:b", "m", 5.0))
    val b = OneOrMoreKeys.unsafeFrom(List(Key("ss:b")))
    assertEquals(send(blocking.bzpopmax[String](b, 1)), Some(KV(Key("ss:b"), "m" -> 5.0)))
  }

  test(s"a $dest client reads ranges with scores, and combines and stores sorted sets") {
    import laserdisc.redis_6_2.*

    send(del("ss:x", "ss:y", "ss:z", "ss:l"))
    send(zadd("ss:x", "a", 1.0, "b", 2.0, "c", 3.0))
    send(zadd("ss:y", "b", 10.0, "c", 20.0, "d", 30.0))
    send(zadd("ss:l", "a", 0.0, "b", 0.0, "c", 0.0))

    assertEquals(send(zrangewithscores[String]("ss:x", 0L, -1L)), Seq("a" -> 1.0, "b" -> 2.0, "c" -> 3.0))
    assertEquals(send(zrevrangewithscores[String]("ss:x", 0L, 0L)), Seq("c" -> 3.0))

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("ss:x"), Key("ss:y")))
    assertEquals(send(zdiff[String](ks)), Seq("a"))
    assertEquals(send(zdiffwithscores[String](ks)), Seq("a" -> 1.0))
    assertEquals(send(zinter[String](ks, sortedsettypes.aggregate.max)), Seq("b", "c"))
    assertEquals(send(zinterwithscores[String](ks)), Seq("b" -> 12.0, "c" -> 23.0))
    assertEquals(send(zunion[String](ks)), Seq("a", "b", "c", "d"))

    val weighted = TwoOrMoreWeightedKeys.unsafeFrom(List(Key("ss:x") -> ValidDouble(2.0), Key("ss:y") -> ValidDouble(1.0)))
    assertEquals(send(zunionweightedwithscores[String](weighted)), Seq("a" -> 2.0, "b" -> 14.0, "c" -> 26.0, "d" -> 30.0))

    assertEquals(send(zdiffstore(ks, "ss:z")).value, 1)
    assertEquals(send(zrangestore("ss:x", "ss:z", sortedsettypes.scorerange.closed(2.0, 3.0))).value, 2)
    assertEquals(send(zrange[String]("ss:z", 0L, -1L)), Seq("b", "c"))
    assertEquals(send(zrevrangestore("ss:x", "ss:z", 0L, 0L)).value, 1)
    assertEquals(send(zrange[String]("ss:z", 0L, -1L)), Seq("c"))
    assertEquals(send(zrevrangestore("ss:x", "ss:z", sortedsettypes.scorerange.closed(1.0, 3.0), 0L, 2L)).value, 2)
    assertEquals(send(zrange[String]("ss:z", 0L, -1L)), Seq("b", "c"))
    assertEquals(send(zrangestore("ss:l", "ss:z", sortedsettypes.lexrange.closed("a", "b"))).value, 2)
    assertEquals(send(zrevrangestore("ss:l", "ss:z", sortedsettypes.lexrange.closed("a", "b"), 0L, 1L)).value, 1)
    assertEquals(send(zrange[String]("ss:z", 0L, -1L)), Seq("b"))

    assert(send(zrandmember[String]("ss:x")).exists(Set("a", "b", "c")))
    assertEquals(send(zrandmemberswithscores[String]("ss:x", 3)).toSet, Set("a" -> 1.0, "b" -> 2.0, "c" -> 3.0))
  }
}
