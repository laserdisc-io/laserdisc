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

final class RedisSortedSetClientSpec extends SortedSetClientSpec(LiveServers.port(6379), "redis") {
  since[Redis7_2]("a redis client counts, pops and ranks") { v =>
    import v.*

    send(del("ss:x", "ss:y"))
    send(zadd("ss:x", "a", 1.0, "b", 2.0, "c", 3.0))
    send(zadd("ss:y", "b", 10.0, "c", 20.0, "d", 30.0))

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("ss:x"), Key("ss:y")))
    assertEquals(send(zintercard(ks)).value, 2)
    assertEquals(send(zintercard(ks, 1)).value, 1)
    assertEquals(send(zrankwithscore("ss:y", "c")).map { case (rank, score) => rank.value -> score }, Some(1 -> 20.0))
    assertEquals(send(zrevrankwithscore("ss:y", "zz")), None)

    val x = OneOrMoreKeys.unsafeFrom(List(Key("ss:x")))
    assertEquals(send(zmpop[String](x, sortedsettypes.pop.min)), Some(KV(Key("ss:x"), Seq("a" -> 1.0))))
    assertEquals(send(blocking.bzmpop[String](x, sortedsettypes.pop.max, 2, 1.0)), Some(KV(Key("ss:x"), Seq("c" -> 3.0, "b" -> 2.0))))
    assertEquals(send(zmpop[String](x, sortedsettypes.pop.min, 1)), None)
  }

  since[Redis8_8]("a redis client counts in how many sets each member is") { v =>
    import v.*

    send(del("ss:x", "ss:y"))
    send(zadd("ss:x", "a", 1.0, "b", 2.0, "c", 3.0))
    send(zadd("ss:y", "b", 10.0, "c", 20.0, "d", 30.0))

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("ss:x"), Key("ss:y")))
    assertEquals(send(zinterwithscores[String](ks, sortedsettypes.aggregate.count)), Seq("b" -> 2.0, "c" -> 2.0))
  }
}

final class KeyDbSortedSetClientSpec extends SortedSetClientSpec(LiveServers.port(6380), "keyDb")

final class ValkeySortedSetClientSpec extends SortedSetClientSpec(LiveServers.port(6381), "valkey") {
  since[Valkey8_0]("a valkey client scans without scores and pops") { v =>
    import v.*

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
    assertEquals(send(blocking.bzpopmax[String](b, 1.0)), Some(KV(Key("ss:b"), "m" -> 5.0)))
    assertEquals(send(blocking.bzpopmax[String](b, 0.1)), None)
  }

  test(s"a $dest client reads ranges of scores and of names with one ZRANGE") {
    import laserdisc.redis_6_2.*
    import sortedsettypes.{lexrange, scorerange}

    send(del("ss:r", "ss:rl"))
    send(zadd("ss:r", "a", 1.0, "b", 2.0, "c", 3.0))
    val scores = scorerange.closed(1.0, 3.0)

    assertEquals(send(zrange[String]("ss:r", scorerange.closed(1.0, 2.0))), Seq("a", "b"))
    assertEquals(send(zrange[String]("ss:r", scores, 1L, 1L)), Seq("b"))
    assertEquals(send(zrangewithscores[String]("ss:r", scorerange.closed(1.0, 2.0))), Seq("a" -> 1.0, "b" -> 2.0))
    assertEquals(send(zrangewithscores[String]("ss:r", scores, 2L, 5L)), Seq("c" -> 3.0))
    assertEquals(send(zrevrange[String]("ss:r", scores)), Seq("c", "b", "a"))
    assertEquals(send(zrevrange[String]("ss:r", scores, 0L, 1L)), Seq("c"))
    assertEquals(send(zrevrangewithscores[String]("ss:r", scores)), Seq("c" -> 3.0, "b" -> 2.0, "a" -> 1.0))
    assertEquals(send(zrevrangewithscores[String]("ss:r", scores, 0L, 1L)), Seq("c" -> 3.0))

    send(zadd("ss:rl", "a", 0.0, "b", 0.0, "c", 0.0))
    val names = lexrange.closed("a", "b")
    assertEquals(send(zrange[String]("ss:rl", names)), Seq("a", "b"))
    assertEquals(send(zrange[String]("ss:rl", names, 1L, 1L)), Seq("b"))
    assertEquals(send(zrevrange[String]("ss:rl", names)), Seq("b", "a"))
    assertEquals(send(zrevrange[String]("ss:rl", names, 0L, 1L)), Seq("b"))
    send(del("ss:r", "ss:rl"))
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

  test(s"a $dest client counts, ranks, increments and removes") {
    import laserdisc.redis_6_2.*
    import sortedsettypes.{flag, lexrange, scorerange}

    send(del("ss:c", "ss:n"))
    assertEquals(send(zcard("ss:c")), None)
    assertEquals(send(zadd("ss:c", "a", 1.0, "b", 2.0, "c", 3.0)).value, 3)
    assertEquals(send(zcard("ss:c")).map(_.value), Some(3))
    assertEquals(send(zaddch("ss:c", "a", 1.5, "d", 4.0)).value, 2)
    assertEquals(send(zaddch("ss:c", flag.xx, "a", 1.0, "z", 9.0)).value, 1)
    assertEquals(send(zaddchincr("ss:c", "d", 1.0)), 5.0)
    assertEquals(send(zaddchincr("ss:c", flag.nx, "d", 1.0)), None)

    val middle = scorerange.closed(2.0, 3.0)
    assertEquals(send(zcount("ss:c", middle)).value, 2)
    assertEquals(send(zincrby("ss:c", "a", 0.5)), 1.5)
    assertEquals(send(zrank("ss:c", "a")).map(_.value), Some(0))
    assertEquals(send(zrevrank("ss:c", "a")).map(_.value), Some(3))
    assertEquals(send(zrank("ss:c", "z")), None)

    assertEquals(send(zrangebyscore[String]("ss:c", middle)), Seq("b", "c"))
    assertEquals(send(zrangebyscore[String]("ss:c", middle, 1L, 1L)), Seq("c"))
    assertEquals(send(zrevrangebyscore[String]("ss:c", middle)), Seq("c", "b"))
    assertEquals(send(zrevrangebyscore[String]("ss:c", middle, 1L, 1L)), Seq("b"))
    assertEquals(send(zrangebyscorewithscores[String]("ss:c", middle)), Seq("b" -> 2.0, "c" -> 3.0))
    assertEquals(send(zrangebyscorewithscores[String]("ss:c", middle, 0L, 1L)), Seq("b" -> 2.0))
    assertEquals(send(zrevrangebyscorewithscores[String]("ss:c", middle)), Seq("c" -> 3.0, "b" -> 2.0))
    assertEquals(send(zrevrangebyscorewithscores[String]("ss:c", middle, 0L, 1L)), Seq("c" -> 3.0))
    assertEquals(send(zrandmembers[String]("ss:c", 2)).size, 2)

    assertEquals(send(zrem("ss:c", "a", "z")).value, 1)
    assertEquals(send(zremrangebyscore("ss:c", scorerange.closed(2.0, 2.0))).value, 1)
    assertEquals(send(zremrangebyrank("ss:c", 0L, 0L)).value, 1)

    send(zadd("ss:n", "a", 0.0, "b", 0.0, "c", 0.0))
    val names = lexrange.closed("a", "b")
    assertEquals(send(zlexcount("ss:n", names)).value, 2)
    assertEquals(send(zrangebylex[String]("ss:n", names)), Seq("a", "b"))
    assertEquals(send(zrangebylex[String]("ss:n", names, 1L, 1L)), Seq("b"))
    assertEquals(send(zrevrangebylex[String]("ss:n", names)), Seq("b", "a"))
    assertEquals(send(zrevrangebylex[String]("ss:n", names, 1L, 1L)), Seq("a"))
    assertEquals(send(zremrangebylex("ss:n", names)).value, 2)
  }

  test(s"a $dest client stores combined sorted sets with weights and an aggregate") {
    import laserdisc.redis_6_2.*
    import sortedsettypes.aggregate

    send(del("ss:p", "ss:q", "ss:d"))
    send(zadd("ss:p", "a", 1.0, "b", 2.0))
    send(zadd("ss:q", "b", 10.0, "c", 20.0))

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("ss:p"), Key("ss:q")))
    val w  = TwoOrMoreWeightedKeys.unsafeFrom(List(Key("ss:p") -> ValidDouble(2.0), Key("ss:q") -> ValidDouble(1.0)))

    assertEquals(send(zinterstore(ks, "ss:d")).value, 1)
    assertEquals(send(zscore("ss:d", "b")), Some(12.0))
    assertEquals(send(zinterstore(ks, "ss:d", aggregate.max)).value, 1)
    assertEquals(send(zscore("ss:d", "b")), Some(10.0))
    assertEquals(send(zinterstoreweighted(w, "ss:d")).value, 1)
    assertEquals(send(zscore("ss:d", "b")), Some(14.0))
    assertEquals(send(zinterstoreweighted(w, "ss:d", aggregate.min)).value, 1)
    assertEquals(send(zscore("ss:d", "b")), Some(4.0))

    assertEquals(send(zunionstore(ks, "ss:d")).value, 3)
    assertEquals(send(zunionstore(ks, "ss:d", aggregate.max)).value, 3)
    assertEquals(send(zunionstoreweighted(w, "ss:d")).value, 3)
    assertEquals(send(zscore("ss:d", "a")), Some(2.0))
    assertEquals(send(zunionstoreweighted(w, "ss:d", aggregate.max)).value, 3)
    assertEquals(send(zscore("ss:d", "b")), Some(10.0))

    assertEquals(send(zinterweighted[String](w)), Seq("b"))
    assertEquals(send(zinterweighted[String](w, aggregate.max)), Seq("b"))
    assertEquals(send(zunionweighted[String](w)), Seq("a", "b", "c"))
    assertEquals(send(zunionweighted[String](w, aggregate.min)), Seq("a", "b", "c"))
    assertEquals(send(zunionwithscores[String](ks)), Seq("a" -> 1.0, "b" -> 12.0, "c" -> 20.0))
    assertEquals(send(zunionwithscores[String](ks, aggregate.max)), Seq("a" -> 1.0, "b" -> 10.0, "c" -> 20.0))
  }
}
