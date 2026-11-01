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

final case class NoShow(i: Int)

final class VersionImportsSpec extends BaseSpec {
  private def frame(ps: String*): Seq[RESP] = Seq(Arr(ps.map(Bulk(_)).toList))

  test("all has every SET variant") {
    import laserdisc.all.*
    import laserdisc.auto.*

    assertEquals(set("a", 2).frames, frame("SET", "a", "2"))
    assertEquals(set("a", 2, strings.expiry.seconds(10L)).frames, frame("SET", "a", "2", "EX", "10"))
    assertEquals(set("a", 2, strings.expiry.keepTtl).frames, frame("SET", "a", "2", "KEEPTTL"))
    assertEquals(set("a", 2, strings.flag.nx).frames, frame("SET", "a", "2", "NX"))
    assertEquals(set("a", 2, strings.expiry.millis(5L), strings.flag.xx).frames, frame("SET", "a", "2", "XX", "PX", "5"))
    assertEquals(set("a", 2, strings.ifEq(1)).frames, frame("SET", "a", "2", "IFEQ", "1"))
    assertEquals(set("a", 2, strings.expiry.atSeconds(99L), strings.ifEq(1)).frames, frame("SET", "a", "2", "IFEQ", "1", "EXAT", "99"))
    assertEquals(set("a", 2, strings.compare.ifNe(1)).frames, frame("SET", "a", "2", "IFNE", "1"))
    assertEquals(set("a", 2, strings.compare.ifDeq("abc")).frames, frame("SET", "a", "2", "IFDEQ", "abc"))
  }

  test("all has setget, getset, getdel and getex, choosing the type of the reply") {
    import laserdisc.all.*
    import laserdisc.auto.*

    assertEquals(setget[Int]("a", 2).frames, frame("SET", "a", "2", "GET"))
    assertEquals(setget[Int]("a", "two", strings.expiry.seconds(10L)).frames, frame("SET", "a", "two", "GET", "EX", "10"))
    assertEquals(setget[Int]("a", 2, strings.flag.nx).frames, frame("SET", "a", "2", "NX", "GET"))
    assertEquals(setget[Int]("a", 2, strings.expiry.keepTtl, strings.flag.xx).frames, frame("SET", "a", "2", "XX", "GET", "KEEPTTL"))
    assertEquals(setget[Int]("a", 2).decode(Bulk(1)), Some(1))
    assertEquals(setget[Int]("a", 2).decode(NullBulk), None)
    assertEquals(getset[Int]("a", 2).frames, frame("GETSET", "a", "2"))
    assertEquals(getdel[Int]("a").frames, frame("GETDEL", "a"))
    assertEquals(getex[Int]("a", strings.expiry.persist).frames, frame("GETEX", "a", "PERSIST"))
    assertEquals(getex[Int]("a", strings.expiry.atMillis(7L)).decode(Bulk(1)), Some(1))
  }

  test("SET's KEEPTTL and GETEX's PERSIST only go with their own command") {
    import laserdisc.all.*
    import laserdisc.auto.*

    assertEquals(getex[Int]("a").frames, frame("GETEX", "a"))
    assert(compileErrors("""getex[Int]("a", strings.expiry.keepTtl)""").nonEmpty)
    assert(compileErrors("""set("a", 2, strings.expiry.persist)""").nonEmpty)
  }

  test("a value without a Show gives laserdisc's message for set, and a type mismatch for setget") {
    import laserdisc.all.*
    import laserdisc.auto.*

    assertEquals(set("a", 2).frames, frame("SET", "a", "2"))
    assert(compileErrors("""set("a", NoShow(1))""").contains("Show"))
    assert(compileErrors("""setget[Int]("a", NoShow(1))""").nonEmpty)
  }

  test("redis_6_2 has setget, but not with NX or XX") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*

    assertEquals(setget[Int]("a", 2).frames, frame("SET", "a", "2", "GET"))
    assert(compileErrors("""setget[Int]("a", 2, strings.flag.nx)""").nonEmpty)
  }

  test("redis_8_2 has setget with NX, but no IFEQ or IFNE") {
    import laserdisc.auto.*
    import laserdisc.redis_8_2.*

    assertEquals(setget[Int]("a", 2, strings.flag.nx).frames, frame("SET", "a", "2", "NX", "GET"))
    assert(compileErrors("""set("a", 2, strings.ifEq(1))""").nonEmpty)
    assert(compileErrors("""set("a", 2, strings.compare.ifNe(1))""").nonEmpty)
  }

  test("redis_8_4 has IFEQ and IFNE") {
    import laserdisc.auto.*
    import laserdisc.redis_8_4.*

    assertEquals(set("a", 2, strings.ifEq(1)).frames, frame("SET", "a", "2", "IFEQ", "1"))
    assertEquals(set("a", 2, strings.compare.ifNe(1)).frames, frame("SET", "a", "2", "IFNE", "1"))
  }

  test("valkey_8_1 has IFEQ, but no IFNE") {
    import laserdisc.auto.*
    import laserdisc.valkey_8_1.*

    assertEquals(set("a", 2, strings.ifEq(1)).frames, frame("SET", "a", "2", "IFEQ", "1"))
    assert(compileErrors("""set("a", 2, strings.compare.ifNe(1))""").nonEmpty)
  }

  test("a version's options can be used next to import laserdisc.*") {
    import laserdisc.*
    import laserdisc.auto.*
    import laserdisc.valkey_8_1.*

    assertEquals(set(Key("a"), 2, strings.ifEq(1)).frames, frame("SET", "a", "2", "IFEQ", "1"))
  }

  test("valkey_8_0 has no IFEQ") {
    import laserdisc.auto.*
    import laserdisc.valkey_8_0.*

    assertEquals(set("a", 2).frames, frame("SET", "a", "2"))
    assert(compileErrors("""set("a", 2, strings.ifEq(1))""").nonEmpty)
  }

  test("redis_6_2 has ZADD with GT and LT and BZPOPMIN, but not ZINTERCARD, ZMPOP or BZMPOP") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*

    assertEquals(zadd("a", sortedsets.flag.xxGt, "m", 1.5).frames, frame("ZADD", "a", "XX", "GT", "1.5", "m"))
    assertEquals(blocking.bzpopmin[String](OneOrMoreKeys.unsafeFrom(List(Key("a"))), 1).frames, frame("BZPOPMIN", "a", "1"))
    assert(compileErrors("""zintercard(TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))))""").nonEmpty)
    assert(compileErrors("""zmpop[String](OneOrMoreKeys.unsafeFrom(List(Key("a"))), sortedsets.pop.min)""").nonEmpty)
    assert(compileErrors("""blocking.bzmpop[String](OneOrMoreKeys.unsafeFrom(List(Key("a"))), sortedsets.pop.min, 1)""").nonEmpty)
  }

  test("redis_7_0 has ZINTERCARD, ZMPOP and BZMPOP, but not ZRANK with WITHSCORE") {
    import laserdisc.auto.*
    import laserdisc.redis_7_0.*

    val a = OneOrMoreKeys.unsafeFrom(List(Key("a")))
    assertEquals(zintercard(TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))), 5).frames, frame("ZINTERCARD", "2", "a", "b", "LIMIT", "5"))
    assertEquals(zmpop[String](a, sortedsets.pop.min).frames, frame("ZMPOP", "1", "a", "MIN"))
    assertEquals(blocking.bzmpop[String](a, sortedsets.pop.max, 2, 1).frames, frame("BZMPOP", "1", "1", "a", "MAX", "COUNT", "2"))
    assertEquals(blocking.bzpopmin[String](a, 1).frames, frame("BZPOPMIN", "a", "1"))
    assert(compileErrors("""zrankwithscore("a", "m")""").nonEmpty)
  }

  test("only Redis 8.8 and later have AGGREGATE COUNT") {
    import laserdisc.auto.*

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))

    {
      import laserdisc.redis_8_8.*
      assertEquals(zunionstore(ks, "c", sortedsets.aggregate.count).frames, frame("ZUNIONSTORE", "c", "2", "a", "b", "AGGREGATE", "COUNT"))
      assertEquals(zunion[String](ks, sortedsets.aggregate.max).frames, frame("ZUNION", "2", "a", "b", "AGGREGATE", "MAX"))
    }
    {
      import laserdisc.redis_8_6.*
      assert(compileErrors("""zinter[String](ks, sortedsets.aggregate.count)""").nonEmpty)
    }
    {
      import laserdisc.valkey_9_1.*
      assert(compileErrors("""zinter[String](ks, sortedsets.aggregate.count)""").nonEmpty)
    }
  }

  test("only Valkey 8.0 and later have ZSCAN with NOSCORES") {
    import laserdisc.auto.*

    {
      import laserdisc.valkey_8_0.*
      assertEquals(zscannoscores[String]("a", 0L, 10).frames, frame("ZSCAN", "a", "0", "COUNT", "10", "NOSCORES"))
    }
    {
      import laserdisc.valkey_7_2.*
      assert(compileErrors("""zscannoscores[String]("a", 0L)""").nonEmpty)
    }
    {
      import laserdisc.redis_8_10.*
      assert(compileErrors("""zscannoscores[String]("a", 0L)""").nonEmpty)
    }
  }

  test("all has every sorted set command, with the blocking ones under blocking") {
    import laserdisc.all.*
    import laserdisc.auto.*

    val a = OneOrMoreKeys.unsafeFrom(List(Key("a")))
    assertEquals(zscannoscores[String]("a", 0L).frames, frame("ZSCAN", "a", "0", "NOSCORES"))
    assertEquals(zrevrankwithscore("a", "m").frames, frame("ZREVRANK", "a", "m", "WITHSCORE"))
    assertEquals(blocking.bzmpop[String](a, sortedsets.pop.min, 0).frames, frame("BZMPOP", "0", "1", "a", "MIN"))
    assertEquals(blocking.blpop[String](a, 0).frames, frame("BLPOP", "a", "0"))
  }

  test("redis_6_2 has LMOVE, LPOS and BLMOVE, but not LMPOP, BLMPOP or LMOVEM") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*
    import listtypes.*

    assertEquals(lmove[String]("a", "b", ListSide.left, ListSide.right).frames, frame("LMOVE", "a", "b", "LEFT", "RIGHT"))
    assertEquals(lpos("a", "x", 2, ListSearch.rank(-1)).frames, frame("LPOS", "a", "x", "COUNT", "2", "RANK", "-1"))
    assertEquals(
      blocking.blmove[String]("a", "b", ListSide.right, ListSide.left, 1).frames,
      frame("BLMOVE", "a", "b", "RIGHT", "LEFT", "1")
    )
    assert(compileErrors("""lmpop[String](OneOrMoreKeys.unsafeFrom(List(Key("a"))), ListSide.left)""").nonEmpty)
    assert(compileErrors("""blocking.blmpop[String](OneOrMoreKeys.unsafeFrom(List(Key("a"))), ListSide.left, 1)""").nonEmpty)
    assert(compileErrors("""lmovem[String]("a", "b", ListSide.left, ListSide.left, ListAmount.upTo(2), ListOrder.asBlock)""").nonEmpty)
  }

  test("valkey_9_1 has LMPOP and BLMPOP, but not LMOVEM or BLMOVEM") {
    import laserdisc.auto.*
    import laserdisc.valkey_9_1.*
    import listtypes.*

    val a = OneOrMoreKeys.unsafeFrom(List(Key("a")))
    assertEquals(lmpop[String](a, ListSide.left, 2).frames, frame("LMPOP", "1", "a", "LEFT", "COUNT", "2"))
    assertEquals(blocking.blmpop[String](a, ListSide.right, 3).frames, frame("BLMPOP", "3", "1", "a", "RIGHT"))
    assert(compileErrors("""lmovem[String]("a", "b", ListSide.left, ListSide.left, ListAmount.upTo(2), ListOrder.asBlock)""").nonEmpty)
    assert(
      compileErrors(
        """blocking.blmovem[String]("a", "b", ListSide.left, ListSide.left, ListAmount.upTo(2), ListOrder.asBlock, 1)"""
      ).nonEmpty
    )
  }

  test("redis_8_10 has LMOVEM and BLMOVEM") {
    import laserdisc.auto.*
    import laserdisc.redis_8_10.*
    import listtypes.*

    assertEquals(
      lmovem[String]("a", "b", ListSide.left, ListSide.left, ListAmount.upTo(2), ListOrder.oneByOne).frames,
      frame("LMOVEM", "a", "b", "LEFT", "LEFT", "COUNT", "2", "OBO")
    )
    assertEquals(
      blocking.blmovem[String]("a", "b", ListSide.left, ListSide.right, ListAmount.exactly(2), ListOrder.asBlock, 1).frames,
      frame("BLMOVEM", "a", "b", "LEFT", "RIGHT", "1", "EXACTLY", "2", "BULK")
    )
  }

  test("redis_6_2 has SMISMEMBER, but not SINTERCARD") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*

    assertEquals(smismember("a", OneOrMore.unsafeFrom(List("x", "y"))).frames, frame("SMISMEMBER", "a", "x", "y"))
    assert(compileErrors("""sintercard(TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))))""").nonEmpty)
  }

  test("valkey_9_1 has SINTERCARD, but not SDIFFCARD or SUNIONCARD") {
    import laserdisc.auto.*
    import laserdisc.valkey_9_1.*

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))
    assertEquals(sintercard(ks, 3).frames, frame("SINTERCARD", "2", "a", "b", "LIMIT", "3"))
    assert(compileErrors("""sdiffcard(ks)""").nonEmpty)
    assert(compileErrors("""sunioncard(ks, sets.approx)""").nonEmpty)
  }

  test("redis_8_10 has SDIFFCARD and SUNIONCARD") {
    import laserdisc.auto.*
    import laserdisc.redis_8_10.*

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))
    assertEquals(sdiffcard(ks, 2).frames, frame("SDIFFCARD", "2", "a", "b", "LIMIT", "2"))
    assertEquals(sunioncard(ks, sets.approx).frames, frame("SUNIONCARD", "2", "a", "b", "APPROX"))
  }
}
