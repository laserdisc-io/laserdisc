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
}
