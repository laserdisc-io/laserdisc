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

  test("redis_6_2 has GEOSEARCH and GEOADD with NX, but not BYPOLYGON") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*
    import geotypes.*

    val area = GeoArea.radius(GeoCenter.member("m"), 10.5, GeoUnit.kilometers)
    assertEquals(
      geosearch("a", area, GeoCount.any(2)).frames,
      frame("GEOSEARCH", "a", "FROMMEMBER", "m", "BYRADIUS", "10.5", "km", "COUNT", "2", "ANY")
    )
    assertEquals(
      geosearchstore("a", "b", area, GeoStoreDistances).frames,
      frame("GEOSEARCHSTORE", "b", "a", "FROMMEMBER", "m", "BYRADIUS", "10.5", "km", "STOREDIST")
    )
    assert(
      compileErrors("""geosearch("a", GeoPolygon(GeoCoordinates(1.0, 1.0), GeoCoordinates(1.0, 2.0), GeoCoordinates(2.0, 2.0)))""").nonEmpty
    )
  }

  test("valkey_9_0 has GEOSEARCH with BYPOLYGON") {
    import laserdisc.auto.*
    import laserdisc.valkey_9_0.*
    import geotypes.*

    val triangle = GeoPolygon(GeoCoordinates(1.5, 1.5), GeoCoordinates(1.5, 2.5), GeoCoordinates(2.5, 2.5))
    assertEquals(geosearch("a", triangle).frames, frame("GEOSEARCH", "a", "BYPOLYGON", "3", "1.5", "1.5", "2.5", "1.5", "2.5", "2.5"))
    assertEquals(
      geosearchstore("a", "b", triangle, Direction.asc).frames,
      frame("GEOSEARCHSTORE", "b", "a", "BYPOLYGON", "3", "1.5", "1.5", "2.5", "1.5", "2.5", "2.5", "ASC")
    )
  }

  test("redis_6_2 has COPY, SCAN with TYPE and SORT with GET, but not EXPIRE with a condition, EXPIRETIME, SORT_RO or WAITAOF") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*
    import keytypes.*

    assertEquals(copy("a", "b", KeyReplace).frames, frame("COPY", "a", "b", "REPLACE"))
    assertEquals(scan(0L, KeyType.zset).frames, frame("SCAN", "0", "TYPE", "zset"))
    assertEquals(sort[String]("a", KeySort.alpha, KeySortGet("#")).frames, frame("SORT", "a", "ALPHA", "GET", "#"))
    assert(compileErrors("""expire("a", 10, KeyExpireCondition.nx)""").nonEmpty)
    assert(compileErrors("""expiretime("a")""").nonEmpty)
    assert(compileErrors("""sortro[String]("a", KeySort.alpha)""").nonEmpty)
    assert(compileErrors("""waitaof(0, 1, 100L)""").nonEmpty)
  }

  test("redis_7_0 has EXPIRE with a condition, EXPIRETIME and SORT_RO, but not WAITAOF") {
    import laserdisc.auto.*
    import laserdisc.redis_7_0.*
    import keytypes.*

    assertEquals(pexpireat("a", 100L, KeyExpireCondition.lt).frames, frame("PEXPIREAT", "a", "100", "LT"))
    assertEquals(expiretime("a").frames, frame("EXPIRETIME", "a"))
    assertEquals(sortro[String]("a", KeySort.desc).frames, frame("SORT_RO", "a", "DESC"))
    assert(compileErrors("""waitaof(0, 1, 100L)""").nonEmpty)
  }

  test("valkey_7_2 has WAITAOF") {
    import laserdisc.auto.*
    import laserdisc.valkey_7_2.*

    assertEquals(waitaof(0, 1, 100L).frames, frame("WAITAOF", "0", "1", "100"))
  }

  test("redis_7_2 has HRANDFIELD and HSET with several fields, but no field expiry or HSCAN with NOVALUES") {
    import laserdisc.auto.*
    import laserdisc.redis_7_2.*

    val fs = OneOrMoreKeys.unsafeFrom(List(Key("f")))
    assertEquals(hrandfields("a", 2).frames, frame("HRANDFIELD", "a", "2"))
    assertEquals(hset("a", OneOrMore.unsafeFrom(List(Key("f") -> 1))).frames, frame("HSET", "a", "f", "1"))
    assert(compileErrors("""httl("a", fs)""").nonEmpty)
    assert(compileErrors("""hscannovalues("a", 0L)""").nonEmpty)
  }

  test("redis_7_4 has field expiry and HSCAN with NOVALUES, but not HGETEX, HSETEX or HGETDEL") {
    import laserdisc.auto.*
    import laserdisc.redis_7_4.*
    import hashtypes.*

    val fs = OneOrMoreKeys.unsafeFrom(List(Key("f")))
    assertEquals(hexpire("a", 10L, HashExpireCondition.nx, fs).frames, frame("HEXPIRE", "a", "10", "NX", "FIELDS", "1", "f"))
    assertEquals(hscannovalues("a", 0L).frames, frame("HSCAN", "a", "0", "NOVALUES"))
    assert(compileErrors("""hgetex[Int]("a", fs)""").nonEmpty)
    assert(compileErrors("""hgetdel[Int]("a", fs)""").nonEmpty)
  }

  test("valkey_9_0 has HGETEX and HSETEX, but not HGETDEL or HSETEX with NX") {
    import laserdisc.auto.*
    import laserdisc.valkey_9_0.*
    import hashtypes.*

    val fs  = OneOrMoreKeys.unsafeFrom(List(Key("f")))
    val fvs = OneOrMore.unsafeFrom(List(Key("f") -> 1))
    assertEquals(hgetex[Int]("a", strings.expiry.persist, fs).frames, frame("HGETEX", "a", "PERSIST", "FIELDS", "1", "f"))
    assertEquals(hsetex("a", HashFieldsCondition.fxx, fvs).frames, frame("HSETEX", "a", "FXX", "FIELDS", "1", "f", "1"))
    assert(compileErrors("""hgetdel[Int]("a", fs)""").nonEmpty)
    assert(compileErrors("""hsetex("a", HashKeyCondition.nx, fvs)""").nonEmpty)
  }

  test("valkey_9_1 has HGETDEL and HSETEX with NX, and redis_8_10 has HGETDEL but not HSETEX with NX") {
    import laserdisc.auto.*

    val fs  = OneOrMoreKeys.unsafeFrom(List(Key("f")))
    val fvs = OneOrMore.unsafeFrom(List(Key("f") -> 1))

    {
      import laserdisc.valkey_9_1.*
      import hashtypes.*
      assertEquals(hgetdel[Int]("a", fs).frames, frame("HGETDEL", "a", "FIELDS", "1", "f"))
      assertEquals(hsetex("a", HashKeyCondition.nx, fvs).frames, frame("HSETEX", "a", "NX", "FIELDS", "1", "f", "1"))
    }
    {
      import laserdisc.redis_8_10.*
      import hashtypes.*
      assertEquals(hgetdel[Int]("a", fs).frames, frame("HGETDEL", "a", "FIELDS", "1", "f"))
      assert(compileErrors("""hsetex("a", HashKeyCondition.nx, fvs)""").nonEmpty)
    }
  }

  test("redis_6_2 has BITFIELD and BITFIELD_RO, but not LCS, BITCOUNT with BIT or BITOP with DIFF") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*

    val bf = strings.bitfield
    assertEquals(bitfield("a", bf.incrBy(bf.u(8), bf.field(1L), 5L)).frames, frame("BITFIELD", "a", "INCRBY", "u8", "#1", "5"))
    assertEquals(bitfieldro("a", OneOrMore.unsafeFrom(List(bf.i(64) -> bf.bit(0L)))).frames, frame("BITFIELD_RO", "a", "GET", "i64", "0"))
    assert(compileErrors("""bf.u(64)""").nonEmpty)
    assert(compileErrors("""lcs[String]("a", "b")""").nonEmpty)
    assert(compileErrors("""bitcount("a", 0L, 7L, strings.bitUnit.bit)""").nonEmpty)
    assert(compileErrors("""bitop(strings.bitwise.diff, TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))), "c")""").nonEmpty)
  }

  test("valkey_9_1 has LCS and BITCOUNT with BIT, but not BITOP with DIFF") {
    import laserdisc.auto.*
    import laserdisc.valkey_9_1.*

    assertEquals(lcslen("a", "b").frames, frame("LCS", "a", "b", "LEN"))
    assertEquals(bitcount("a", 0L, 7L, strings.bitUnit.bit).frames, frame("BITCOUNT", "a", "0", "7", "BIT"))
    assert(compileErrors("""bitop(strings.bitwise.diff, TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))), "c")""").nonEmpty)
  }

  test("redis_8_2 has BITOP with DIFF, DIFF1, ANDOR and ONE") {
    import laserdisc.auto.*
    import laserdisc.redis_8_2.*

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))
    assertEquals(bitop(strings.bitwise.andor, ks, "c").frames, frame("BITOP", "ANDOR", "c", "a", "b"))
    assertEquals(bitop(strings.bitwise.and, ks, "c").frames, frame("BITOP", "AND", "c", "a", "b"))
  }

  test("redis_8_4 has DIGEST, DELEX and MSETEX, but not INCREX or DELIFEQ") {
    import laserdisc.auto.*
    import laserdisc.redis_8_4.*

    assertEquals(delex("a", strings.compare.ifDeq("abc")).frames, frame("DELEX", "a", "IFDEQ", "abc"))
    assertEquals(digest("a").frames, frame("DIGEST", "a"))
    assertEquals(
      msetex(OneOrMore.unsafeFrom(List(Key("a") -> 1)), strings.expiry.keepTtl).frames,
      frame("MSETEX", "1", "a", "1", "KEEPTTL")
    )
    assert(compileErrors("""increx("a")""").nonEmpty)
    assert(compileErrors("""delifeq("a", 1)""").nonEmpty)
  }

  test("redis_8_8 has INCREX, and its expiry can't be KEEPTTL or ENX with PERSIST") {
    import laserdisc.auto.*
    import laserdisc.redis_8_8.*

    assertEquals(increx("a", 2L, strings.increx.int.expiry(strings.expiry.persist)).frames, frame("INCREX", "a", "BYINT", "2", "PERSIST"))
    assert(compileErrors("""strings.increx.int.expiry(strings.expiry.keepTtl)""").nonEmpty)
    assert(compileErrors("""strings.increx.int.expiryIfNone(strings.expiry.persist)""").nonEmpty)
    assert(compileErrors("""increx("a", 2L, strings.increx.float)""").nonEmpty)
  }

  test("valkey_9_0 has DELIFEQ, but not MSETEX, DELEX or DIGEST, and valkey_9_1 has MSETEX") {
    import laserdisc.auto.*

    val kvs = OneOrMore.unsafeFrom(List(Key("a") -> 1))

    {
      import laserdisc.valkey_9_0.*
      assertEquals(delifeq("a", 1).frames, frame("DELIFEQ", "a", "1"))
      assert(compileErrors("""msetex(kvs)""").nonEmpty)
      assert(compileErrors("""delex("a")""").nonEmpty)
      assert(compileErrors("""digest("a")""").nonEmpty)
    }
    {
      import laserdisc.valkey_9_1.*
      assertEquals(msetex(kvs, strings.flag.xx).frames, frame("MSETEX", "1", "a", "1", "XX"))
    }
  }

  test("redis_6_2 has CLIENT UNPAUSE, FAILOVER and FLUSHALL SYNC, but not CLIENT NO-EVICT, SHUTDOWN NOW or several CONFIG parameters") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*

    assertEquals(client.unpause.frames, frame("CLIENT", "UNPAUSE"))
    assertEquals(replicafailover.frames, frame("FAILOVER"))
    assertEquals(flushallsync.frames, frame("FLUSHALL", "SYNC"))
    assert(compileErrors("""client.noevict(on = true)""").nonEmpty)
    assert(compileErrors("""shutdown(servers.shutdownOptions.now)""").nonEmpty)
    assert(compileErrors("""config.set(OneOrMore.unsafeFrom(List(Key("a") -> 1)))""").nonEmpty)
    assert(compileErrors("""info(OneOrMore.unsafeFrom(List(servers.info.cpu)))""").nonEmpty)
  }

  test("redis_7_0 has CLIENT NO-EVICT, SHUTDOWN NOW and several CONFIG parameters, but not CLIENT NO-TOUCH") {
    import laserdisc.auto.*
    import laserdisc.redis_7_0.*

    assertEquals(client.noevict(on = true).frames, frame("CLIENT", "NO-EVICT", "ON"))
    assertEquals(shutdownabort.frames, frame("SHUTDOWN", "ABORT"))
    assertEquals(config.set(OneOrMore.unsafeFrom(List(Key("a") -> 1))).frames, frame("CONFIG", "SET", "a", "1"))
    assert(compileErrors("""client.notouch(on = true)""").nonEmpty)
  }

  test("only Redis 7.4 and Valkey 8.0 and later have CLIENT KILL with MAXAGE, and only Valkey 8.1 and later BGSAVE CANCEL") {
    import laserdisc.auto.*

    {
      import laserdisc.valkey_7_2.*
      assertEquals(client.notouch(on = true).frames, frame("CLIENT", "NO-TOUCH", "ON"))
      assert(compileErrors("""client.kill(servers.clientFilter.id(1L), 10L)""").nonEmpty)
    }
    {
      import laserdisc.valkey_8_0.*
      assertEquals(client.kill(servers.clientFilter.id(1L), 10L).frames, frame("CLIENT", "KILL", "ID", "1", "MAXAGE", "10"))
      assert(compileErrors("""bgsavecancel""").nonEmpty)
    }
    {
      import laserdisc.valkey_8_1.*
      assertEquals(bgsavecancel.frames, frame("BGSAVE", "CANCEL"))
    }
    {
      import laserdisc.redis_8_10.*
      assertEquals(client.kill(servers.clientFilter.id(1L), 10L).frames, frame("CLIENT", "KILL", "ID", "1", "MAXAGE", "10"))
      assert(compileErrors("""bgsavecancel""").nonEmpty)
    }
  }

  test("each version can ask INFO only for the sections it has") {
    import laserdisc.auto.*

    {
      import laserdisc.redis_6_2.*
      assertEquals(info(servers.info.errorstats).frames, frame("INFO", "errorstats"))
      assert(compileErrors("""info(servers.info.latencystats)""").nonEmpty)
    }
    {
      import laserdisc.redis_7_4.*
      assertEquals(info(servers.info.latencystats).frames, frame("INFO", "latencystats"))
      assert(compileErrors("""info(servers.info.keysizes)""").nonEmpty)
    }
    {
      import laserdisc.redis_8_4.*
      assertEquals(info(servers.info.threads).frames, frame("INFO", "threads"))
      assert(compileErrors("""info(servers.info.hotkeys)""").nonEmpty)
    }
    {
      import laserdisc.redis_8_10.*
      assertEquals(info(servers.info.hotkeys).frames, frame("INFO", "hotkeys"))
      assert(compileErrors("""info(servers.info.tls)""").nonEmpty)
    }
    {
      import laserdisc.valkey_9_1.*
      assertEquals(info(servers.info.tls).frames, frame("INFO", "tls"))
      assert(compileErrors("""info(servers.info.keysizes)""").nonEmpty)
    }
  }

  test("redis_6_2 has SLOWLOG, MEMORY, LATENCY and COMMAND INFO, but not LATENCY HISTOGRAM, COMMAND LIST, HOTKEYS or COMMANDLOG") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*

    assertEquals(slowlog.len.frames, frame("SLOWLOG", "LEN"))
    assertEquals(memory.usage("a").frames, frame("MEMORY", "USAGE", "a"))
    assertEquals(latency.latest.frames, frame("LATENCY", "LATEST"))
    assertEquals(command.count.frames, frame("COMMAND", "COUNT"))
    assert(compileErrors("""latency.histogram""").nonEmpty)
    assert(compileErrors("""command.list""").nonEmpty)
    assert(compileErrors("""hotkeys.stop""").nonEmpty)
    assert(compileErrors("""commandlog.len(servers.commandLogType.slow)""").nonEmpty)
  }

  test("redis_7_0 has LATENCY HISTOGRAM and COMMAND LIST, redis_8_6 has HOTKEYS, and valkey_8_1 has COMMANDLOG") {
    import laserdisc.auto.*

    {
      import laserdisc.redis_7_0.*
      assertEquals(latency.histogram.frames, frame("LATENCY", "HISTOGRAM"))
      assertEquals(command.list.frames, frame("COMMAND", "LIST"))
    }
    {
      import laserdisc.redis_8_6.*
      assertEquals(hotkeys.reset.frames, frame("HOTKEYS", "RESET"))
      assert(compileErrors("""commandlog.len(servers.commandLogType.slow)""").nonEmpty)
    }
    {
      import laserdisc.valkey_8_1.*
      assertEquals(commandlog.len(servers.commandLogType.largeRequest).frames, frame("COMMANDLOG", "LEN", "large-request"))
      assert(compileErrors("""hotkeys.stop""").nonEmpty)
    }
  }

  test("each version has the cluster commands its server has") {
    import laserdisc.auto.*

    val ranges = OneOrMore.unsafeFrom(List(Slot(0) -> Slot(10)))

    {
      import laserdisc.redis_6_2.*
      assertEquals(myid.frames, frame("CLUSTER", "MYID"))
      assert(compileErrors("""shards""").nonEmpty)
      assert(compileErrors("""addslotsrange(ranges)""").nonEmpty)
    }
    {
      import laserdisc.redis_7_0.*
      assertEquals(addslotsrange(ranges).frames, frame("CLUSTER", "ADDSLOTSRANGE", "0", "10"))
      assert(compileErrors("""myshardid""").nonEmpty)
    }
    {
      import laserdisc.redis_8_2.*
      assertEquals(myshardid.frames, frame("CLUSTER", "MYSHARDID"))
      assertEquals(slotstats(Slot(0), Slot(1)).frames, frame("CLUSTER", "SLOT-STATS", "SLOTSRANGE", "0", "1"))
      assert(compileErrors("""trimslots(ranges)""").nonEmpty)
    }
    {
      import laserdisc.redis_8_10.*
      assertEquals(trimslots(ranges).frames, frame("TRIMSLOTS", "RANGES", "1", "0", "10"))
      assertEquals(backup.status.frames, frame("BACKUP", "STATUS"))
      assert(compileErrors("""replicatenoone""").nonEmpty)
      assert(compileErrors("""flushslot(Slot(0))""").nonEmpty)
    }
    {
      import laserdisc.valkey_8_0.*
      assertEquals(setslot(Slot(0), NonNegLong(5L)).frames, frame("CLUSTER", "SETSLOT", "0", "STABLE", "TIMEOUT", "5"))
      assert(compileErrors("""replicatenoone""").nonEmpty)
    }
    {
      import laserdisc.valkey_9_1.*
      assertEquals(replicatenoone.frames, frame("CLUSTER", "REPLICATE", "NO", "ONE"))
      assertEquals(flushslot(Slot(0)).frames, frame("CLUSTER", "FLUSHSLOT", "0"))
      assert(compileErrors("""trimslots(ranges)""").nonEmpty)
      assert(compileErrors("""backup.status""").nonEmpty)
    }
  }

  test("redis_6_2 has ACL, but not ACL DRYRUN or rules with selectors, which redis_7_0 has") {
    import laserdisc.auto.*

    {
      import laserdisc.redis_6_2.*
      assertEquals(acl.setuser("u", servers.aclRules.on).frames, frame("ACL", "SETUSER", "u", "on"))
      assert(compileErrors("""acl.dryrun("u", ping)""").nonEmpty)
      assert(compileErrors("""acl.setuser("u", servers.aclSelectorRules.on)""").nonEmpty)
    }
    {
      import laserdisc.redis_7_0.*
      assertEquals(acl.dryrun("u", ping).frames, frame("ACL", "DRYRUN", "u", "PING"))
      assertEquals(acl.setuser("u", servers.aclSelectorRules.writeKeys("w:*")).frames, frame("ACL", "SETUSER", "u", "%W~w:*"))
    }
  }
}
