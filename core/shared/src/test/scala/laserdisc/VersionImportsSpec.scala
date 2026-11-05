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

  test("redis_8_10 has every SET variant") {
    import laserdisc.redis_8_10.*
    import laserdisc.auto.*

    assertEquals(set("a", 2).frames, frame("SET", "a", "2"))
    assertEquals(set("a", 2, stringtypes.expiry.seconds(10L)).frames, frame("SET", "a", "2", "EX", "10"))
    assertEquals(set("a", 2, stringtypes.expiry.keepTtl).frames, frame("SET", "a", "2", "KEEPTTL"))
    assertEquals(set("a", 2, stringtypes.flag.nx).frames, frame("SET", "a", "2", "NX"))
    assertEquals(set("a", 2, stringtypes.expiry.millis(5L), stringtypes.flag.xx).frames, frame("SET", "a", "2", "XX", "PX", "5"))
    assertEquals(set("a", 2, stringtypes.ifEq(1)).frames, frame("SET", "a", "2", "IFEQ", "1"))
    assertEquals(
      set("a", 2, stringtypes.expiry.atSeconds(99L), stringtypes.ifEq(1)).frames,
      frame("SET", "a", "2", "IFEQ", "1", "EXAT", "99")
    )
    assertEquals(set("a", 2, stringtypes.compare.ifNe(1)).frames, frame("SET", "a", "2", "IFNE", "1"))
    assertEquals(set("a", 2, stringtypes.compare.ifDeq("abc")).frames, frame("SET", "a", "2", "IFDEQ", "abc"))
  }

  test("redis_8_10 has setget, getset, getdel and getex, choosing the type of the reply") {
    import laserdisc.redis_8_10.*
    import laserdisc.auto.*

    assertEquals(setget[Int]("a", 2).frames, frame("SET", "a", "2", "GET"))
    assertEquals(setget[Int]("a", "two", stringtypes.expiry.seconds(10L)).frames, frame("SET", "a", "two", "GET", "EX", "10"))
    assertEquals(setget[Int]("a", 2, stringtypes.flag.nx).frames, frame("SET", "a", "2", "NX", "GET"))
    assertEquals(
      setget[Int]("a", 2, stringtypes.expiry.keepTtl, stringtypes.flag.xx).frames,
      frame("SET", "a", "2", "XX", "GET", "KEEPTTL")
    )
    assertEquals(setget[Int]("a", 2).decode(Bulk(1)), Some(1))
    assertEquals(setget[Int]("a", 2).decode(NullBulk), None)
    assertEquals(getset[Int]("a", 2).frames, frame("GETSET", "a", "2"))
    assertEquals(getdel[Int]("a").frames, frame("GETDEL", "a"))
    assertEquals(getex[Int]("a", stringtypes.expiry.persist).frames, frame("GETEX", "a", "PERSIST"))
    assertEquals(getex[Int]("a", stringtypes.expiry.atMillis(7L)).decode(Bulk(1)), Some(1))
  }

  test("SET's KEEPTTL and GETEX's PERSIST only go with their own command") {
    import laserdisc.redis_8_10.*
    import laserdisc.auto.*

    assertEquals(getex[Int]("a").frames, frame("GETEX", "a"))
    assert(compileErrors("""getex[Int]("a", stringtypes.expiry.keepTtl)""").nonEmpty)
    assert(compileErrors("""set("a", 2, stringtypes.expiry.persist)""").nonEmpty)
  }

  test("a value without a Show gives laserdisc's message for set, and a type mismatch for setget") {
    import laserdisc.redis_8_10.*
    import laserdisc.auto.*

    assertEquals(set("a", 2).frames, frame("SET", "a", "2"))
    assert(compileErrors("""set("a", NoShow(1))""").contains("Show"))
    assert(compileErrors("""setget[Int]("a", NoShow(1))""").nonEmpty)
  }

  test("redis_6_2 has setget, but not with NX or XX") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*

    assertEquals(setget[Int]("a", 2).frames, frame("SET", "a", "2", "GET"))
    assert(compileErrors("""setget[Int]("a", 2, stringtypes.flag.nx)""").nonEmpty)
  }

  test("redis_8_2 has setget with NX, but no IFEQ or IFNE") {
    import laserdisc.auto.*
    import laserdisc.redis_8_2.*

    assertEquals(setget[Int]("a", 2, stringtypes.flag.nx).frames, frame("SET", "a", "2", "NX", "GET"))
    assert(compileErrors("""set("a", 2, stringtypes.ifEq(1))""").nonEmpty)
    assert(compileErrors("""set("a", 2, stringtypes.compare.ifNe(1))""").nonEmpty)
  }

  test("redis_8_4 has IFEQ and IFNE") {
    import laserdisc.auto.*
    import laserdisc.redis_8_4.*

    assertEquals(set("a", 2, stringtypes.ifEq(1)).frames, frame("SET", "a", "2", "IFEQ", "1"))
    assertEquals(set("a", 2, stringtypes.compare.ifNe(1)).frames, frame("SET", "a", "2", "IFNE", "1"))
  }

  test("valkey_8_1 has IFEQ, but no IFNE") {
    import laserdisc.auto.*
    import laserdisc.valkey_8_1.*

    assertEquals(set("a", 2, stringtypes.ifEq(1)).frames, frame("SET", "a", "2", "IFEQ", "1"))
    assert(compileErrors("""set("a", 2, stringtypes.compare.ifNe(1))""").nonEmpty)
  }

  test("a version's options can be used next to import laserdisc.*") {
    import laserdisc.*
    import laserdisc.valkey_8_1.*

    assertEquals(set(Key("a"), 2, stringtypes.ifEq(1)).frames, frame("SET", "a", "2", "IFEQ", "1"))
  }

  test("valkey_8_0 has no IFEQ") {
    import laserdisc.auto.*
    import laserdisc.valkey_8_0.*

    assertEquals(set("a", 2).frames, frame("SET", "a", "2"))
    assert(compileErrors("""set("a", 2, stringtypes.ifEq(1))""").nonEmpty)
  }

  test("redis_6_2 has ZADD with GT and LT and BZPOPMIN, but not ZINTERCARD, ZMPOP or BZMPOP") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*

    assertEquals(zadd("a", sortedsettypes.flag.xxGt, "m", 1.5).frames, frame("ZADD", "a", "XX", "GT", "1.5", "m"))
    assertEquals(blocking.bzpopmin[String](OneOrMoreKeys.unsafeFrom(List(Key("a"))), 1).frames, frame("BZPOPMIN", "a", "1"))
    assert(compileErrors("""zintercard(TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))))""").nonEmpty)
    assert(compileErrors("""zmpop[String](OneOrMoreKeys.unsafeFrom(List(Key("a"))), sortedsettypes.pop.min)""").nonEmpty)
    assert(compileErrors("""blocking.bzmpop[String](OneOrMoreKeys.unsafeFrom(List(Key("a"))), sortedsettypes.pop.min, 1)""").nonEmpty)
  }

  test("redis_7_0 has ZINTERCARD, ZMPOP and BZMPOP, but not ZRANK with WITHSCORE") {
    import laserdisc.auto.*
    import laserdisc.redis_7_0.*

    val a = OneOrMoreKeys.unsafeFrom(List(Key("a")))
    assertEquals(zintercard(TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))), 5).frames, frame("ZINTERCARD", "2", "a", "b", "LIMIT", "5"))
    assertEquals(zmpop[String](a, sortedsettypes.pop.min).frames, frame("ZMPOP", "1", "a", "MIN"))
    assertEquals(blocking.bzmpop[String](a, sortedsettypes.pop.max, 2, 1).frames, frame("BZMPOP", "1", "1", "a", "MAX", "COUNT", "2"))
    assertEquals(blocking.bzpopmin[String](a, 1).frames, frame("BZPOPMIN", "a", "1"))
    assert(compileErrors("""zrankwithscore("a", "m")""").nonEmpty)
  }

  test("only Redis 8.8 and later have AGGREGATE COUNT") {
    import laserdisc.auto.*

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))

    {
      import laserdisc.redis_8_8.*
      assertEquals(
        zunionstore(ks, "c", sortedsettypes.aggregate.count).frames,
        frame("ZUNIONSTORE", "c", "2", "a", "b", "AGGREGATE", "COUNT")
      )
      assertEquals(zunion[String](ks, sortedsettypes.aggregate.max).frames, frame("ZUNION", "2", "a", "b", "AGGREGATE", "MAX"))
    }
    {
      import laserdisc.redis_8_6.*
      assertEquals(zunion[String](ks, sortedsettypes.aggregate.max).frames, frame("ZUNION", "2", "a", "b", "AGGREGATE", "MAX"))
      assert(compileErrors("""zinter[String](ks, sortedsettypes.aggregate.count)""").nonEmpty)
    }
    {
      import laserdisc.valkey_9_1.*
      assertEquals(zunion[String](ks, sortedsettypes.aggregate.max).frames, frame("ZUNION", "2", "a", "b", "AGGREGATE", "MAX"))
      assert(compileErrors("""zinter[String](ks, sortedsettypes.aggregate.count)""").nonEmpty)
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
      assertEquals(zscan[String]("a", 0L).frames, frame("ZSCAN", "a", "0"))
      assert(compileErrors("""zscannoscores[String]("a", 0L)""").nonEmpty)
    }
    {
      import laserdisc.redis_8_10.*
      assertEquals(zscan[String]("a", 0L).frames, frame("ZSCAN", "a", "0"))
      assert(compileErrors("""zscannoscores[String]("a", 0L)""").nonEmpty)
    }
  }

  test("redis_8_10 has every sorted set command, with the blocking ones under blocking") {
    import laserdisc.redis_8_10.*
    import laserdisc.auto.*

    val a = OneOrMoreKeys.unsafeFrom(List(Key("a")))
    assertEquals(valkey_9_1.zscannoscores[String]("a", 0L).frames, frame("ZSCAN", "a", "0", "NOSCORES"))
    assertEquals(zrevrankwithscore("a", "m").frames, frame("ZREVRANK", "a", "m", "WITHSCORE"))
    assertEquals(blocking.bzmpop[String](a, sortedsettypes.pop.min, 0).frames, frame("BZMPOP", "0", "1", "a", "MIN"))
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
    assert(compileErrors("""sunioncard(ks, settypes.approx)""").nonEmpty)
  }

  test("redis_8_10 has SDIFFCARD and SUNIONCARD") {
    import laserdisc.auto.*
    import laserdisc.redis_8_10.*

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))
    assertEquals(sdiffcard(ks, 2).frames, frame("SDIFFCARD", "2", "a", "b", "LIMIT", "2"))
    assertEquals(sunioncard(ks, settypes.approx).frames, frame("SUNIONCARD", "2", "a", "b", "APPROX"))
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

    assertEquals(hrandfields("a", 2).frames, frame("HRANDFIELD", "a", "2"))
    assertEquals(hset("a", OneOrMore.unsafeFrom(List(Key("f") -> 1))).frames, frame("HSET", "a", "f", "1"))
    assert(compileErrors("""httl("a", OneOrMoreKeys.unsafeFrom(List(Key("f"))))""").nonEmpty)
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
    assertEquals(hgetex[Int]("a", stringtypes.expiry.persist, fs).frames, frame("HGETEX", "a", "PERSIST", "FIELDS", "1", "f"))
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
      assertEquals(hgetdel[Int]("a", fs).frames, frame("HGETDEL", "a", "FIELDS", "1", "f"))
      assert(compileErrors("""hsetex("a", hashtypes.HashKeyCondition.nx, fvs)""").nonEmpty)
    }
  }

  test("redis_6_2 has BITFIELD and BITFIELD_RO, but not LCS, BITCOUNT with BIT or BITOP with DIFF") {
    import laserdisc.auto.*
    import laserdisc.redis_6_2.*

    val bf = stringtypes.bitfield
    assertEquals(bitfield("a", bf.incrBy(bf.u(8), bf.field(1L), 5L)).frames, frame("BITFIELD", "a", "INCRBY", "u8", "#1", "5"))
    assertEquals(bitfieldro("a", OneOrMore.unsafeFrom(List(bf.i(64) -> bf.bit(0L)))).frames, frame("BITFIELD_RO", "a", "GET", "i64", "0"))
    assert(compileErrors("""bf.u(64)""").nonEmpty)
    assert(compileErrors("""lcs[String]("a", "b")""").nonEmpty)
    assert(compileErrors("""bitcount("a", 0L, 7L, stringtypes.bitUnit.bit)""").nonEmpty)
    assert(compileErrors("""bitop(stringtypes.bitwise.diff, TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))), "c")""").nonEmpty)
  }

  test("valkey_9_1 has LCS and BITCOUNT with BIT, but not BITOP with DIFF") {
    import laserdisc.auto.*
    import laserdisc.valkey_9_1.*

    assertEquals(lcslen("a", "b").frames, frame("LCS", "a", "b", "LEN"))
    assertEquals(bitcount("a", 0L, 7L, stringtypes.bitUnit.bit).frames, frame("BITCOUNT", "a", "0", "7", "BIT"))
    assert(compileErrors("""bitop(stringtypes.bitwise.diff, TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))), "c")""").nonEmpty)
  }

  test("redis_8_2 has BITOP with DIFF, DIFF1, ANDOR and ONE") {
    import laserdisc.auto.*
    import laserdisc.redis_8_2.*

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))
    assertEquals(bitop(stringtypes.bitwise.andor, ks, "c").frames, frame("BITOP", "ANDOR", "c", "a", "b"))
    assertEquals(bitop(stringtypes.bitwise.and, ks, "c").frames, frame("BITOP", "AND", "c", "a", "b"))
  }

  test("redis_8_4 has DIGEST, DELEX and MSETEX, but not INCREX or DELIFEQ") {
    import laserdisc.auto.*
    import laserdisc.redis_8_4.*

    assertEquals(delex("a", stringtypes.compare.ifDeq("abc")).frames, frame("DELEX", "a", "IFDEQ", "abc"))
    assertEquals(digest("a").frames, frame("DIGEST", "a"))
    assertEquals(
      msetex(OneOrMore.unsafeFrom(List(Key("a") -> 1)), stringtypes.expiry.keepTtl).frames,
      frame("MSETEX", "1", "a", "1", "KEEPTTL")
    )
    assert(compileErrors("""increx("a")""").nonEmpty)
    assert(compileErrors("""delifeq("a", 1)""").nonEmpty)
  }

  test("redis_8_8 has INCREX, and its expiry can't be KEEPTTL or ENX with PERSIST") {
    import laserdisc.auto.*
    import laserdisc.redis_8_8.*

    assertEquals(
      increx("a", 2L, stringtypes.increx.int.expiry(stringtypes.expiry.persist)).frames,
      frame("INCREX", "a", "BYINT", "2", "PERSIST")
    )
    assert(compileErrors("""stringtypes.increx.int.expiry(stringtypes.expiry.keepTtl)""").nonEmpty)
    assert(compileErrors("""stringtypes.increx.int.expiryIfNone(stringtypes.expiry.persist)""").nonEmpty)
    assert(compileErrors("""increx("a", 2L, stringtypes.increx.float)""").nonEmpty)
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
      assertEquals(msetex(kvs, stringtypes.flag.xx).frames, frame("MSETEX", "1", "a", "1", "XX"))
    }
  }

  test("redis_6_2 has CLIENT UNPAUSE, FAILOVER and FLUSHALL SYNC, but not CLIENT NO-EVICT, SHUTDOWN NOW or several CONFIG parameters") {
    import laserdisc.redis_6_2.*

    assertEquals(client.unpause.frames, frame("CLIENT", "UNPAUSE"))
    assertEquals(replicafailover.frames, frame("FAILOVER"))
    assertEquals(flushallsync.frames, frame("FLUSHALL", "SYNC"))
    assert(compileErrors("""client.noevict(on = true)""").nonEmpty)
    assert(compileErrors("""shutdown(servertypes.shutdownOptions.now)""").nonEmpty)
    assert(compileErrors("""config.set(OneOrMore.unsafeFrom(List(Key("a") -> 1)))""").nonEmpty)
    assert(compileErrors("""info(OneOrMore.unsafeFrom(List(servertypes.info.cpu)))""").nonEmpty)
  }

  test("redis_7_0 has CLIENT NO-EVICT, SHUTDOWN NOW and several CONFIG parameters, but not CLIENT NO-TOUCH") {
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
      assert(compileErrors("""client.kill(servertypes.clientFilter.id(1L), 10L)""").nonEmpty)
    }
    {
      import laserdisc.valkey_8_0.*
      assertEquals(client.kill(servertypes.clientFilter.id(1L), 10L).frames, frame("CLIENT", "KILL", "ID", "1", "MAXAGE", "10"))
      assert(compileErrors("""bgsavecancel""").nonEmpty)
    }
    {
      import laserdisc.valkey_8_1.*
      assertEquals(bgsavecancel.frames, frame("BGSAVE", "CANCEL"))
    }
    {
      import laserdisc.redis_8_10.*
      assertEquals(client.kill(servertypes.clientFilter.id(1L), 10L).frames, frame("CLIENT", "KILL", "ID", "1", "MAXAGE", "10"))
      assert(compileErrors("""bgsavecancel""").nonEmpty)
    }
  }

  test("each version can ask INFO only for the sections it has") {

    {
      import laserdisc.redis_6_2.*
      assertEquals(info(servertypes.info.errorstats).frames, frame("INFO", "errorstats"))
      assert(compileErrors("""info(servertypes.info.latencystats)""").nonEmpty)
    }
    {
      import laserdisc.redis_7_4.*
      assertEquals(info(servertypes.info.latencystats).frames, frame("INFO", "latencystats"))
      assert(compileErrors("""info(servertypes.info.keysizes)""").nonEmpty)
    }
    {
      import laserdisc.redis_8_4.*
      assertEquals(info(servertypes.info.threads).frames, frame("INFO", "threads"))
      assert(compileErrors("""info(servertypes.info.hotkeys)""").nonEmpty)
    }
    {
      import laserdisc.redis_8_10.*
      assertEquals(info(servertypes.info.hotkeys).frames, frame("INFO", "hotkeys"))
      assert(compileErrors("""info(servertypes.info.tls)""").nonEmpty)
    }
    {
      import laserdisc.valkey_9_1.*
      assertEquals(info(servertypes.info.tls).frames, frame("INFO", "tls"))
      assert(compileErrors("""info(servertypes.info.keysizes)""").nonEmpty)
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
    assert(compileErrors("""commandlog.len(servertypes.commandLogType.slow)""").nonEmpty)
  }

  test("redis_7_0 has LATENCY HISTOGRAM and COMMAND LIST, redis_8_6 has HOTKEYS, and valkey_8_1 has COMMANDLOG") {

    {
      import laserdisc.redis_7_0.*
      assertEquals(latency.histogram.frames, frame("LATENCY", "HISTOGRAM"))
      assertEquals(command.list.frames, frame("COMMAND", "LIST"))
    }
    {
      import laserdisc.redis_8_6.*
      assertEquals(hotkeys.reset.frames, frame("HOTKEYS", "RESET"))
      assert(compileErrors("""commandlog.len(servertypes.commandLogType.slow)""").nonEmpty)
    }
    {
      import laserdisc.valkey_8_1.*
      assertEquals(commandlog.len(servertypes.commandLogType.largeRequest).frames, frame("COMMANDLOG", "LEN", "large-request"))
      assert(compileErrors("""hotkeys.stop""").nonEmpty)
    }
  }

  test("each version has the cluster commands its server has") {

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
      assertEquals(acl.setuser("u", servertypes.aclRules.on).frames, frame("ACL", "SETUSER", "u", "on"))
      assert(compileErrors("""acl.dryrun("u", ping)""").nonEmpty)
      assert(compileErrors("""acl.setuser("u", servertypes.aclSelectorRules.on)""").nonEmpty)
    }
    {
      import laserdisc.redis_7_0.*
      assertEquals(acl.dryrun("u", ping).frames, frame("ACL", "DRYRUN", "u", "PING"))
      assertEquals(acl.setuser("u", servertypes.aclSelectorRules.writeKeys("w:*")).frames, frame("ACL", "SETUSER", "u", "%W~w:*"))
    }
  }

  test("redis_6_2 and redis_8_10.streams have the stream commands") {
    import laserdisc.auto.*

    val fields = OneOrMore.unsafeFrom(List(Key("a") -> 1))

    {
      import laserdisc.redis_6_2.*
      assertEquals(xadd("s", fields).frames, frame("XADD", "s", "*", "a", "1"))
      assertEquals(xlen("s").frames, frame("XLEN", "s"))
    }
    {
      import laserdisc.redis_8_10.streams.*
      import streamtypes.*
      assertEquals(xtrim("s", StreamTrim.maxLenApprox(10L)).frames, frame("XTRIM", "s", "MAXLEN", "~", "10"))
    }
  }

  test("redis_6_2 has consumer groups, but not ENTRIESREAD, which redis_7_0 has") {
    import laserdisc.auto.*

    {
      import laserdisc.redis_6_2.*
      import streamtypes.*
      assertEquals(xgroupcreate("s", "g", StreamFrom.latest).frames, frame("XGROUP", "CREATE", "s", "g", "$"))
      assert(compileErrors("""xgroupsetid("s", "g", StreamFrom.latest, 3L)""").nonEmpty)
    }
    {
      import laserdisc.redis_7_0.*
      import streamtypes.*
      assertEquals(xgroupsetid("s", "g", StreamFrom.latest, 3L).frames, frame("XGROUP", "SETID", "s", "g", "$", "ENTRIESREAD", "3"))
    }
  }

  test("redis_6_2 has XCLAIM, XAUTOCLAIM, XINFO and XSETID, but not XSETID with ENTRIESADDED, which redis_7_0 has") {
    import laserdisc.auto.*

    {
      import laserdisc.redis_6_2.*
      import streamtypes.*
      assertEquals(xsetid("s", StreamId(5L, 0L)).frames, frame("XSETID", "s", "5-0"))
      assertEquals(xinfogroups("s").frames, frame("XINFO", "GROUPS", "s"))
      assert(compileErrors("""xsetid("s", StreamId(5L, 0L), 1L, StreamId(4L, 0L))""").nonEmpty)
    }
    {
      import laserdisc.redis_7_0.*
      import streamtypes.*
      assertEquals(
        xsetid("s", StreamId(5L, 0L), 1L, StreamId(4L, 0L)).frames,
        frame("XSETID", "s", "5-0", "ENTRIESADDED", "1", "MAXDELETEDID", "4-0")
      )
    }
  }

  test("only Redis 8.2 and later have the stream reference policies, and only Redis 8.6 and later idempotent XADD") {
    import laserdisc.auto.*

    {
      import laserdisc.redis_8_2.*
      import streamtypes.*
      assertEquals(xtrim("s", StreamTrim.maxLen(1L), StreamRefPolicy.delRef).frames, frame("XTRIM", "s", "MAXLEN", "1", "DELREF"))
      assert(compileErrors("""xcfgset("s", StreamIdmpConfig.maxSize(10L))""").nonEmpty)
    }
    {
      import laserdisc.redis_8_10.*
      import streamtypes.*
      assertEquals(xcfgset("s", StreamIdmpConfig.maxSize(10L)).frames, frame("XCFGSET", "s", "IDMP-MAXSIZE", "10"))
    }
    {
      import laserdisc.valkey_9_1.*
      import streamtypes.*
      assertEquals(xtrim("s", StreamTrim.maxLen(1L)).frames, frame("XTRIM", "s", "MAXLEN", "1"))
      assert(compileErrors("""xtrim("s", StreamTrim.maxLen(1L), StreamRefPolicy.delRef)""").nonEmpty)
      assert(compileErrors("""xcfgset("s", StreamIdmpConfig.maxSize(10L))""").nonEmpty)
    }
  }

  test("only Redis 8.8 and later have XNACK") {
    import laserdisc.auto.*

    val ids = OneOrMore.unsafeFrom(List(protocol.StreamP.Id(NonNegLong(1L), NonNegLong(1L))))

    {
      import laserdisc.redis_8_8.*
      import streamtypes.*
      assertEquals(xnack("s", "g", StreamNackMode.fail, ids).frames, frame("XNACK", "s", "g", "FAIL", "IDS", "1", "1-1"))
    }
    {
      import laserdisc.redis_8_6.*
      import streamtypes.*
      assertEquals(xcfgset("s", StreamIdmpConfig.maxSize(10L)).frames, frame("XCFGSET", "s", "IDMP-MAXSIZE", "10"))
      assert(compileErrors("""xnack("s", "g", StreamNackMode.fail, ids)""").nonEmpty)
    }
    {
      import laserdisc.valkey_9_1.*
      import streamtypes.*
      assertEquals(xtrim("s", StreamTrim.maxLen(1L)).frames, frame("XTRIM", "s", "MAXLEN", "1"))
      assert(compileErrors("""xnack("s", "g", StreamNackMode.fail, ids)""").nonEmpty)
    }
  }

  test("only Redis 8.8 and later have arrays") {
    import laserdisc.auto.*

    {
      import laserdisc.redis_8_8.*
      assertEquals(arlen("a").frames, frame("ARLEN", "a"))
      assertEquals(arrays.arcount("a").frames, frame("ARCOUNT", "a"))
    }
    {
      import laserdisc.redis_8_6.*
      assertEquals(llen("a").frames, frame("LLEN", "a"))
      assert(compileErrors("""arlen("a")""").nonEmpty)
    }
    {
      import laserdisc.valkey_9_1.*
      assertEquals(llen("a").frames, frame("LLEN", "a"))
      assert(compileErrors("""arlen("a")""").nonEmpty)
    }
  }

  test("every version has the blocking stream reads under blocking") {
    import laserdisc.auto.*

    {
      import laserdisc.redis_6_2.*
      import streamtypes.*
      assertEquals(
        blocking.xread[Int](OneOrMore.unsafeFrom(List(Key("s") -> StreamFrom.latest)), 100L).frames,
        frame("XREAD", "BLOCK", "100", "STREAMS", "s", "$")
      )
    }
    {
      import laserdisc.redis_8_10.streams.*
      import streamtypes.*
      assertEquals(
        redis_8_10.blocking.xreadgroup[Int]("g", "c", OneOrMore.unsafeFrom(List(Key("s") -> StreamGroupFrom.newEntries)), 100L).frames,
        frame("XREADGROUP", "GROUP", "g", "c", "BLOCK", "100", "STREAMS", "s", ">")
      )
    }
  }

  test("every version has EVAL and SCRIPT, only Redis 7.0 and later have functions, and only Valkey 8.0 and later SCRIPT SHOW") {
    import laserdisc.auto.*

    val sha: Sha1 = Sha1("e0e1f9fabfc9d4800c877a703b823ac0578ff8db")

    {
      import laserdisc.redis_6_2.*
      assertEquals(eval[Num, Long]("return 1").frames, frame("EVAL", "return 1", "0"))
      assertEquals(script.load("return 1").frames, frame("SCRIPT", "LOAD", "return 1"))
      assert(compileErrors("""fcall[Num, Long]("f")""").nonEmpty)
      assert(compileErrors("""function.list""").nonEmpty)
    }
    {
      import laserdisc.redis_8_10.*
      assertEquals(fcall[Num, Long]("f").frames, frame("FCALL", "f", "0"))
      assertEquals(function.list.frames, frame("FUNCTION", "LIST"))
      assert(compileErrors("""script.show(sha)""").nonEmpty)
    }
    {
      import laserdisc.valkey_7_2.*
      assertEquals(fcallro[Num, Long]("f").frames, frame("FCALL_RO", "f", "0"))
      assert(compileErrors("""script.show(sha)""").nonEmpty)
    }
    {
      import laserdisc.valkey_8_0.*
      assertEquals(script.show(sha).frames, frame("SCRIPT", "SHOW", sha.value))
    }
    {
      import laserdisc.redis_8_10.scripting.*
      assertEquals(valkey_9_1.script.show(sha).frames, frame("SCRIPT", "SHOW", sha.value))
      assertEquals(evalro[Num, Long]("return 1").frames, frame("EVAL_RO", "return 1", "0"))
    }
  }

  test("a family of a version brings that family's commands, and no others") {
    import laserdisc.redis_8_10.streams.*
    assertEquals(xlen(Key("a")).command, "XLEN")
    assert(compileErrors("""get[String](Key("a"))""").nonEmpty)
  }

  test("a family of a version brings its options of that version") {
    import laserdisc.redis_8_4.strings.*
    assertEquals(set(Key("a"), 2, stringtypes.ifEq(1)).command, "SET")
  }

  test("a family of a version doesn't bring what arrived in a later version") {
    import laserdisc.redis_8_2.strings.*
    assertEquals(set(Key("a"), 2).command, "SET")
    assert(compileErrors("""set(Key("a"), 2, stringtypes.ifEq(1))""").nonEmpty)
  }

  test("a family of a version reads its replies as that version does") {
    val start                                      = protocol.StreamP.Id(NonNegLong(0L), NonNegLong(0L))
    val older                                      = Arr(Bulk("0-0"), Arr(Nil))
    def claim(v: protocol.StreamClaimAndInfoBaseP) = v.xautoclaim[Int](Key("s"), Key("g"), Key("c"), NonNegLong(0L), start)
    assertEquals(claim(redis_6_2.streams).decode(older).map(_.next), start)
    assert(claim(redis_7_0.streams).decode(older).isLeft)
    assert(redis_7_0.server.acl.getuser(Key("u")).decode(NilArr).isLeft)
  }

  test("a family of a version has that version's command groups") {
    assert(compileErrors("""redis_6_2.server.client.noevict(on = true)""").nonEmpty)
    assertEquals(redis_7_0.server.client.noevict(on = true).command, "CLIENT")
  }

  test("the command and the family called keys live together, and so do the ones called publish") {
    import laserdisc.redis_6_2.*
    assertEquals(keys(GlobPattern("a*")).command, "KEYS")
    assertEquals(publish(Key("c"), "m").command, "PUBLISH")
    assertEquals(redis_6_2.keys.del(Key("a")).command, "DEL")
    assertEquals(redis_6_2.publish.publish(Key("c"), "m").command, "PUBLISH")
  }

  test("a family called keys can be imported") {
    import laserdisc.redis_6_2.keys.*
    assertEquals(del(Key("a")).command, "DEL")
  }

  test("blocking commands can be imported from a version") {
    import laserdisc.redis_7_0.blocking.*
    assertEquals(blmpop[String](OneOrMoreKeys.unsafeFrom(List(Key("a"))), protocol.ListP.Side.left, NonNegInt(1)).command, "BLMPOP")
    assert(compileErrors("""get[String](Key("a"))""").nonEmpty)
  }

  test("the families of each version together have what the version has") {
    val versions: List[Base6_2] = List(
      redis_6_2,
      redis_7_0,
      redis_7_2,
      redis_7_4,
      redis_8_0,
      redis_8_2,
      redis_8_4,
      redis_8_6,
      redis_8_8,
      redis_8_10,
      valkey_7_2,
      valkey_8_0,
      valkey_8_1,
      valkey_9_0,
      valkey_9_1
    )
    versions.foreach { v =>
      val arrays = v match {
        case v: Redis8_8 => List[protocol.Features](v.arrays)
        case _           => Nil
      }
      val families = arrays ++ List[protocol.Features](
        v.blocking,
        v.cluster,
        v.connection,
        v.geo,
        v.hashmaps,
        v.hyperloglog,
        v.keys,
        v.lists,
        v.publish,
        v.scripting,
        v.server,
        v.sets,
        v.sortedsets,
        v.streams,
        v.strings,
        v.transaction
      )
      assertEquals(families.flatMap(_.supported).toSet, v.supported.toSet)
    }
  }
}
