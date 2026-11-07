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

import laserdisc.auto.*

/** The optional arguments of the commands with the most combinations of them, sent where the servers' documentation says they go */
final class CommandOptionsSpec extends BaseSpec {
  private def listed(ss: List[String]): Arr = Arr(ss.map(Bulk(_)))
  private def bulks(ss: String*): Arr       = listed(ss.toList)

  test("geosearchstore sends the destination first, then the area, the sort, the count and STOREDIST") {
    import laserdisc.redis_8_10.*
    import geotypes.*

    val area                     = GeoArea.radius(GeoCenter.member("m"), NonNegDouble(1.5d), GeoUnit.kilometers)
    val sort                     = Direction.asc
    val count                    = GeoCount.any(2)
    val store                    = GeoStoreDistances
    val common                   = List("GEOSEARCHSTORE", "d", "s", "FROMMEMBER", "m", "BYRADIUS", "1.5", "km")
    def sent(more: String*): Arr = listed(common ++ more.toList)

    assertEquals(geosearchstore("s", "d", area).encode, sent())
    assertEquals(geosearchstore("s", "d", area, sort).encode, sent("ASC"))
    assertEquals(geosearchstore("s", "d", area, count).encode, sent("COUNT", "2", "ANY"))
    assertEquals(geosearchstore("s", "d", area, sort, count).encode, sent("ASC", "COUNT", "2", "ANY"))
    assertEquals(geosearchstore("s", "d", area, store).encode, sent("STOREDIST"))
    assertEquals(geosearchstore("s", "d", area, sort, store).encode, sent("ASC", "STOREDIST"))
    assertEquals(geosearchstore("s", "d", area, count, store).encode, sent("COUNT", "2", "ANY", "STOREDIST"))
    assertEquals(geosearchstore("s", "d", area, sort, count, store).encode, sent("ASC", "COUNT", "2", "ANY", "STOREDIST"))
    assertEquals(geosearchstore("s", "d", area).decode(Num(3L)), NonNegInt(3))
  }

  test("geosearch and geosearchstore send a polygon as its number of vertices, then each one's longitude and latitude") {
    import laserdisc.valkey_9_1.*
    import geotypes.*

    def at(n: Double) = GeoCoordinates(Latitude.unsafeFrom(n), Longitude.unsafeFrom(n))

    val polygon                      = GeoPolygon(at(1.5d), at(2.5d), at(3.5d))
    val sort                         = Direction.desc
    val count                        = GeoCount.any(2)
    val store                        = GeoStoreDistances
    val shape                        = List("BYPOLYGON", "3", "1.5", "1.5", "2.5", "2.5", "3.5", "3.5")
    def searched(more: String*): Arr = listed(List("GEOSEARCH", "k") ++ shape ++ more)
    def stored(more: String*): Arr   = listed(List("GEOSEARCHSTORE", "d", "s") ++ shape ++ more)

    assertEquals(geosearch("k", polygon).encode, searched())
    assertEquals(geosearch("k", polygon, sort).encode, searched("DESC"))
    assertEquals(geosearch("k", polygon, count).encode, searched("COUNT", "2", "ANY"))
    assertEquals(geosearch("k", polygon, sort, count).encode, searched("DESC", "COUNT", "2", "ANY"))

    assertEquals(geosearchstore("s", "d", polygon).encode, stored())
    assertEquals(geosearchstore("s", "d", polygon, sort).encode, stored("DESC"))
    assertEquals(geosearchstore("s", "d", polygon, count).encode, stored("COUNT", "2", "ANY"))
    assertEquals(geosearchstore("s", "d", polygon, sort, count).encode, stored("DESC", "COUNT", "2", "ANY"))
    assertEquals(geosearchstore("s", "d", polygon, store).encode, stored("STOREDIST"))
    assertEquals(geosearchstore("s", "d", polygon, sort, store).encode, stored("DESC", "STOREDIST"))
    assertEquals(geosearchstore("s", "d", polygon, count, store).encode, stored("COUNT", "2", "ANY", "STOREDIST"))
    assertEquals(geosearchstore("s", "d", polygon, sort, count, store).encode, stored("DESC", "COUNT", "2", "ANY", "STOREDIST"))
  }

  test("zrangestore and zrevrangestore send the destination first, and a reverse range from its highest end") {
    import laserdisc.redis_8_10.*
    import sortedsettypes.*

    val score           = scorerange.closed(ValidDouble(1.5d), ValidDouble(2.5d))
    val lex             = lexrange.closed("a", "b")
    val (offset, count) = (NonNegLong(1L), PosLong(2L))

    assertEquals(zrangestore("s", "d", Index(0L), Index(-1L)).encode, bulks("ZRANGESTORE", "d", "s", "0", "-1"))
    assertEquals(zrangestore("s", "d", score).encode, bulks("ZRANGESTORE", "d", "s", "1.5", "2.5", "BYSCORE"))
    assertEquals(
      zrangestore("s", "d", score, offset, count).encode,
      bulks("ZRANGESTORE", "d", "s", "1.5", "2.5", "BYSCORE", "LIMIT", "1", "2")
    )
    assertEquals(zrangestore("s", "d", lex).encode, bulks("ZRANGESTORE", "d", "s", "[a", "[b", "BYLEX"))
    assertEquals(zrangestore("s", "d", lex, offset, count).encode, bulks("ZRANGESTORE", "d", "s", "[a", "[b", "BYLEX", "LIMIT", "1", "2"))

    assertEquals(zrevrangestore("s", "d", Index(0L), Index(-1L)).encode, bulks("ZRANGESTORE", "d", "s", "0", "-1", "REV"))
    assertEquals(zrevrangestore("s", "d", score).encode, bulks("ZRANGESTORE", "d", "s", "2.5", "1.5", "BYSCORE", "REV"))
    assertEquals(
      zrevrangestore("s", "d", score, offset, count).encode,
      bulks("ZRANGESTORE", "d", "s", "2.5", "1.5", "BYSCORE", "REV", "LIMIT", "1", "2")
    )
    assertEquals(zrevrangestore("s", "d", lex).encode, bulks("ZRANGESTORE", "d", "s", "[b", "[a", "BYLEX", "REV"))
    assertEquals(
      zrevrangestore("s", "d", lex, offset, count).encode,
      bulks("ZRANGESTORE", "d", "s", "[b", "[a", "BYLEX", "REV", "LIMIT", "1", "2")
    )
  }

  test("migrate sends one key in place, several after KEYS, and the mode and the password before them") {
    import laserdisc.redis_8_10.*
    import keytypes.*

    val two  = TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))
    val mode = KeyMigrateMode.both
    val auth = KeyMigrateAuth.password("p")
    val one  = List("MIGRATE", "h", "6379", "k", "1", "5")
    val many = List("MIGRATE", "h", "6379", "", "1", "5")
    val keys = List("KEYS", "a", "b")

    assertEquals(migrate("k", "h", 6379, DbIndex(1), 5).encode, listed(one))
    assertEquals(migrate(two, "h", 6379, DbIndex(1), 5).encode, listed(many ++ keys))
    assertEquals(migrate("k", "h", 6379, DbIndex(1), 5, mode).encode, listed(one ++ List("COPY", "REPLACE")))
    assertEquals(migrate(two, "h", 6379, DbIndex(1), 5, mode).encode, listed(many ++ List("COPY", "REPLACE") ++ keys))
    assertEquals(migrate("k", "h", 6379, DbIndex(1), 5, auth).encode, listed(one ++ List("AUTH", "p")))
    assertEquals(migrate(two, "h", 6379, DbIndex(1), 5, auth).encode, listed(many ++ List("AUTH", "p") ++ keys))
    assertEquals(migrate("k", "h", 6379, DbIndex(1), 5, mode, auth).encode, listed(one ++ List("COPY", "REPLACE", "AUTH", "p")))
    assertEquals(migrate(two, "h", 6379, DbIndex(1), 5, mode, auth).encode, listed(many ++ List("COPY", "REPLACE", "AUTH", "p") ++ keys))
  }

  test("xadd sends NOMKSTREAM, the reference policy, the idempotency, the trim, the id and the fields, in that order") {
    import laserdisc.redis_8_10.*
    import streamtypes.*

    val fields                     = OneOrMore.unsafeFrom(List(Key("a") -> 1))
    val no                         = StreamNoMkStream
    val id                         = StreamId(NonNegLong(1L), NonNegLong(0L))
    val at                         = StreamIdAt(NonNegLong(1L))
    val trim                       = StreamTrim.maxLen(NonNegLong(5L))
    val policy                     = StreamRefPolicy.keepRef
    val idmp                       = StreamIdmp.of("p", "i")
    def sent(middle: String*): Arr = listed(List("XADD", "k") ++ middle ++ List("a", "1"))

    assertEquals(xadd("k", id, trim, fields).encode, sent("MAXLEN", "5", "1-0"))
    assertEquals(xadd("k", no, fields).encode, sent("NOMKSTREAM", "*"))
    assertEquals(xadd("k", no, id, fields).encode, sent("NOMKSTREAM", "1-0"))
    assertEquals(xadd("k", no, id, trim, fields).encode, sent("NOMKSTREAM", "MAXLEN", "5", "1-0"))
    assertEquals(xadd("k", at, fields).encode, sent("1-*"))
    assertEquals(xadd("k", at, trim, fields).encode, sent("MAXLEN", "5", "1-*"))
    assertEquals(xadd("k", no, at, fields).encode, sent("NOMKSTREAM", "1-*"))
    assertEquals(xadd("k", no, at, trim, fields).encode, sent("NOMKSTREAM", "MAXLEN", "5", "1-*"))
    assertEquals(xadd("k", trim, policy, fields).encode, sent("KEEPREF", "MAXLEN", "5", "*"))
    assertEquals(xadd("k", id, trim, policy, fields).encode, sent("KEEPREF", "MAXLEN", "5", "1-0"))
    assertEquals(xadd("k", no, trim, policy, fields).encode, sent("NOMKSTREAM", "KEEPREF", "MAXLEN", "5", "*"))
    assertEquals(xadd("k", no, id, trim, policy, fields).encode, sent("NOMKSTREAM", "KEEPREF", "MAXLEN", "5", "1-0"))
    assertEquals(xadd("k", at, trim, policy, fields).encode, sent("KEEPREF", "MAXLEN", "5", "1-*"))
    assertEquals(xadd("k", no, at, trim, policy, fields).encode, sent("NOMKSTREAM", "KEEPREF", "MAXLEN", "5", "1-*"))
    assertEquals(xadd("k", idmp, fields).encode, sent("IDMP", "p", "i", "*"))
    assertEquals(xadd("k", idmp, trim, fields).encode, sent("IDMP", "p", "i", "MAXLEN", "5", "*"))
    assertEquals(xadd("k", idmp, trim, policy, fields).encode, sent("KEEPREF", "IDMP", "p", "i", "MAXLEN", "5", "*"))
    assertEquals(xadd("k", no, idmp, fields).encode, sent("NOMKSTREAM", "IDMP", "p", "i", "*"))
    assertEquals(xadd("k", no, idmp, trim, fields).encode, sent("NOMKSTREAM", "IDMP", "p", "i", "MAXLEN", "5", "*"))
    assertEquals(xadd("k", no, idmp, trim, policy, fields).encode, sent("NOMKSTREAM", "KEEPREF", "IDMP", "p", "i", "MAXLEN", "5", "*"))
  }

  test("the hash field expiry commands fail on a number their server doesn't reply with") {
    import laserdisc.redis_8_10.*

    val fields                                                  = OneOrMoreKeys.unsafeFrom(List(Key("f")))
    def fails[A](protocol: Protocol.Aux[A], number: Long): Unit = {
      val reply = protocol.decodeReplies(Seq(Arr(Num(number))))
      assert(reply.isLeft, reply.toString)
    }

    fails(hexpire("k", NonNegLong(5L), fields), 9L)
    fails(httl("k", fields), -5L)
    fails(hexpiretime("k", fields), -5L)
    fails(hpersist("k", fields), 9L)
  }
}
