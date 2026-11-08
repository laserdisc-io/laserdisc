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

/** Each command taking its items as separate arguments sends what it sends given the same items in a list */
final class VarargsSpec extends BaseSpec {
  private val oneOrMoreKeys       = OneOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))
  private val twoKeys             = TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"), Key("c")))
  private val values              = OneOrMore.unsafeFrom(List(1, 2))
  private val (id1, id2)          = (StreamP.Id(NonNegLong(1L), NonNegLong(0L)), StreamP.Id(NonNegLong(2L), NonNegLong(0L)))
  private val ids                 = OneOrMore.unsafeFrom(List(id1, id2))
  private val (index1, index2)    = (NonNegLong(1L), NonNegLong(2L))
  private val indexes             = OneOrMore.unsafeFrom(List(index1, index2))
  private val (position, another) =
    (GeoP.Position(Key("a"), Latitude(1d), Longitude(2d)), GeoP.Position(Key("b"), Latitude(3d), Longitude(4d)))
  private val positions = OneOrMore.unsafeFrom(List(position, another))

  test("the commands on lists, sets and transactions") {
    import laserdisc.redis_8_10.*

    assertEquals(lpushx("k", 1, 2).encode, lpushx("k", values).encode)
    assertEquals(rpushx("k", 1, 2).encode, rpushx("k", values).encode)
    assertEquals(smismember("k", 1, 2).encode, smismember("k", values).encode)
    assertEquals(sintercard("a", "b", "c").encode, sintercard(twoKeys).encode)
    assertEquals(sdiffcard("a", "b", "c").encode, sdiffcard(twoKeys).encode)
    assertEquals(sunioncard("a", "b", "c").encode, sunioncard(twoKeys).encode)
    assertEquals(watch("a", "b").encode, watch(oneOrMoreKeys).encode)
  }

  test("the commands on sorted sets") {
    import laserdisc.redis_8_10.*

    assertEquals(zdiff[String]("a", "b", "c").encode, zdiff[String](twoKeys).encode)
    assertEquals(zdiffwithscores[String]("a", "b", "c").encode, zdiffwithscores[String](twoKeys).encode)
    assertEquals(zinter[String]("a", "b", "c").encode, zinter[String](twoKeys).encode)
    assertEquals(zinterwithscores[String]("a", "b", "c").encode, zinterwithscores[String](twoKeys).encode)
    assertEquals(zunion[String]("a", "b", "c").encode, zunion[String](twoKeys).encode)
    assertEquals(zunionwithscores[String]("a", "b", "c").encode, zunionwithscores[String](twoKeys).encode)
    assertEquals(zintercard("a", "b", "c").encode, zintercard(twoKeys).encode)
    assertEquals(zmscore("k", 1, 2).encode, zmscore("k", values).encode)
  }

  test("the commands on hashes") {
    import laserdisc.redis_8_10.*
    import hashtypes.*

    val condition = HashExpireCondition.nx
    val expiry    = StringP.Expiry.seconds(PosLong(5L))

    assertEquals(hexpire("k", 5L, "a", "b").encode, hexpire("k", 5L, oneOrMoreKeys).encode)
    assertEquals(hexpire("k", 5L, condition, "a", "b").encode, hexpire("k", 5L, condition, oneOrMoreKeys).encode)
    assertEquals(hexpireat("k", 5L, "a", "b").encode, hexpireat("k", 5L, oneOrMoreKeys).encode)
    assertEquals(hexpireat("k", 5L, condition, "a", "b").encode, hexpireat("k", 5L, condition, oneOrMoreKeys).encode)
    assertEquals(hpexpire("k", 5L, "a", "b").encode, hpexpire("k", 5L, oneOrMoreKeys).encode)
    assertEquals(hpexpire("k", 5L, condition, "a", "b").encode, hpexpire("k", 5L, condition, oneOrMoreKeys).encode)
    assertEquals(hpexpireat("k", 5L, "a", "b").encode, hpexpireat("k", 5L, oneOrMoreKeys).encode)
    assertEquals(hpexpireat("k", 5L, condition, "a", "b").encode, hpexpireat("k", 5L, condition, oneOrMoreKeys).encode)
    assertEquals(hexpiretime("k", "a", "b").encode, hexpiretime("k", oneOrMoreKeys).encode)
    assertEquals(hpexpiretime("k", "a", "b").encode, hpexpiretime("k", oneOrMoreKeys).encode)
    assertEquals(hpersist("k", "a", "b").encode, hpersist("k", oneOrMoreKeys).encode)
    assertEquals(httl("k", "a", "b").encode, httl("k", oneOrMoreKeys).encode)
    assertEquals(hpttl("k", "a", "b").encode, hpttl("k", oneOrMoreKeys).encode)
    assertEquals(hgetex[String]("k", "a", "b").encode, hgetex[String]("k", oneOrMoreKeys).encode)
    assertEquals(hgetex[String]("k", expiry, "a", "b").encode, hgetex[String]("k", expiry, oneOrMoreKeys).encode)
    assertEquals(hgetdel[String]("k", "a", "b").encode, hgetdel[String]("k", oneOrMoreKeys).encode)
    assertEquals(himportprepare("n", "a", "b").encode, himportprepare("n", oneOrMoreKeys).encode)
    assertEquals(himportset("k", "n", 1, 2).encode, himportset("k", "n", values).encode)
  }

  test("the commands on geo sets") {
    import laserdisc.redis_8_10.*
    import geotypes.*

    assertEquals(geoadd("k", GeoFlag.nx, position, another).encode, geoadd("k", GeoFlag.nx, positions).encode)
    assertEquals(geoaddch("k", position, another).encode, geoaddch("k", positions).encode)
    assertEquals(geoaddch("k", GeoFlag.nx, position, another).encode, geoaddch("k", GeoFlag.nx, positions).encode)
  }

  test("the commands on streams") {
    import laserdisc.redis_8_10.*
    import streamtypes.*

    assertEquals(xdel("k", id1, id2).encode, xdel("k", ids).encode)
    assertEquals(xack("k", "g", id1, id2).encode, xack("k", "g", ids).encode)
    assertEquals(xclaim[String]("k", "g", "c", 0L, id1, id2).encode, xclaim[String]("k", "g", "c", 0L, ids).encode)
    assertEquals(xclaimjustid("k", "g", "c", 0L, id1, id2).encode, xclaimjustid("k", "g", "c", 0L, ids).encode)
    assertEquals(xackdel("k", "g", id1, id2).encode, xackdel("k", "g", ids).encode)
    assertEquals(xdelex("k", id1, id2).encode, xdelex("k", ids).encode)
    assertEquals(xnack("k", "g", StreamNackMode.silent, id1, id2).encode, xnack("k", "g", StreamNackMode.silent, ids).encode)
  }

  test("the commands on arrays") {
    import laserdisc.redis_8_10.*
    import arraytypes.*

    val (min, max)   = (ArrayBound.min, ArrayBound.max)
    val (one, other) = (ArrayPredicate.exact(1), ArrayPredicate.matching("m"))
    val predicates   = OneOrMore.unsafeFrom(List(one, other))

    assertEquals(ardel("k", index1, index2).encode, ardel("k", indexes).encode)
    assertEquals(argrep("k", min, max, one, other).encode, argrep("k", min, max, predicates).encode)
    assertEquals(argrepwithvalues[String]("k", min, max, one, other).encode, argrepwithvalues[String]("k", min, max, predicates).encode)
    assertEquals(arinsert("k", 1, 2).encode, arinsert("k", values).encode)
    assertEquals(armget[String]("k", index1, index2).encode, armget[String]("k", indexes).encode)
    assertEquals(arring("k", 3L, 1, 2).encode, arring("k", 3L, values).encode)
    assertEquals(arset("k", index1, 1, 2).encode, arset("k", index1, values).encode)
  }

  test("the commands under a namespace, and on the server") {
    import laserdisc.redis_8_10.*

    val (stats, cpu) = (servertypes.info.stats, servertypes.info.cpu)
    val sha          = Sha1("e0e1f9fabfc9d4800c877a703b823ac0578ff8db")

    assertEquals(cluster.addslots(Slot(1), Slot(2)).encode, cluster.addslots(OneOrMore.unsafeFrom(List(Slot(1), Slot(2)))).encode)
    assertEquals(cluster.delslots(Slot(1), Slot(2)).encode, cluster.delslots(OneOrMore.unsafeFrom(List(Slot(1), Slot(2)))).encode)
    assertEquals(pubsub.numsub("a", "b").encode, pubsub.numsub(oneOrMoreKeys).encode)
    assertEquals(pubsub.shardnumsub("a", "b").encode, pubsub.shardnumsub(oneOrMoreKeys).encode)
    assertEquals(script.exists(sha, sha).encode, script.exists(OneOrMore.unsafeFrom(List(sha, sha))).encode)
    assertEquals(client.list(index1, index2).encode, client.list(indexes).encode)
    assertEquals(
      config.get(GlobPattern("a*"), GlobPattern("b*")).encode,
      config.get(OneOrMore.unsafeFrom(List(GlobPattern("a*"), GlobPattern("b*")))).encode
    )
    assertEquals(info(stats, cpu).encode, info(OneOrMore.unsafeFrom(List[ServerP.InfoSection](stats, cpu))).encode)
  }

  test("the commands on Valkey's client") {
    import laserdisc.valkey_9_1.*

    assertEquals(client.capa("a", "b").encode, client.capa(oneOrMoreKeys).encode)
  }

  test("the commands on Dragonfly") {
    import laserdisc.dragonfly_2_0.*

    assertEquals(fieldexpire("k", 5L, "a", "b").encode, fieldexpire("k", 5L, oneOrMoreKeys).encode)
    assertEquals(saddex("k", 5L, 1, 2).encode, saddex("k", 5L, values).encode)
    assertEquals(saddex("k", dragonflytypes.keepTtl, 5L, 1, 2).encode, saddex("k", dragonflytypes.keepTtl, 5L, values).encode)
    assertEquals(stick("a", "b").encode, stick(oneOrMoreKeys).encode)
  }

  test("the commands with a form for one item need two items here") {
    import laserdisc.redis_8_10.*

    assert(compileErrors("""lpushx("k")""").nonEmpty)
    assertEquals(lpushx("k", 1).encode, lpushx("k", OneOrMore.unsafeFrom(List(1))).encode)
  }

  test("an invalid literal among the items doesn't compile") {
    import laserdisc.redis_8_10.*

    assertEquals(watch("a", "b").encode, watch(oneOrMoreKeys).encode)
    assert(compileErrors("""watch("a", "")""").nonEmpty)
    assert(compileErrors("""sintercard("a")""").nonEmpty)
    assert(compileErrors("""zmscore("k", 1, "a")""").nonEmpty)
  }
}
