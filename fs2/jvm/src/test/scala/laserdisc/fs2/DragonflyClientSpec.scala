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
import laserdisc.protocol.DragonflyP.{FieldExpireResult, FieldTtl, Rm, Throttle}
import laserdisc.protocol.ServerP.InfoSection

// what dragonfly_2_0 has, on the server it's for: the other Dragonfly specs extend those of the other servers, whose tests use redis_6_2
final class DragonflyClientSpec extends LaserdiscFs2Suite(6382) {
  import laserdisc.dragonfly_2_0.*

  private def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"dragonfly replied with an error: ${e.getMessage}", e), identity)

  private def failure[A](protocol: Protocol.Aux[A]): String =
    client.send(protocol).unsafeRunSync()(using runtime).fold(_.getMessage, a => fail(s"expected an error, but got $a"))

  private val laserdiscDragonfly = laserdisc.dragonfly_2_0

  test("a dragonfly client prepends, adds set members and fields that expire, makes keys sticky and deletes them while scanning") {
    send(del("df:p", "df:s", "df:h", "df:none"))
    send(set("df:p", "x"))
    assertEquals(send(prepend("df:p", "ab")).value, 3L)
    assertEquals(send(get[String]("df:p")), Some("abx"))

    assertEquals(send(saddex("df:s", 100L, OneOrMore.unsafeFrom(List("a", "b")))).value, 2L)
    assertEquals(send(saddex("df:s", dragonflytypes.keepTtl, 100L, OneOrMore.unsafeFrom(List("b", "c")))).value, 1L)
    assert(send(fieldttl("df:s", "a")) match {
      case FieldTtl.Expires(seconds) => seconds.value > 0L
      case _                         => false
    })

    send(hset("df:h", "a", 1))
    assertEquals(send(fieldttl("df:h", "a")), FieldTtl.NoExpiry)
    assertEquals(send(fieldttl("df:h", "none")), FieldTtl.NoField)
    assertEquals(send(fieldttl("df:none", "a")), FieldTtl.NoKey)
    assertEquals(
      send(fieldexpire("df:h", 100L, OneOrMoreKeys.unsafeFrom(List(Key("a"), Key("none"))))),
      Seq[FieldExpireResult](FieldExpireResult.Set, FieldExpireResult.Missing)
    )

    assertEquals(send(stick(OneOrMoreKeys.unsafeFrom(List(Key("df:p"), Key("df:none"))))).value, 1L)
    assertEquals(send(rm(0L, dragonflytypes.rmOptions.pattern("df:p").keyType(keytypes.KeyType.string).count(1000))), Rm(0L, 1L))
    assertEquals(send(exists("df:p")), None)
    assertEquals(send(rm(0L, dragonflytypes.rmOptions.pattern("df:none*").count(1000))), Rm(0L, 0L))
  }

  test("a dragonfly client throttles") {
    send(del("df:t"))
    assertEquals(
      send(clthrottle("df:t", 0L, 1L, 60L)),
      Throttle(false, 1L, 0L, None, send(clthrottle("df:t2", 0L, 1L, 60L)).resetAfterSeconds)
    )
    val limited = send(clthrottle("df:t", 0L, 1L, 60L, 1L))
    assert(limited.limited)
    assert(limited.retryAfterSeconds.nonEmpty)
    send(del("df:t", "df:t2"))
  }

  test("a dragonfly client takes over as a replica would, which does nothing on a primary") {
    assertEquals(send(repltakeover(1)), OK)
  }

  test("a dragonfly client reads the server's time, info and role") {
    assert(send(time).timestamp.value > 0L)
    assert(send(info).sections.contains(InfoSection.server))
    assert(send(info(servertypes.info.keyspace)).sections.contains(InfoSection.keyspace))
    assert(send(info(OneOrMore.unsafeFrom(List(servertypes.info.server, servertypes.info.cpu)))).sections.contains(InfoSection.cpu))
    assert(send(lastsave).value > 0L)
    assert(send(dbsize).value >= 0L)
    assertEquals(send(replicaofnoone), OK)
    assertEquals(send(slaveofnoone), OK)
    assertEquals(send(role), dragonflytypes.role.Primary(Nil))
  }

  test("a dragonfly client lists and closes clients with the filters it has") {
    val v  = laserdiscDragonfly
    val me = send(v.client.id)
    assertEquals(send(v.client.setname("ld-df")), OK)
    assertEquals(send(v.client.getname).map(_.value), Some("ld-df"))
    assertEquals(send(v.client.unsetname), OK)
    assert(send(v.client.help).nonEmpty)
    assert(send(v.client.info).parameters.id[String] == Right(me.value.toString))
    assert(send(v.client.list).clients.nonEmpty)
    assert(send(v.client.list(servertypes.clientType.normal)).clients.nonEmpty)
    assertEquals(send(v.client.list(OneOrMore.unsafeFrom(List(me)))).clients.size, 1)
    assertEquals(send(v.client.setlibname("laserdisc")), OK)
    assertEquals(send(v.client.setlibversion("0.8")), OK)
    assertEquals(send(v.client.pause(1L)), OK)
    assertEquals(send(v.client.pause(1L, servertypes.pauseMode.write)), OK)
    assertEquals(send(v.client.unpause), OK)
    assertEquals(send(v.client.kill(999999L)).value, 0L)
    assertEquals(send(v.client.killaddress(Host("192.0.2.1"), Port(1))).value, 0L)
    assertEquals(send(v.client.killlocaladdress(Host("192.0.2.1"), Port(1))).value, 0L)
  }

  test("a dragonfly client inspects the server") {
    send(rpush("df:m", "a"))
    assert(send(slowlog.get).size >= 0)
    assert(send(slowlog.get(5)).size <= 5)
    assert(send(slowlog.len).value >= 0L)
    assert(send(slowlog.help).nonEmpty)
    assertEquals(send(slowlog.reset), OK)
    assert(send(memory.usage("df:m")).exists(_.value > 0L))
    assert(send(memory.usage("df:m", 0)).exists(_.value > 0L))
    assertEquals(send(memory.usage("df:none")), None)
    assert(send(memory.stats).values.nonEmpty)
    assert(send(memory.mallocstats).nonEmpty)
    assert(send(memory.help).nonEmpty)
    assert(send(latency.latest).forall(_.name.nonEmpty))
    assert(send(latency.histogram).size >= 0)
    assert(send(latency.histogram(OneOrMore.unsafeFrom(List(Key("get"))))).size >= 0)
    assert(send(command.count).value > 0)
    assert(send(command.help).nonEmpty)
    assertEquals(send(command.info(OneOrMore.unsafeFrom(List(Key("get"))))).flatten.size, 1)
    assert(send(module.list).size >= 0)
    assert(send(config.help).nonEmpty)
    assert(send(config.get("maxmemory")).parameters.maxmemory[String].isRight)
    assertEquals(send(config.set("slowlog_max_len", 128)), OK)
    assertEquals(send(config.resetstat), OK)
    send(del("df:m"))
  }

  test("a dragonfly client manages users with the rules it accepts") {
    val rules = servertypes.aclRules.on.password("secret").keys("app:*").allow("get").allow("set")
    assertEquals(send(acl.setuser("ld-df", rules)), OK)
    val user = send(acl.getuser("ld-df")).getOrElse(fail("expected the user"))
    assert(user.flags.contains("on"))
    assertEquals(user.passwords.size, 1)
    assert(user.keys.contains("~app:*"))
    assertEquals(send(acl.getuser("ld-df-none")), None)
    assert(send(acl.users).contains("ld-df"))
    assert(send(acl.list).exists(_.startsWith("user ld-df ")))
    assert(send(acl.whoami).nonEmpty)
    assert(send(acl.cat).nonEmpty)
    assert(send(acl.cat("read")).nonEmpty)
    assert(send(acl.genpass).nonEmpty)
    assert(send(acl.genpass(32)).nonEmpty)
    assert(send(acl.help).nonEmpty)
    assert(send(acl.log).size >= 0)
    assert(send(acl.log(1)).size <= 1)
    assertEquals(send(acl.logreset), OK)
    assertEquals(send(acl.dryrun("ld-df", get[String]("app:1"))), None)
    assert(failure(acl.load).contains("ACL file"))
    assert(failure(acl.save).contains("ACL file"))
    assertEquals(send(acl.deluser(OneOrMoreKeys.unsafeFrom(List(Key("ld-df"))))).value, 1L)
  }

  test("a dragonfly client runs scripts, and read-only ones") {
    val sha = send(script.load("return 1"))
    assertEquals(send(script.exists(OneOrMore.unsafeFrom(List(sha)))), Seq(true))
    assertEquals(send(eval[Num, Long]("return 1")), 1L)
    assertEquals(send(evalsha[Num, Long](sha)), 1L)
    assertEquals(send(evalro[Num, Long]("return 1")), 1L)
    assertEquals(send(evalsharo[Num, Long](sha)), 1L)
    assert(send(script.help).nonEmpty)
    assertEquals(send(script.flush), OK)
    assertEquals(send(script.flush(scriptingtypes.ScriptingFlushMode.sync)), OK)
  }

  test("a dragonfly client copies, expires on conditions and sorts by GET #") {
    send(del("df:k", "df:c", "df:l"))
    send(set("df:k", "v"))
    assert(send(copy("df:k", "df:c")))
    assert(send(copy("df:k", "df:c", keytypes.KeyReplace)))
    assert(send(expire("df:k", 100, keytypes.KeyExpireCondition.nx)))
    assert(!send(expire("df:k", 100, keytypes.KeyExpireCondition.nx)))
    assert(send(expiretime("df:k")).toString.nonEmpty)
    send(rpush("df:l", OneOrMore.unsafeFrom(List(1, 2, 3))))
    assertEquals(send(sort[Int]("df:l", keytypes.KeySort.desc)), Seq(3, 2, 1))
    assertEquals(send(sortro[Int]("df:l", keytypes.KeySort.desc)), Seq(3, 2, 1))
    assertEquals(send(sort[String]("df:l", keytypes.KeySort.alpha, keytypes.KeySortGet("#"))), Seq(Some("1"), Some("2"), Some("3")))
    send(del("df:k", "df:c", "df:l"))
  }

  test("a dragonfly client pops from several lists and sorted sets, and counts intersections") {
    send(del("df:l", "df:z", "df:s1", "df:s2"))
    send(rpush("df:l", OneOrMore.unsafeFrom(List("a", "b"))))
    val lists = OneOrMoreKeys.unsafeFrom(List(Key("df:l")))
    assertEquals(send(lmpop[String](lists, listtypes.ListSide.left)), Some(KV(Key("df:l"), Seq("a"))))
    assertEquals(send(blocking.blmpop[String](lists, listtypes.ListSide.left, 1)), Some(KV(Key("df:l"), Seq("b"))))
    assertEquals(send(lmpop[String](lists, listtypes.ListSide.left)), None)
    assertEquals(send(lpop[String]("df:l", 1)), None)
    send(sadd("df:s1", OneOrMoreKeys.unsafeFrom(List(Key("a")))))
    send(sadd("df:s2", OneOrMoreKeys.unsafeFrom(List(Key("a")))))
    assertEquals(send(sintercard(TwoOrMoreKeys.unsafeFrom(List(Key("df:s1"), Key("df:s2"))))).value, 1)
    send(zadd("df:z", OneOrMore.unsafeFrom(List("a" -> ValidDouble(1.0), "b" -> ValidDouble(2.0)))))
    assert(send(zrandmember[String]("df:z")).exists(Set("a", "b")))
    assertEquals(send(zrankwithscore("df:z", "b")), Some(NonNegInt(1) -> 2.0))
    val zs = OneOrMoreKeys.unsafeFrom(List(Key("df:z")))
    assertEquals(send(zmpop[String](zs, laserdisc.protocol.SortedSetP.Pop.min)), Some(KV(Key("df:z"), Seq("a" -> 1.0))))
    assertEquals(send(blocking.bzmpop[String](zs, laserdisc.protocol.SortedSetP.Pop.min, 1)), Some(KV(Key("df:z"), Seq("b" -> 2.0))))
    send(del("df:s1", "df:s2"))
  }

  test("a dragonfly client reads a stream, its groups and its pending entries") {
    import streamtypes.*
    send(del("df:x"))
    send(xgroupcreate("df:x", "g", StreamFrom.latest, StreamMkStream))
    val first  = send(xadd("df:x", OneOrMore.unsafeFrom(List(Key("a") -> 1))))
    val second = send(xadd("df:x", StreamTrim.maxLenExact(100L), OneOrMore.unsafeFrom(List(Key("a") -> 2))))
    send(xreadgroup[Int]("g", "c", OneOrMore.unsafeFrom(List(Key("df:x") -> StreamGroupFrom.newEntries))))
    val summary = send(xpending("df:x", "g"))
    assertEquals(summary.count.value, 2L)
    assertEquals(summary.consumers, Seq(Key("c") -> NonNegLong(2L)))
    assertEquals(send(xpending("df:x", "g", StreamBound.min, StreamBound.max, 10L)).map(_.id), Seq(first, second))
    assertEquals(send(xclaim[Int]("df:x", "g", "c2", 0L, OneOrMore.unsafeFrom(List(first)))).map(_.id), Seq(first))
    assertEquals(send(xautoclaim[Int]("df:x", "g", "c3", 0L, StreamId(0L, 0L))).claimed.map(_.id), Seq(first, second))
    assertEquals(send(xinfostream[Int]("df:x")).length.value, 2L)
    assertEquals(send(xinfostreamfull[Int]("df:x")).entries.map(_.id), Seq(first, second))
    assertEquals(send(xinfogroups("df:x")).map(_.name), Seq(Key("g")))
    assert(send(xinfoconsumers("df:x", "g")).map(_.name).contains(Key("c3")))
    assert(send(xgrouphelp).nonEmpty)
    assert(send(xinfohelp).nonEmpty)
    assertEquals(send(xgroupsetid("df:x", "g", StreamFrom.latest, 2L)), OK)
    send(del("df:x"))
  }

  test("a dragonfly client expires hash fields, and sets and gets them with an expiry") {
    send(del("df:h"))
    send(hset("df:h", "a", 1))
    val a = OneOrMoreKeys.unsafeFrom(List(Key("a")))
    assertEquals(send(httl("df:h", a)).size, 1)
    assertEquals(send(hexpire("df:h", 100L, a)).size, 1)
    assertEquals(send(hexpire("df:h", 100L, hashtypes.HashExpireCondition.nx, a)).size, 1)
    assertEquals(send(hpexpiretime("df:h", a)).size, 1)
    assertEquals(send(hgetex[Int]("df:h", a)), Seq(Some(1)))
    assertEquals(send(hgetex[Int]("df:h", stringtypes.expiry.seconds(100L), a)), Seq(Some(1)))
    assert(send(hsetex("df:h", OneOrMore.unsafeFrom(List(Key("a") -> 2)))))
    assert(
      send(hsetex("df:h", hashtypes.HashFieldsCondition.fxx, stringtypes.expiry.seconds(100L), OneOrMore.unsafeFrom(List(Key("a") -> 3))))
    )
    assert(send(hscannovalues("df:h", 0L)).values.nonEmpty)
    send(del("df:h"))
  }

  test("a dragonfly client digests strings, deletes on a match and counts bits by unit") {
    send(del("df:d", "df:b"))
    send(set("df:d", "v"))
    val hash = send(digest("df:d")).getOrElse(fail("expected a digest"))
    assert(!send(delex("df:d", stringtypes.compare.ifDeq(hash + "0"))))
    assert(send(delex("df:d", stringtypes.ifEq("v"))))
    assertEquals(send(digest("df:d")), None)
    send(setbit("df:b", 3L, stringtypes.bit.set))
    assertEquals(send(bitcount("df:b", 0L, 7L, stringtypes.bitUnit.bit)).value, 1)
    assertEquals(send(bitpos("df:b", stringtypes.bit.set, 0L, 7L, stringtypes.bitUnit.bit)).map(_.value), Some(3))
    send(del("df:b"))
  }

  test("a dragonfly client publishes, with shard channels outside cluster mode only refused") {
    assert(send(publish("df:c", "m")).value >= 0)
    assert(send(spublish("df:c", "m")).value >= 0)
    assert(send(pubsub.channels).size >= 0)
    assert(send(pubsub.numpat).value >= 0)
    assert(send(pubsub.help).nonEmpty)
    assert(failure(pubsub.shardchannels).contains("non cluster mode"))
  }
}

// a Dragonfly in its emulated cluster mode, which only reports on itself: the rest of CLUSTER isn't in it
final class DragonflyClusterClientSpec extends LaserdiscFs2Suite(6383) {
  import laserdisc.dragonfly_2_0.*

  private def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"dragonfly replied with an error: ${e.getMessage}", e), identity)

  test("a dragonfly client in cluster mode reads the cluster's information, nodes, slots and shards") {
    assert(send(clusterhelp).nonEmpty)
    assert(send(clusterinfo).toString.nonEmpty)
    assertEquals(send(keyslot("a")).value, 15495)
    assert(send(myid).toString.nonEmpty)
    assert(send(nodes).nodes.nonEmpty)
    assert(send(slots).slots.nonEmpty)
    assertEquals(send(shards).size, 1)
    assertEquals(send(readwrite), OK)
    assertEquals(send(readonly), OK)
    assert(send(pubsub.shardchannels).size >= 0)
    assert(send(pubsub.shardnumsub(OneOrMoreKeys.unsafeFrom(List(Key("df:c"))))).size == 1)
  }
}
