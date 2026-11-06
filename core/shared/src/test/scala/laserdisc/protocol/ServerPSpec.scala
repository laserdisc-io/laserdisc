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

import org.scalacheck.Prop.forAll

final class ServerPSpec extends BaseSpec with ServerP {
  import ServerP.{ClientInfo, Info, InfoSection, InfoSectionKey, Role}

  test("The Server protocol using info reads the sections a server sends, including ones laserdisc has no name for, like a module's") {
    val reply = List(
      "# Server",
      "redis_version:8.10.3",
      "executable:/data/redis-server",
      "",
      "# Hotkeys",
      "",
      "# Keyspace",
      "db0:keys=1,expires=0,avg_ttl=0",
      "",
      "# Errorstats",
      "errorstat_ERR:count=3",
      "",
      "# Search_version",
      "search_version:8.0.0",
      ""
    ).mkString("\r\n")

    info.decode(Bulk(reply)).onRight { case Info(sections) =>
      sections.keySet == Set(
        InfoSection.server,
        InfoSection.hotkeys,
        InfoSection.keyspace,
        InfoSection.errorstats,
        InfoSectionKey.Other("search_version")
      ) &&
      sections(InfoSection.server).redis_version[String] == Right("8.10.3") &&
      sections(InfoSection.server).executable[String] == Right("/data/redis-server") &&
      sections(InfoSection.keyspace).db0[String] == Right("keys=1,expires=0,avg_ttl=0")
    }
    assertEquals(info(servertypes.info.keysizes).encode, Arr(Bulk("INFO"), Bulk("keysizes")))
    assertEquals(info(servertypes.info.latencystats).encode, Arr(Bulk("INFO"), Bulk("latencystats")))
    assertEquals(info(servertypes.info.everything).encode, Arr(Bulk("INFO"), Bulk("everything")))
  }

  property("The Server protocol using info with several sections sends each of them") {
    forAll { (b: Boolean) =>
      val sections = if (b) List(servertypes.info.memory, servertypes.info.cpu) else List(servertypes.info.modules)
      assertEquals(info(OneOrMore.unsafeFrom(sections)).encode, Arr(Bulk("INFO") :: sections.map(s => Bulk(Show[InfoSection].show(s)))))
    }
  }

  property("The Server protocol using client.kill with filters sends each filter and reads how many clients it closed") {
    forAll { (id: NonNegLong, h: Host, p: Port, u: String, n: NonNegLong) =>
      val filter   = servertypes.clientFilter.id(id).address(h, p).clientType(servertypes.clientType.pubsub).user(u).skipMe(false)
      val protocol = client.kill(filter)
      assertEquals(
        protocol.encode,
        Arr(
          List("CLIENT", "KILL", "ID", id.value.toString, "ADDR", s"${h.value}:${p.value}", "TYPE", "pubsub", "USER", u, "SKIPME", "no")
            .map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(Num(n.value)), n)
      assertEquals(
        client.kill(servertypes.clientFilter.localAddress(h, p), PosLong(10L)).encode,
        Arr(List("CLIENT", "KILL", "LADDR", s"${h.value}:${p.value}", "MAXAGE", "10").map(Bulk(_)))
      )
    }
  }

  property("The Server protocol using the client commands sends their arguments") {
    forAll { (ids: OneOrMore[NonNegLong], ms: PosLong, id: NonNegLong, k: Key) =>
      assertEquals(client.list(ids).encode, Arr(Bulk("CLIENT") :: Bulk("LIST") :: Bulk("ID") :: ids.value.map(Bulk(_))))
      assertEquals(client.list(servertypes.clientType.replica).encode, Arr(Bulk("CLIENT"), Bulk("LIST"), Bulk("TYPE"), Bulk("replica")))
      assertEquals(client.pause(ms, servertypes.pauseMode.write).encode, Arr(Bulk("CLIENT"), Bulk("PAUSE"), Bulk(ms), Bulk("WRITE")))
      assertEquals(client.unblock(id, servertypes.unblockMode.error).encode, Arr(Bulk("CLIENT"), Bulk("UNBLOCK"), Bulk(id), Bulk("ERROR")))
      assertEquals(client.unblock(id).decode(Num(1L)), true)
      assertEquals(client.noevict(on = true).encode, Arr(Bulk("CLIENT"), Bulk("NO-EVICT"), Bulk("ON")))
      assertEquals(client.notouch(on = false).encode, Arr(Bulk("CLIENT"), Bulk("NO-TOUCH"), Bulk("OFF")))
      assertEquals(client.setlibname(k).encode, Arr(Bulk("CLIENT"), Bulk("SETINFO"), Bulk("LIB-NAME"), Bulk(k)))
      assertEquals(client.id.decode(Num(id.value)), id)
    }
  }

  test("The Server protocol using client.info reads the client's fields") {
    client.info.decode(Bulk("id=3 addr=127.0.0.1:50000 name=me db=0\n")).onRight { case ClientInfo(parameters) =>
      parameters.id[String] == Right("3") && parameters.name[String] == Right("me")
    }
  }

  property("The Server protocol using config with several parameters sends each of them") {
    forAll { (ps: OneOrMore[(Key, Int)]) =>
      assertEquals(config.set(ps).encode, Arr(Bulk("CONFIG") :: Bulk("SET") :: ps.value.flatMap { case (k, v) => List(Bulk(k), Bulk(v)) }))
      assertEquals(
        config.get(OneOrMore.unsafeFrom(List(GlobPattern("max*"), GlobPattern("timeout")))).encode,
        Arr(Bulk("CONFIG"), Bulk("GET"), Bulk("max*"), Bulk("timeout"))
      )
    }
  }

  private def words(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)
  private val twoIds                  = OneOrMore.unsafeFrom(List(NonNegLong(1L), NonNegLong(2L)))

  test("The Server protocol using client.list reads no clients from an empty reply") {
    assertEquals(client.list.decode(Bulk("")), ServerP.ConnectedClients(Nil))
    assertEquals(client.list.decode(Bulk("id=1 name=a\nid=2 name=b\n")).map(_.clients.size), 2)
  }

  test("The Server protocol has the slave client type, and primary from client") {
    assertEquals(client.list(servertypes.clientType.slave).encode, words("CLIENT", "LIST", "TYPE", "slave"))
    assertEquals(client.list(client.primary).encode, words("CLIENT", "LIST", "TYPE", "primary"))
    assertEquals(
      client.kill(servertypes.clientFilter.clientType(client.primary)).encode,
      words("CLIENT", "KILL", "TYPE", "primary")
    )
  }

  test("The Server protocol using client.list sends the filters of CLIENT KILL, and client.ids sends several ids") {
    val filter = servertypes.clientFilter.user("u").skipMe(true)
    assertEquals(client.list(filter).encode, words("CLIENT", "LIST", "USER", "u", "SKIPME", "yes"))
    assertEquals(client.list(filter, PosLong(9L)).encode, words("CLIENT", "LIST", "USER", "u", "SKIPME", "yes", "MAXAGE", "9"))
    assertEquals(client.kill(client.ids(twoIds).skipMe(false)).encode, words("CLIENT", "KILL", "ID", "1", "2", "SKIPME", "no"))
    assertEquals(client.list(client.ids(twoIds)).encode, words("CLIENT", "LIST", "ID", "1", "2"))
  }

  test("The Server protocol sends each of Valkey's filters, for both CLIENT KILL and CLIENT LIST") {
    val h        = Host("h.example")
    val p        = Port(1)
    val k        = Key("k")
    val n        = ConnectionName("n")
    val d        = DbIndex(1)
    val f        = servertypes.valkeyClientFilter
    val positive = f
      .ids(twoIds)
      .clientType(servertypes.clientType.normal)
      .user("u")
      .address(h, p)
      .localAddress(h, p)
      .ip(h)
      .name(n)
      .flags(k)
      .libName(k)
      .libVersion(k)
      .db(d)
      .capabilities(k)
      .maxAge(PosLong(5L))
      .idle(PosLong(6L))
      .skipMe(false)
    val positiveWords = List(
      "ID",
      "1",
      "2",
      "TYPE",
      "normal",
      "USER",
      "u",
      "ADDR",
      "h.example:1",
      "LADDR",
      "h.example:1",
      "IP",
      "h.example",
      "NAME",
      "n",
      "FLAGS",
      "k",
      "LIB-NAME",
      "k",
      "LIB-VER",
      "k",
      "DB",
      "1",
      "CAPA",
      "k",
      "MAXAGE",
      "5",
      "IDLE",
      "6",
      "SKIPME",
      "no"
    )
    assertEquals(client.kill(positive).encode, Arr(("CLIENT" :: "KILL" :: positiveWords).map(Bulk(_))))
    val negative = f
      .notIds(twoIds)
      .notClientType(client.primary)
      .notUser("u")
      .notAddress(h, p)
      .notLocalAddress(h, p)
      .notIp(h)
      .notName(n)
      .notFlags(k)
      .notLibName(k)
      .notLibVersion(k)
      .notDb(d)
      .notCapabilities(k)
    val negativeWords = List(
      "NOT-ID",
      "1",
      "2",
      "NOT-TYPE",
      "primary",
      "NOT-USER",
      "u",
      "NOT-ADDR",
      "h.example:1",
      "NOT-LADDR",
      "h.example:1",
      "NOT-IP",
      "h.example",
      "NOT-NAME",
      "n",
      "NOT-FLAGS",
      "k",
      "NOT-LIB-NAME",
      "k",
      "NOT-LIB-VER",
      "k",
      "NOT-DB",
      "1",
      "NOT-CAPA",
      "k"
    )
    assertEquals(client.list(negative).encode, Arr(("CLIENT" :: "LIST" :: negativeWords).map(Bulk(_))))
    assertEquals(client.list(f.idle(PosLong(6L))).encode, words("CLIENT", "LIST", "IDLE", "6"))
    assertEquals(client.kill(f.notName(n)).encode, words("CLIENT", "KILL", "NOT-NAME", "n"))
    assertEquals(client.kill(f.notName(n)).decode(Num(2L)), NonNegLong(2L))
    // each filter can also start one
    List[(ServerP.ValkeyClientFilter, List[String])](
      f.clientType(servertypes.clientType.normal)    -> List("TYPE", "normal"),
      f.user("u")                                    -> List("USER", "u"),
      f.address(h, p)                                -> List("ADDR", "h.example:1"),
      f.localAddress(h, p)                           -> List("LADDR", "h.example:1"),
      f.ip(h)                                        -> List("IP", "h.example"),
      f.name(n)                                      -> List("NAME", "n"),
      f.flags(k)                                     -> List("FLAGS", "k"),
      f.libName(k)                                   -> List("LIB-NAME", "k"),
      f.libVersion(k)                                -> List("LIB-VER", "k"),
      f.db(d)                                        -> List("DB", "1"),
      f.capabilities(k)                              -> List("CAPA", "k"),
      f.maxAge(PosLong(5L))                          -> List("MAXAGE", "5"),
      f.skipMe(true)                                 -> List("SKIPME", "yes"),
      f.notIds(twoIds)                               -> List("NOT-ID", "1", "2"),
      f.notClientType(servertypes.clientType.pubsub) -> List("NOT-TYPE", "pubsub"),
      f.notUser("u")                                 -> List("NOT-USER", "u"),
      f.notAddress(h, p)                             -> List("NOT-ADDR", "h.example:1"),
      f.notLocalAddress(h, p)                        -> List("NOT-LADDR", "h.example:1"),
      f.notIp(h)                                     -> List("NOT-IP", "h.example"),
      f.notFlags(k)                                  -> List("NOT-FLAGS", "k"),
      f.notLibName(k)                                -> List("NOT-LIB-NAME", "k"),
      f.notLibVersion(k)                             -> List("NOT-LIB-VER", "k"),
      f.notDb(d)                                     -> List("NOT-DB", "1"),
      f.notCapabilities(k)                           -> List("NOT-CAPA", "k")
    ).foreach { case (filter, expected) => assertEquals(filter.params, expected) }
  }

  test("The Server protocol using shutdown sends SAFE and FAILOVER") {
    val o = servertypes.valkeyShutdownOptions
    assertEquals(shutdown(o.safe).encode, words("SHUTDOWN", "SAFE"))
    assertEquals(shutdown(o.failover).encode, words("SHUTDOWN", "FAILOVER"))
    assertEquals(shutdown(o.now.force.safe.failover).encode, words("SHUTDOWN", "NOW", "FORCE", "SAFE", "FAILOVER"))
    assertEquals(shutdown(o.force.failover).encode, words("SHUTDOWN", "FORCE", "FAILOVER"))
    assertEquals(shutdown(servertypes.flag.save, o.safe).encode, words("SHUTDOWN", "save", "SAFE"))
  }

  property("The Server protocol using replicafailover, replicaof and shutdown sends their options") {
    forAll { (h: Host, p: Port, ms: PosLong) =>
      assertEquals(replicafailover(h, p, ms).encode, Arr(Bulk("FAILOVER"), Bulk("TO"), Bulk(h), Bulk(p), Bulk("TIMEOUT"), Bulk(ms)))
      assertEquals(
        replicafailoverforce(h, p, ms).encode,
        Arr(Bulk("FAILOVER"), Bulk("TO"), Bulk(h), Bulk(p), Bulk("FORCE"), Bulk("TIMEOUT"), Bulk(ms))
      )
      assertEquals(replicaof(h, p).encode, Arr(Bulk("REPLICAOF"), Bulk(h), Bulk(p)))
      assertEquals(
        shutdown(servertypes.flag.nosave, servertypes.shutdownOptions.now.force).encode,
        Arr(Bulk("SHUTDOWN"), Bulk("nosave"), Bulk("NOW"), Bulk("FORCE"))
      )
    }
  }

  test("The Server protocol reads the message BGSAVE and BGREWRITEAOF reply with") {
    assertEquals(bgsave.decode(Str("Background saving started")), "Background saving started")
    assertEquals(bgsaveschedule.encode, Arr(Bulk("BGSAVE"), Bulk("SCHEDULE")))
    assertEquals(
      bgrewriteaof.decode(Str("Background append only file rewriting scheduled")),
      "Background append only file rewriting scheduled"
    )
    assertEquals(flushallsync.encode, Arr(Bulk("FLUSHALL"), Bulk("SYNC")))
    assertEquals(replicafailoverabort.encode, Arr(Bulk("FAILOVER"), Bulk("ABORT")))
  }

  test("The Server protocol using role reads a master, with its replicas") {
    assertEquals(role.encode, Arr(Bulk("ROLE")))
    assertEquals(role.decode(Arr(Bulk("master"), Num(0L), Arr(Nil))), Role.Master(NonNegLong(0L), Nil): Role)
    assertEquals(
      role.decode(Arr(Bulk("master"), Num(14L), Arr(Arr(Bulk("172.18.0.3"), Bulk("6379"), Bulk("14"))))),
      Role.Master(NonNegLong(14L), Seq(Role.Client(Host.unsafeFrom("172.18.0.3"), Port.unsafeFrom(6379), NonNegLong(14L)))): Role
    )
  }

  test("The Server protocol using role reads a replica, with no offset until it has synced") {
    def replica(status: String, offset: Long) = Arr(Bulk("slave"), Bulk("10.0.0.1"), Num(6379L), Bulk(status), Num(offset))
    val host                                  = Host.unsafeFrom("10.0.0.1")
    val port                                  = Port.unsafeFrom(6379)
    assertEquals(role.decode(replica("connected", 14L)), Role.Slave(host, port, Role.connected, Some(NonNegLong(14L))): Role)
    assertEquals(role.decode(replica("handshake", -1L)), Role.Slave(host, port, Role.handshake, None): Role)
    assertEquals(role.decode(replica("connecting", -1L)), Role.Slave(host, port, Role.connecting, None): Role)
    assert(role.decode(replica("nope", 0L)).isLeft)
  }

  test("The Server protocol using role reads a sentinel, and rejects a reply naming another role") {
    assertEquals(role.decode(Arr(Bulk("sentinel"), Arr(Bulk("mymaster")))), Role.Sentinel(Seq(Key("mymaster"))): Role)
    assert(role.decode(Arr(Bulk("nope"), Num(0L), Arr(Nil))).isLeft)
    assert(role.decode(Arr(Bulk("sentinel"), Num(0L), Arr(Nil))).isLeft)
    assert(role.decode(Arr(Bulk("master"), Num(0L))).isLeft)
  }
}
