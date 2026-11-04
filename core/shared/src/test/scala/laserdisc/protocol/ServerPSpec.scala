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
  import ServerP.{ClientInfo, Info, InfoSection, InfoSectionKey}

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
}
