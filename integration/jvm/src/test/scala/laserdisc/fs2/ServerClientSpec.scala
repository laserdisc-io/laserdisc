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
import laserdisc.protocol.ServerP.{ConnectedClients, InfoSection, InfoSectionKey}

final class RedisServerClientSpec extends ServerClientSpec(LiveServers.port(6379), "redis") {
  since[Redis8_0]("a redis client reads the sections Redis 8 added under their names") { v =>
    import v.{info, servertypes}

    assert(send(info(servertypes.info.keysizes)).sections.contains(InfoSection.keysizes))
    assert(send(info(servertypes.info.everything)).sections.keySet.exists(_.isInstanceOf[InfoSectionKey.Other]))
  }

  since[Redis8_6]("a redis client reads the hot keys section under its name") { v =>
    import v.{info, servertypes}

    assert(send(info(servertypes.info.hotkeys)).sections.contains(InfoSection.hotkeys))
  }

  since[Redis7_4]("a redis client sets its library and reads several settings")(v => newer(v, v.client))
}

final class KeyDbServerClientSpec extends ServerClientSpec(LiveServers.port(6380), "keyDb")

final class ValkeyServerClientSpec extends ServerClientSpec(LiveServers.port(6381), "valkey") {
  since[Valkey9_1]("a valkey client reads the TLS section under its name") { v =>
    import v.{info, servertypes}

    assert(send(info(servertypes.info.tls)).sections.contains(InfoSection.tls))
  }

  since[Valkey8_0]("a valkey client sets its library and reads several settings")(v => newer(v, v.client))

  since[Valkey9_1]("a valkey client finds clients with Valkey's filters, and closes none when none match") { v =>
    import v.servertypes

    val f     = servertypes.valkeyClientFilter
    val other = Host("192.0.2.1")
    val port  = Port(1)
    val me    = send(v.client.id)
    val mine  = OneOrMore.unsafeFrom(List(me))
    assertEquals(send(v.client.setname("ld-filter")), OK)
    def ids(protocol: Protocol[ConnectedClients]) =
      send(protocol).clients.map(_.get[String]("id").fold(e => fail(e.getMessage), identity))

    assertEquals(ids(v.client.list(servertypes.clientFilter.user("default").skipMe(false).id(me))), Seq(me.value.toString))
    assertEquals(ids(v.client.list(v.client.ids(mine).skipMe(false))), Seq(me.value.toString))
    assertEquals(ids(v.client.list(servertypes.clientFilter.id(me), 999999L)), Nil)
    assertEquals(ids(v.client.list(v.client.primary)), Nil)
    assertEquals(send(v.client.kill(v.client.ids(OneOrMore.unsafeFrom(List[NonNegLong](999991L, 999992L))))).value, 0L)

    assertEquals(ids(v.client.list(f.name("ld-filter"))), Seq(me.value.toString))
    assertEquals(ids(v.client.list(f.ids(mine).db(0).flags("N").user("default").notIp(other))), Seq(me.value.toString))
    assertEquals(ids(v.client.list(f.notIds(mine).name("ld-filter"))), Nil)
    assertEquals(ids(v.client.list(f.ids(mine).idle(999999L))), Nil)
    assertEquals(
      ids(
        v.client.list(
          f.ids(mine)
            .notClientType(servertypes.clientType.pubsub)
            .notAddress(other, port)
            .notLocalAddress(other, port)
            .notName("nobody")
            .notFlags("P")
            .notLibName("nobody")
            .notLibVersion("nobody")
            .notDb(1)
            .notCapabilities("r")
        )
      ),
      Seq(me.value.toString)
    )
    assertEquals(ids(v.client.list(f.ids(mine).notUser("default"))), Nil)
    assertEquals(ids(v.client.list(f.ids(mine).libName("nobody"))), Nil)
    assertEquals(ids(v.client.list(f.ids(mine).libVersion("nobody"))), Nil)
    assertEquals(ids(v.client.list(f.ids(mine).capabilities("r"))), Nil)
    assertEquals(ids(v.client.list(f.ids(mine).address(other, port))), Nil)
    assertEquals(ids(v.client.list(f.ids(mine).localAddress(other, port))), Nil)
    assertEquals(ids(v.client.list(f.ids(mine).ip(other))), Nil)
    assertEquals(send(v.client.kill(f.name("nobody").maxAge(5L))).value, 0L)

    assertEquals(send(v.client.unsetname), OK)
  }
}

abstract class ServerClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  private def connection = client

  protected final def send[A](protocol: Protocol[A]): A =
    connection.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  protected final def newer(version: Redis7_2, clientCommands: Client7_4): Unit = {
    import version.{config, info, servertypes}

    assertEquals(send(clientCommands.setlibname("laserdisc")), OK)
    assertEquals(send(clientCommands.setlibversion("0.8")), OK)
    assertEquals(send(clientCommands.noevict(on = false)), OK)
    assertEquals(send(clientCommands.notouch(on = false)), OK)
    assertEquals(send(clientCommands.kill(servertypes.clientFilter.id(999999L), 60L)).value, 0L)

    val settings = send(config.get(OneOrMore.unsafeFrom(List(GlobPattern("maxmemory-samples"), GlobPattern("timeout")))))
    val samples  = settings.parameters.get[String]("maxmemory-samples").fold(e => fail(e.getMessage), identity)
    assertEquals(send(config.set(OneOrMore.unsafeFrom(List(Key("maxmemory-samples") -> samples)))), OK)
    assert(settings.parameters.get[String]("timeout").isRight)

    assertEquals(
      send(info(OneOrMore.unsafeFrom(List(servertypes.info.server, servertypes.info.cpu)))).sections.keySet,
      Set[InfoSectionKey](InfoSection.server, InfoSection.cpu)
    )
  }

  test(s"a $dest client reads the server's info, and finds and lists clients") {
    val v = laserdisc.redis_6_2
    import v.{info, servertypes}

    val all = send(info).sections
    assert(all.contains(InfoSection.server) && all.contains(InfoSection.keyspace))
    assert(send(info(servertypes.info.server)).sections(InfoSection.server).get[String]("redis_version").isRight)

    val id = send(v.client.id)
    assert(send(v.client.info).parameters.get[String]("id") == Right(id.value.toString))
    assert(send(v.client.list(OneOrMore.unsafeFrom(List(id)))).clients.size == 1)
    assert(send(v.client.list(servertypes.clientType.normal)).clients.nonEmpty)
    assertEquals(send(v.client.kill(servertypes.clientFilter.id(999999L))).value, 0L)
    assertEquals(send(v.client.kill(servertypes.clientFilter.clientType(servertypes.clientType.slave))).value, 0L)
    assert(!send(v.client.unblock(999999L)))
    assertEquals(send(v.client.unpause), OK)
  }

  test(s"a $dest client reads the server's time") {
    import laserdisc.redis_6_2.*

    assert(send(time).timestamp.value > 0L)
  }
}
