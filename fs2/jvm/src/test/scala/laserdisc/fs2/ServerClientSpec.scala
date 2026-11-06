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

final class RedisServerClientSpec extends ServerClientSpec(6379, "redis") {
  test("a redis client reads the sections Redis 8 added under their names") {
    import laserdisc.redis_8_10.{info, servertypes}

    assert(send(info(servertypes.info.keysizes)).sections.contains(InfoSection.keysizes))
    assert(send(info(servertypes.info.hotkeys)).sections.contains(InfoSection.hotkeys))
    assert(send(info(servertypes.info.everything)).sections.keySet.exists(_.isInstanceOf[InfoSectionKey.Other]))
  }

  test("a redis client sets its library and reads several settings with the commands of Redis 8.10")(newer(redis_8_10, redis_8_10.client))
}

final class KeyDbServerClientSpec extends ServerClientSpec(6380, "keyDb")

final class ValkeyServerClientSpec extends ServerClientSpec(6381, "valkey") {
  test("a valkey client reads the TLS section of Valkey 9.1 under its name") {
    import laserdisc.valkey_9_1.{info, servertypes}

    assert(send(info(servertypes.info.tls)).sections.contains(InfoSection.tls))
  }

  test("a valkey client sets its library and reads several settings with the commands of Valkey 9.1")(newer(valkey_9_1, valkey_9_1.client))

  test("a valkey client finds clients with the filters of Valkey 9.1, and closes none when none match") {
    val v = laserdisc.valkey_9_1
    import v.servertypes

    val f     = servertypes.valkeyClientFilter
    val other = Host("192.0.2.1")
    val port  = Port(1)
    val me    = send(v.client.id)
    val mine  = OneOrMore.unsafeFrom(List(me))
    assertEquals(send(v.client.setname("ld-filter")), OK)
    def ids(protocol: Protocol.Aux[ConnectedClients]) = send(protocol).clients.map(_.id[String].fold(e => fail(e.getMessage), identity))

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

  protected final def send[A](protocol: Protocol.Aux[A]): A =
    connection.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  protected final def newer(version: Redis7_2, clientCommands: Client7_4): Unit = {
    import version.{config, info, servertypes}

    assertEquals(send(clientCommands.setlibname("laserdisc")), OK)
    assertEquals(send(clientCommands.setlibversion("0.8")), OK)
    assertEquals(send(clientCommands.noevict(on = false)), OK)
    assertEquals(send(clientCommands.notouch(on = false)), OK)
    assertEquals(send(clientCommands.kill(servertypes.clientFilter.id(999999L), 60L)).value, 0L)

    val settings = send(config.get(OneOrMore.unsafeFrom(List(GlobPattern("maxmemory-samples"), GlobPattern("timeout")))))
    val samples  = settings.parameters.`maxmemory-samples`[String].fold(e => fail(e.getMessage), identity)
    assertEquals(send(config.set(OneOrMore.unsafeFrom(List(Key("maxmemory-samples") -> samples)))), OK)
    assert(settings.parameters.timeout[String].isRight)

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
    assert(send(info(servertypes.info.server)).sections(InfoSection.server).redis_version[String].isRight)

    val id = send(v.client.id)
    assert(send(v.client.info).parameters.id[String] == Right(id.value.toString))
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
