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
import laserdisc.protocol.ServerP.{InfoSection, InfoSectionKey}

final class RedisServerClientSpec extends ServerClientSpec(6379, "redis") {
  test("a redis client reads the sections Redis 8 added under their names") {
    import laserdisc.redis_8_10.{info, servers}

    assert(send(info(servers.info.keysizes)).sections.contains(InfoSection.keysizes))
    assert(send(info(servers.info.hotkeys)).sections.contains(InfoSection.hotkeys))
    assert(send(info(servers.info.everything)).sections.keySet.exists(_.isInstanceOf[InfoSectionKey.Other]))
  }

  test("a redis client sets its library and reads several settings with the commands of Redis 8.10")(newer(redis_8_10, redis_8_10.client))
}

final class KeyDbServerClientSpec extends ServerClientSpec(6380, "keyDb")

final class ValkeyServerClientSpec extends ServerClientSpec(6381, "valkey") {
  test("a valkey client reads the TLS section of Valkey 9.1 under its name") {
    import laserdisc.valkey_9_1.{info, servers}

    assert(send(info(servers.info.tls)).sections.contains(InfoSection.tls))
  }

  test("a valkey client sets its library and reads several settings with the commands of Valkey 9.1")(newer(valkey_9_1, valkey_9_1.client))
}

abstract class ServerClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  private def connection = client

  protected final def send[A](protocol: Protocol.Aux[A]): A =
    connection.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  protected final def newer(version: Redis7_2, clientCommands: Client7_4): Unit = {
    import version.{config, info, servers}

    assertEquals(send(clientCommands.setlibname("laserdisc")), OK)
    assertEquals(send(clientCommands.setlibversion("0.8")), OK)
    assertEquals(send(clientCommands.noevict(on = false)), OK)
    assertEquals(send(clientCommands.notouch(on = false)), OK)
    assertEquals(send(clientCommands.kill(servers.clientFilter.id(999999L), 60L)).value, 0L)

    val settings = send(config.get(OneOrMore.unsafeFrom(List(GlobPattern("maxmemory-samples"), GlobPattern("timeout")))))
    val samples  = settings.parameters.`maxmemory-samples`[String].fold(e => fail(e.getMessage), identity)
    assertEquals(send(config.set(OneOrMore.unsafeFrom(List(Key("maxmemory-samples") -> samples)))), OK)
    assert(settings.parameters.timeout[String].isRight)

    assertEquals(
      send(info(OneOrMore.unsafeFrom(List(servers.info.server, servers.info.cpu)))).sections.keySet,
      Set[InfoSectionKey](InfoSection.server, InfoSection.cpu)
    )
  }

  test(s"a $dest client reads the server's info, and finds and lists clients") {
    val v = laserdisc.redis_6_2
    import v.{info, servers}

    val all = send(info).sections
    assert(all.contains(InfoSection.server) && all.contains(InfoSection.keyspace))
    assert(send(info(servers.info.server)).sections(InfoSection.server).redis_version[String].isRight)

    val id = send(v.client.id)
    assert(send(v.client.info).parameters.id[String] == Right(id.value.toString))
    assert(send(v.client.list(OneOrMore.unsafeFrom(List(id)))).clients.size == 1)
    assert(send(v.client.list(servers.clientType.normal)).clients.nonEmpty)
    assertEquals(send(v.client.kill(servers.clientFilter.id(999999L))).value, 0L)
    assert(!send(v.client.unblock(999999L)))
    assertEquals(send(v.client.unpause), OK)
  }
}
