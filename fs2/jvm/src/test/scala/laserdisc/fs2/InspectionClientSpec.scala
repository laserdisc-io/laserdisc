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

final class RedisInspectionClientSpec extends InspectionClientSpec(6379, "redis") {
  test("a redis client reads histograms, lists commands and tracks hot keys with the commands of Redis 8.10") {
    val v = redis_8_10
    import v.{hotkeys, servertypes}

    assert(send(v.latency.histogram(OneOrMore.unsafeFrom(List(Key("ping"))))).keySet.subsetOf(Set("ping")))
    assert(send(v.command.list(servertypes.commandFilter.pattern("xa*"))).contains("xadd"))
    assertEquals(send(v.command.getkeysandflags(v.set("i:a", 1))).map(_.key), Seq(Key("i:a")))
    assert(send(v.module.list).exists(_.name == "search"))

    assertEquals(send(hotkeys.start(servertypes.hotKeysMetrics.cpuAndNet, servertypes.hotKeysOptions.count(5))), OK)
    send(v.set("i:hot", 1))
    send(v.get[Int]("i:hot"))
    assert(send(hotkeys.get).exists(_.exists(_.byNetBytes.nonEmpty)))
    assertEquals(send(hotkeys.stop), OK)
    assertEquals(send(hotkeys.reset), OK)
    assertEquals(send(hotkeys.get), None)
  }

  test("a redis client starts, reads and aborts a backup with the commands of Redis 8.10") {
    val v = redis_8_10
    import v.backup

    assertEquals(send(backup.start), OK)
    assert(send(backup.status).state != "idle")
    assert(send(backup.list).forall(_.nonEmpty))
    assertEquals(send(backup.abort), OK)
    assertEquals(send(backup.status).error, "aborted by user")
    assertEquals(send(backup.cleanup), OK)
    assertEquals(send(backup.status).state, "idle")
  }
}

final class KeyDbInspectionClientSpec extends InspectionClientSpec(6380, "keyDb")

final class ValkeyInspectionClientSpec extends InspectionClientSpec(6381, "valkey") {
  test("a valkey client reads the command log with the commands of Valkey 9.1") {
    val v = valkey_9_1
    import v.{commandlog, servertypes}

    assert(send(commandlog.len(servertypes.commandLogType.slow)).value >= 0L)
    assert(send(commandlog.get(servertypes.commandLogType.slow, 5)).size <= 5)
    assert(send(commandlog.get(servertypes.commandLogType.largeReply)).forall(_.arguments.nonEmpty))
  }
}

abstract class InspectionClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  test(s"a $dest client reads the slow log, memory use, latency, commands and modules") {
    val v = laserdisc.redis_6_2
    import v.{latency, memory, slowlog}

    send(v.set("i:m", "some value"))
    assert(send(slowlog.len).value >= 0L)
    assert(send(slowlog.get(5)).size <= 5)
    assert(send(memory.usage("i:m")).exists(_.value > 0L))
    assertEquals(send(memory.usage("i:none")), None)
    assert(send(memory.stats).values.contains("peak.allocated"))
    assert(send(memory.doctor).nonEmpty)
    assert(send(latency.doctor).nonEmpty)
    assert(send(latency.latest).forall(_.name.nonEmpty))
    assert(send(v.command.count).value > 100)
    assertEquals(send(v.command.info(OneOrMore.unsafeFrom(List(Key("get"), Key("nosuch"))))).map(_.map(_.name)), Seq(Some("get"), None))
    assertEquals(send(v.command.getkeys(v.mset(OneOrMore.unsafeFrom(List(Key("i:a") -> 1, Key("i:b") -> 2))))), Seq(Key("i:a"), Key("i:b")))
    assert(send(v.module.list).forall(_.name.nonEmpty))
  }
}
