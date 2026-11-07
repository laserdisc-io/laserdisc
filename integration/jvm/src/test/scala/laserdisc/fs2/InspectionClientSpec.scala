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

final class RedisInspectionClientSpec extends InspectionClientSpec(LiveServers.port(6379), "redis") {
  since[Redis7_0]("a redis client reads histograms and lists commands") { v =>
    import v.servertypes

    assert(send(v.latency.histogram(OneOrMore.unsafeFrom(List(Key("ping"))))).keySet.subsetOf(Set("ping")))
    assert(send(v.command.list(servertypes.commandFilter.pattern("xa*"))).contains("xadd"))
    assertEquals(send(v.command.getkeysandflags(v.set("i:a", 1))).map(_.key), Seq(Key("i:a")))
  }

  since[Redis8_0]("a redis client lists the modules Redis 8 comes with") { v =>
    assert(send(v.module.list).exists(_.name == "search"))
  }

  since[Redis8_6]("a redis client tracks hot keys") { v =>
    import v.{hotkeys, servertypes}

    assertEquals(send(hotkeys.start(servertypes.hotKeysMetrics.cpuAndNet, servertypes.hotKeysOptions.count(5))), OK)
    send(v.set("i:hot", 1))
    send(v.get[Int]("i:hot"))
    assert(send(hotkeys.get).exists(_.exists(_.byNetBytes.nonEmpty)))
    assertEquals(send(hotkeys.stop), OK)
    assertEquals(send(hotkeys.reset), OK)
    assertEquals(send(hotkeys.get), None)
  }

  since[Redis7_0]("a redis client reads the documentation of every command")(v => docs(v))

  since[Redis7_0]("a redis client reads the information of every command")(v => commandInfos(v))

  since[Redis8_10]("a redis client reads how many arguments a slow command had") { v =>
    assert(slowEntry(v).totalArguments.nonEmpty)
  }

  since[Redis6_2]("a redis client takes its art") { v =>
    assert(send(v.lolwut).nonEmpty)
    assert(send(v.lolwut(8)).nonEmpty)
  }

  since[Redis8_10]("a redis client imports a hash from a fieldset") { v =>
    val fields = OneOrMoreKeys.unsafeFrom(List(Key("z"), Key("a"), Key("m")))

    send(v.del("i:hi"))
    assertEquals(send(v.himportprepare("i:fs", fields)), OK)
    assertEquals(send(v.himportset("i:hi", "i:fs", OneOrMore.unsafeFrom(List("one", "two", "three")))), OK)
    assertEquals(send(v.hget[String]("i:hi", "z")), Some("one"))
    assertEquals(send(v.hget[String]("i:hi", "a")), Some("two"))
    assertEquals(send(v.hget[String]("i:hi", "m")), Some("three"))
    assert(failure(v.himportset("i:hi", "i:fs", OneOrMore.unsafeFrom(List("too few")))).contains("does not match"))
    assertEquals(send(v.himportdiscard("i:fs")), true)
    assertEquals(send(v.himportdiscard("i:fs")), false)
    assert(failure(v.himportset("i:hi", "i:fs", OneOrMore.unsafeFrom(List("x")))).contains("no such fieldset"))
    send(v.himportprepare("i:fs2", fields))
    assertEquals(send(v.himportdiscardall), NonNegInt(1))
    send(v.del("i:hi"))
  }

  since[Redis7_0]("a redis client reads the help of FUNCTION")(v => assert(send(v.function.help).nonEmpty))

  since[Redis8_6]("a redis client reads the help of HOTKEYS")(v => assert(send(v.hotkeys.help).nonEmpty))

  since[Redis8_10]("a redis client reads the help of BACKUP")(v => assert(send(v.backup.help).nonEmpty))

  since[Redis8_10]("a redis client starts, reads and aborts a backup") { v =>
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

final class KeyDbInspectionClientSpec extends InspectionClientSpec(LiveServers.port(6380), "keyDb") {
  test("a keyDb client reads a command with no tips, key specifications or subcommands") {
    val info = send(redis_6_2.command.info(OneOrMore.unsafeFrom(List(Key("get"))))).flatten
    assertEquals(info.map(i => (i.name, i.tips, i.keySpecifications, i.subcommands)), Seq(("get", Nil, Nil, Nil)))
  }
}

final class ValkeyInspectionClientSpec extends InspectionClientSpec(LiveServers.port(6381), "valkey") {
  since[Valkey7_2]("a valkey client reads the documentation of every command")(v => docs(v))

  since[Valkey7_2]("a valkey client reads the information of every command")(v => commandInfos(v))

  since[Valkey7_2]("a valkey client reads a slow command with no total of arguments") { v =>
    assertEquals(slowEntry(v).totalArguments, None)
  }

  since[Valkey7_2]("a valkey client takes its art, and reads the help of FUNCTION") { v =>
    assert(send(v.lolwut).nonEmpty)
    assert(send(v.lolwut(8)).nonEmpty)
    assert(send(v.function.help).nonEmpty)
  }

  since[Valkey8_0]("a valkey client tells what its connection can do") { v =>
    assertEquals(send(v.client.capa(OneOrMore.unsafeFrom(List(Key("redirect"))))), OK)
  }

  since[Valkey8_1]("a valkey client turns import mode off, and reads the help of COMMANDLOG") { v =>
    assertEquals(send(v.client.importsource(on = false)), OK)
    assert(failure(v.client.importsource(on = true)).contains("import mode"))
    assert(send(v.commandlog.help).nonEmpty)
  }

  since[Valkey8_1]("a valkey client reads the command log") { v =>
    import v.{commandlog, servertypes}

    assert(send(commandlog.len(servertypes.commandLogType.slow)).value >= 0L)
    assert(send(commandlog.get(servertypes.commandLogType.slow, 5)).size <= 5)
    assert(send(commandlog.get(servertypes.commandLogType.largeReply)).forall(_.arguments.nonEmpty))
  }
}

abstract class InspectionClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  import protocol.ServerInspection
  import protocol.ServerInspection.{BeginSearch, FindKeys}

  protected final def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  protected final def failure[A](protocol: Protocol.Aux[A]): String =
    client.send(protocol).unsafeRunSync()(using runtime).fold(_.getMessage, a => fail(s"$dest should have failed but replied $a"))

  protected final def commandInfos(v: Redis7_0): Unit = {
    val names = send(v.command.docs).map(d => Key.unsafeFrom(d.name)).toList
    val all   = send(v.command.info(OneOrMore.unsafeFrom(names))).flatten
    assertEquals(all.size, names.size)

    val get = all.find(_.name == "get").map(_.keySpecifications.map(k => k.beginSearch -> k.findKeys))
    assertEquals(get, Some(Seq[(BeginSearch, FindKeys)](BeginSearch.Index(1) -> FindKeys.Range(0, 1, 0))))
    assert(all.flatMap(_.keySpecifications).exists(_.beginSearch.isInstanceOf[BeginSearch.Keyword]))
    assert(all.flatMap(_.keySpecifications).exists(_.findKeys.isInstanceOf[FindKeys.KeyNum]))
    assert(all.exists(_.tips.nonEmpty))
    assert(all.find(_.name == "config").exists(_.subcommands.exists(_.name == "config|help")))
  }

  // every command is slow for a moment, so that there's an entry to read
  protected final def slowEntry(v: Redis7_0): ServerInspection.LogEntry = {
    def slowerThan(micros: String) = send(v.config.set(OneOrMore.unsafeFrom(List(Key("slowlog-log-slower-than") -> micros))))
    slowerThan("0")
    try send(v.slowlog.get(1)).head
    finally slowerThan("10000")
  }

  protected final def docs(v: Redis7_0): Unit = {
    val everyCommand = send(v.command.docs)
    assert(everyCommand.size > 100, everyCommand.size)

    val xinfo = send(v.command.docs(OneOrMore.unsafeFrom(List(Key("xinfo"), Key("nosuchcommand")))))
    assertEquals(xinfo.map(_.name), Seq("xinfo"))
    assert(xinfo.flatMap(_.subcommands).exists(_.name == "xinfo|groups"), xinfo)
    val set = send(v.command.docs(OneOrMore.unsafeFrom(List(Key("set"))))).head
    assert(set.summary.nonEmpty && set.history.nonEmpty && set.arguments.exists(_.arguments.nonEmpty), set)
  }

  test(s"a $dest client gets what a string holds with SUBSTR, checks its HyperLogLog, and is told when it can't load modules") {
    val v = laserdisc.redis_6_2

    send(v.set("i:substr", "hello"))
    assertEquals(send(v.substr[String]("i:substr", 1L, 3L)), "ell")
    send(v.del("i:substr"))
    assertEquals(send(v.pfselftest), OK)
    assert(failure(v.module.load("/nosuchmodule.so")).nonEmpty)
    assert(failure(v.module.load("/nosuchmodule.so", OneOrMore.unsafeFrom(List[Value](1)))).nonEmpty)
    assert(failure(v.module.unload("nosuchmodule")).nonEmpty)
  }

  test(s"a $dest client reads the help of each family") {
    val v = laserdisc.redis_6_2
    List(
      v.acl.help,
      v.client.help,
      v.command.help,
      v.config.help,
      v.latency.help,
      v.memory.help,
      v.module.help,
      v.obj.help,
      v.pubsub.help,
      v.script.help,
      v.slowlog.help,
      v.xgrouphelp,
      v.xinfohelp
    ).foreach(help => assert(send(help).nonEmpty))
  }

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
    assert(send(v.role).isInstanceOf[protocol.ServerP.Role.Master])
  }
}
