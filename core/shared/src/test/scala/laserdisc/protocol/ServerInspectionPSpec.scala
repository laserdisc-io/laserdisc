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

final class ServerInspectionPSpec extends BaseSpec with ServerP {
  import ServerInspection.*

  private def bulks(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)

  test("The Server protocol using slowlog.get reads the entries as Redis sends them") {
    val reply = Arr(
      Arr(Num(7L), Num(1793582639L), Num(36L), bulks("SET", "a", "1"), Bulk("172.17.0.1:56480"), Bulk("")),
      Arr(Num(6L), Num(1793582638L), Num(160L), bulks("CONFIG", "SET", "x", "0"), Bulk("172.17.0.1:56480"), Bulk("me"), Num(4L))
    )
    slowlog.get(PosInt(2)).decode(reply).onRight { entries =>
      entries.map(e => (e.id.value, e.microseconds.value, e.arguments, e.clientName)) ==
        Seq((7L, 36L, Seq("SET", "a", "1"), ""), (6L, 160L, Seq("CONFIG", "SET", "x", "0"), "me"))
    }
    assertEquals(slowlog.get(PosInt(2)).encode, bulks("SLOWLOG", "GET", "2"))
    assertEquals(commandlog.get(servers.commandLogType.largeReply).encode, bulks("COMMANDLOG", "GET", "-1", "large-reply"))
  }

  test("The Server protocol using latency reads events, samples and histograms") {
    latency.latest.decode(Arr(Arr(Bulk("command"), Num(1793582671L), Num(90L), Num(95L)))).onRight {
      _ == Seq(LatencyEvent("command", NonNegLong(1793582671L), NonNegLong(90L), NonNegLong(95L)))
    }
    latency.history(Key("command")).decode(Arr(Arr(Num(1793582639L), Num(2L)), Arr(Num(1793582671L), Num(90L)))).onRight {
      _.map(_.milliseconds.value) == Seq(2L, 90L)
    }
    val histogram = Arr(Bulk("set"), Arr(Bulk("calls"), Num(10L), Bulk("histogram_usec"), Arr(Num(1L), Num(4L), Num(2L), Num(10L))))
    latency.histogram(OneOrMore.unsafeFrom(List(Key("set")))).decode(histogram).onRight {
      _ == Map("set" -> LatencyHistogram(NonNegLong(10L), Seq(NonNegLong(1L) -> NonNegLong(4L), NonNegLong(2L) -> NonNegLong(10L))))
    }
    assertEquals(latency.reset(OneOrMore.unsafeFrom(List(Key("command")))).encode, bulks("LATENCY", "RESET", "command"))
  }

  test("The Server protocol using memory reads usage and stats, with the figures of each database") {
    assertEquals(memory.usage(Key("a"), NonNegInt(0)).encode, bulks("MEMORY", "USAGE", "a", "SAMPLES", "0"))
    assertEquals(memory.usage(Key("a")).decode(NullBulk), None)
    val stats = Arr(
      Bulk("peak.allocated"),
      Num(5418656L),
      Bulk("db.0"),
      Arr(Bulk("overhead.hashtable.main"), Num(1896L), Bulk("overhead.hashtable.expires"), Num(328L)),
      Bulk("dataset.percentage"),
      Bulk("98.03681182861328")
    )
    memory.stats.decode(stats).onRight { case MemoryStats(values, databases) =>
      values == Map("peak.allocated" -> "5418656", "dataset.percentage" -> "98.03681182861328") &&
      databases == Map(0 -> Map("overhead.hashtable.main" -> "1896", "overhead.hashtable.expires" -> "328"))
    }
  }

  test("The Server protocol using command reads the information of commands, as Redis 6.2 and Redis 7.0 send it") {
    val redis62 = Arr(Bulk("get"), Num(2L), Arr(Str("readonly"), Str("fast")), Num(1L), Num(1L), Num(1L), Arr(Str("@read"), Str("@string")))
    val redis70 =
      Arr(Bulk("get"), Num(2L), Arr(Str("readonly")), Num(1L), Num(1L), Num(1L), Arr(Str("@read")), Arr(Nil), Arr(Nil), Arr(Nil))
    command.info(OneOrMore.unsafeFrom(List(Key("get"), Key("nosuch")))).decode(Arr(redis62, NullBulk)).onRight {
      _ == Seq(Some(CommandInfo("get", 2, Seq("readonly", "fast"), 1, 1, 1, Seq("@read", "@string"))), None)
    }
    command.info(OneOrMore.unsafeFrom(List(Key("get")))).decode(Arr(redis70)).onRight {
      _ == Seq(Some(CommandInfo("get", 2, Seq("readonly"), 1, 1, 1, Seq("@read"))))
    }
  }

  test("The Server protocol using command.getkeys sends the command it is given") {
    import laserdisc.auto.*
    val lmove = families.lists.lmove[String]("a", "b", families.lists.listtypes.ListSide.left, families.lists.listtypes.ListSide.right)
    assertEquals(command.getkeys(lmove).encode, bulks("COMMAND", "GETKEYS", "LMOVE", "a", "b", "LEFT", "RIGHT"))
    command.getkeysandflags(lmove).decode(Arr(Arr(Bulk("a"), Arr(Str("RW"), Str("access"))), Arr(Bulk("b"), Arr(Str("RW"))))).onRight {
      _ == Seq(KeyAndFlags(Key("a"), Seq("RW", "access")), KeyAndFlags(Key("b"), Seq("RW")))
    }
    assertEquals(
      command.list(servers.commandFilter.pattern(GlobPattern("xa*"))).encode,
      bulks("COMMAND", "LIST", "FILTERBY", "PATTERN", "xa*")
    )
  }

  test("The Server protocol using module.list reads modules with and without a path") {
    val reply = Arr(
      Arr(Bulk("name"), Bulk("search"), Bulk("ver"), Num(81001L), Bulk("path"), Bulk("/modules/redisearch.so"), Bulk("args"), Arr(Nil)),
      Arr(Bulk("name"), Bulk("old"), Bulk("ver"), Num(1L))
    )
    module.list.decode(reply).onRight {
      _ == Seq(ModuleInfo("search", 81001L, Some("/modules/redisearch.so"), Nil), ModuleInfo("old", 1L, None, Nil))
    }
  }

  test("The Server protocol using hotkeys sends the metrics and options, and reads the report or None") {
    assertEquals(
      hotkeys.start(servers.hotKeysMetrics.cpuAndNet, servers.hotKeysOptions.count(PosInt(5)).duration(PosInt(10))).encode,
      bulks("HOTKEYS", "START", "METRICS", "2", "CPU", "NET", "COUNT", "5", "DURATION", "10")
    )
    val report = Arr(
      Arr(
        Bulk("tracking-active"),
        Num(1L),
        Bulk("selected-slots"),
        Arr(Arr(Num(0L), Num(16383L))),
        Bulk("by-cpu-time-us"),
        Arr(Bulk("b"), Num(25L), Bulk("a"), Num(12L)),
        Bulk("by-net-bytes"),
        Arr(Bulk("a"), Num(28L))
      )
    )
    hotkeys.get.decode(report).onRight {
      _ == Some(
        Seq(
          HotKeys(
            Map("tracking-active" -> "1"),
            Seq(Key("b")          -> NonNegLong(25L), Key("a") -> NonNegLong(12L)),
            Seq(Key("a")          -> NonNegLong(28L))
          )
        )
      )
    }
    assertEquals(hotkeys.get.decode(NullBulk), None)
  }
}
