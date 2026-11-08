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

import Protocol.Single

/** The replies and options of the commands that inspect a server: SLOWLOG, LATENCY, MEMORY, COMMAND, MODULE, HOTKEYS and COMMANDLOG */
object ServerInspection {

  /** An entry of SLOWLOG GET or COMMANDLOG GET, as Redis 6.2 and Valkey send it. A long command's `arguments` are cut short */
  sealed trait LogEntry6_2 {
    def id: NonNegLong
    def timestamp: NonNegLong
    def microseconds: NonNegLong
    def arguments: Seq[String]
    def clientAddress: String
    def clientName: String
  }

  /** An entry of SLOWLOG GET from Redis 8.10, which also replies with how many arguments the command had */
  sealed trait LogEntry8_10 extends LogEntry6_2 {
    def totalArguments: NonNegLong
  }

  private[laserdisc] final case class Entry6_2(
      id: NonNegLong,
      timestamp: NonNegLong,
      microseconds: NonNegLong,
      arguments: Seq[String],
      clientAddress: String,
      clientName: String
  ) extends LogEntry6_2
  private[laserdisc] final case class Entry8_10(
      id: NonNegLong,
      timestamp: NonNegLong,
      microseconds: NonNegLong,
      arguments: Seq[String],
      clientAddress: String,
      clientName: String,
      totalArguments: NonNegLong
  ) extends LogEntry8_10

  private final type LogEntryReply  = Arr.AsTupleThen[Num *: Num *: Num *: Arr.Of[Bulk] *: Bulk *: Bulk *: EmptyTuple, RESP]
  private final type LogEntryFields =
    (NonNegLong *: NonNegLong *: NonNegLong *: Seq[String] *: String *: String *: EmptyTuple, Seq[RESP])

  // what follows the fields is left alone, as later versions add to it
  private implicit final val logEntry6_2: Read[LogEntryReply, LogEntry6_2] = Read[LogEntryReply, LogEntryFields].map {
    case (id *: timestamp *: microseconds *: arguments *: address *: name *: _, _) =>
      Entry6_2(id, timestamp, microseconds, arguments, address, name)
  }
  private final val logEntry8_10: Read[LogEntryReply, LogEntry8_10] = Read.instance { reply =>
    Read[LogEntryReply, LogEntryFields].read(reply).flatMap {
      case (id *: timestamp *: microseconds *: arguments *: address *: name *: _, Num(NonNegLong(total)) +: _) =>
        Right(Entry8_10(id, timestamp, microseconds, arguments, address, name, total))
      case (_, rest) => Left(RESPDecErr(s"Expected the total of the arguments after the entry's fields but got: $rest"))
    }
  }
  private[protocol] final val logEntries6_2: Read[Arr.Of[LogEntryReply], Seq[LogEntry6_2]]   = Read[Arr.Of[LogEntryReply], Seq[LogEntry6_2]]
  private[protocol] final val logEntries8_10: Read[Arr.Of[LogEntryReply], Seq[LogEntry8_10]] =
    Read.arrOf2Seq[LogEntryReply, LogEntry8_10](using implicitly, logEntry8_10)

  /** An event of LATENCY LATEST: when it last happened, how long it took then, and the longest it ever took */
  final case class LatencyEvent(name: String, timestamp: NonNegLong, latestMilliseconds: NonNegLong, maxMilliseconds: NonNegLong)
  private final type LatencyEventReply = Arr.AsTupleThen[Bulk *: Num *: Num *: Num *: EmptyTuple, RESP]
  private implicit final val latencyEvent: Read[LatencyEventReply, LatencyEvent] =
    Read[LatencyEventReply, String *: NonNegLong *: NonNegLong *: NonNegLong *: EmptyTuple].map {
      case name *: timestamp *: latest *: max *: _ =>
        LatencyEvent(name, timestamp, latest, max)
    }
  private[protocol] final val latencyEvents: Read[Arr.Of[LatencyEventReply], Seq[LatencyEvent]] =
    Read[Arr.Of[LatencyEventReply], Seq[LatencyEvent]]

  /** A sample of LATENCY HISTORY */
  final case class LatencySample(timestamp: NonNegLong, milliseconds: NonNegLong)
  private implicit final val latencySample: Read[Arr.AsTuple[Num *: Num *: EmptyTuple], LatencySample] =
    Read[Arr.AsTuple[Num *: Num *: EmptyTuple], NonNegLong *: NonNegLong *: EmptyTuple].map { case timestamp *: milliseconds *: _ =>
      LatencySample(timestamp, milliseconds)
    }
  private[protocol] final val latencySamples: Read[Arr.Of[Arr.AsTuple[Num *: Num *: EmptyTuple]], Seq[LatencySample]] =
    Read[Arr.Of[Arr.AsTuple[Num *: Num *: EmptyTuple]], Seq[LatencySample]]

  /** A command's LATENCY HISTOGRAM: how many calls it had, and for each bucket the number of calls that took at most that many microseconds
    */
  final case class LatencyHistogram(calls: NonNegLong, buckets: Seq[(NonNegLong, NonNegLong)])
  private implicit final val latencyHistogram: Read[Record, LatencyHistogram] = Read.record("latency histogram") { f =>
    for {
      calls   <- f.num[NonNegLong]("calls")
      buckets <- f[Arr.OfPairs[Num, Num], Seq[(NonNegLong, NonNegLong)]]("histogram_usec")
    } yield LatencyHistogram(calls, buckets)
  }
  private[protocol] final val latencyHistograms: Read[Arr.OfPairs[Bulk, Record], Map[String, LatencyHistogram]] =
    Read[Arr.OfPairs[Bulk, Record], Map[String, LatencyHistogram]]

  /** MEMORY STATS: the figures for the whole server, and those of each database, as text since they mix whole and decimal numbers */
  final case class MemoryStats(values: Map[String, String], databases: Map[Int, Map[String, String]])
  private[protocol] final val memoryStats: Read[Record, MemoryStats] = {
    implicit val value: Read[RESP, String] = Read.instancePF("a number") { case Num(n) => n.toString; case Bulk(s) => s }
    val database                           = Read[Record, Map[String, String]]
    val Db                                 = raw"db\.(\d+)".r
    Read.record("memory stats") {
      _.others(Set.empty).foldLeft[Either[RESPDecErr, MemoryStats]](Right(MemoryStats(Map.empty, Map.empty))) {
        case (Right(stats), (k @ Db(n), fs: Arr)) =>
          database.read(new Arr.OfPairs(fs.elements)) match {
            case Right(d) => Right(stats.copy(databases = stats.databases + (n.toInt -> d)))
            case Left(e)  => Left(RESPDecErr(s"field $k: ${e.message}"))
          }
        case (Right(stats), (k, v)) =>
          value.read(v) match {
            case Right(s) => Right(stats.copy(values = stats.values + (k -> s)))
            case Left(e)  => Left(RESPDecErr(s"field $k: ${e.message}"))
          }
        case (left, _) => left
      }
    }
  }

  /** An argument of COMMAND DOCS: its name, its type (like `key` or `oneof`), what it's shown as, which of the command's key specifications
    * it's for, the word that comes before it, its summary, when it arrived and when it was deprecated, its flags (like `optional`) and the
    * arguments inside it
    */
  final case class CommandArgument(
      name: String,
      argumentType: String,
      displayText: Option[String],
      keySpecIndex: Option[NonNegInt],
      token: Option[String],
      summary: Option[String],
      since: Option[String],
      deprecatedSince: Option[String],
      flags: Seq[String],
      arguments: Seq[CommandArgument]
  )

  /** COMMAND DOCS of a command. Commands of modules leave out much of it
    *
    * @param module
    *   the module the command comes from
    * @param replacedBy
    *   what to use instead of a deprecated command
    * @param history
    *   each version that changed the command, with what changed
    */
  final case class CommandDoc(
      name: String,
      summary: Option[String],
      since: Option[String],
      group: Option[String],
      complexity: Option[String],
      module: Option[String],
      flags: Seq[String],
      deprecatedSince: Option[String],
      replacedBy: Option[String],
      history: Seq[(String, String)],
      arguments: Seq[CommandArgument],
      subcommands: Seq[CommandDoc]
  )

  private implicit lazy val commandArgument: Read[Record, CommandArgument] = Read.record("command argument") { f =>
    for {
      name            <- f.bulk[String]("name")
      argumentType    <- f.bulk[String]("type")
      displayText     <- f.optionalBulk[String]("display_text")
      keySpecIndex    <- f.optionalNum[NonNegInt]("key_spec_index")
      token           <- f.optionalBulk[String]("token")
      summary         <- f.optionalBulk[String]("summary")
      since           <- f.optionalBulk[String]("since")
      deprecatedSince <- f.optionalBulk[String]("deprecated_since")
      flags           <- f.optional[Texts, Seq[String]]("flags")
      arguments       <- f.optional[Records, Seq[CommandArgument]]("arguments")
    } yield CommandArgument(
      name,
      argumentType,
      displayText,
      keySpecIndex,
      token,
      summary,
      since,
      deprecatedSince,
      flags.getOrElse(Nil),
      arguments.getOrElse(Nil)
    )
  }

  // a reply gives the name of a command next to the rest of it
  private implicit lazy val unnamedCommandDoc: Read[Record, String => CommandDoc] = Read.record("command docs") { f =>
    for {
      summary         <- f.optionalBulk[String]("summary")
      since           <- f.optionalBulk[String]("since")
      group           <- f.optionalBulk[String]("group")
      complexity      <- f.optionalBulk[String]("complexity")
      module          <- f.optionalBulk[String]("module")
      flags           <- f.optional[Texts, Seq[String]]("doc_flags")
      deprecatedSince <- f.optionalBulk[String]("deprecated_since")
      replacedBy      <- f.optionalBulk[String]("replaced_by")
      history         <- f.optional[Arr.Of[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple]], Seq[String *: String *: EmptyTuple]]("history")
      arguments       <- f.optional[Records, Seq[CommandArgument]]("arguments")
      subcommands     <- f.optional[Arr.OfPairs[Bulk, Record], Seq[(String, String => CommandDoc)]]("subcommands")
    } yield CommandDoc(
      _,
      summary,
      since,
      group,
      complexity,
      module,
      flags.getOrElse(Nil),
      deprecatedSince,
      replacedBy,
      history.getOrElse(Nil).map { case version *: change *: _ => version -> change },
      arguments.getOrElse(Nil),
      subcommands.getOrElse(Nil).map { case (name, doc) => doc(name) }
    )
  }

  private[protocol] final lazy val commandDocs: Read[Arr.OfPairs[Bulk, Record], Seq[CommandDoc]] =
    Read[Arr.OfPairs[Bulk, Record], Seq[(String, String => CommandDoc)]].map(_.map { case (name, doc) => doc(name) })

  /** Where the search for a command's keys starts: at a position, or after a keyword looked for from a position. `Unknown` is for commands
    * whose keys can't be found this way, and for any other kind a server may add
    */
  sealed trait BeginSearch
  object BeginSearch {
    final case class Index(position: Int)                     extends BeginSearch
    final case class Keyword(keyword: String, startFrom: Int) extends BeginSearch
    case object Unknown                                       extends BeginSearch

    private val index: Read[Record, BeginSearch]   = Read.record("index")(_.num[Int]("index").map(Index(_)))
    private val keyword: Read[Record, BeginSearch] = Read.record("keyword") { f =>
      for {
        keyword   <- f.bulk[String]("keyword")
        startFrom <- f.num[Int]("startfrom")
      } yield Keyword(keyword, startFrom)
    }
    private[ServerInspection] val read: Read[Record, BeginSearch] = Read.record("begin_search") { f =>
      f.bulk[String]("type").flatMap {
        case "index"   => f[Record, BeginSearch]("spec")(using implicitly, index)
        case "keyword" => f[Record, BeginSearch]("spec")(using implicitly, keyword)
        case _         => Right(Unknown)
      }
    }
  }

  /** How to find a command's keys once the search has started: up to a last position (negative when counted from the end), or by a count of
    * keys at a position. `Unknown` is for commands whose keys can't be found this way, and for any other kind a server may add
    */
  sealed trait FindKeys
  object FindKeys {
    final case class Range(lastKey: Int, keyStep: Int, limit: Int)           extends FindKeys
    final case class KeyNum(keyCountIndex: Int, firstKey: Int, keyStep: Int) extends FindKeys
    case object Unknown                                                      extends FindKeys

    private val range: Read[Record, FindKeys] = Read.record("range") { f =>
      for {
        lastKey <- f.num[Int]("lastkey")
        keyStep <- f.num[Int]("keystep")
        limit   <- f.num[Int]("limit")
      } yield Range(lastKey, keyStep, limit)
    }
    private val keyNum: Read[Record, FindKeys] = Read.record("keynum") { f =>
      for {
        keyCountIndex <- f.num[Int]("keynumidx")
        firstKey      <- f.num[Int]("firstkey")
        keyStep       <- f.num[Int]("keystep")
      } yield KeyNum(keyCountIndex, firstKey, keyStep)
    }
    private[ServerInspection] val read: Read[Record, FindKeys] = Read.record("find_keys") { f =>
      f.bulk[String]("type").flatMap {
        case "range"  => f[Record, FindKeys]("spec")(using implicitly, range)
        case "keynum" => f[Record, FindKeys]("spec")(using implicitly, keyNum)
        case _        => Right(Unknown)
      }
    }
  }

  /** Where a command's keys are and what it does with them */
  final case class KeySpecification(notes: Option[String], flags: Seq[String], beginSearch: BeginSearch, findKeys: FindKeys)
  private implicit final val keySpecification: Read[Record, KeySpecification] = Read.record("key specification") { f =>
    for {
      notes       <- f.optionalBulk[String]("notes")
      flags       <- f[Arr.Of[Str], Seq[String]]("flags")
      beginSearch <- f[Record, BeginSearch]("begin_search")(using implicitly, BeginSearch.read)
      findKeys    <- f[Record, FindKeys]("find_keys")(using implicitly, FindKeys.read)
    } yield KeySpecification(notes, flags, beginSearch, findKeys)
  }

  /** COMMAND INFO of a command: its arity (negative when it takes at least that many arguments), flags, where its keys are and its ACL
    * categories
    */
  sealed trait CommandInfo6_2 {
    def name: String
    def arity: Int
    def flags: Seq[String]
    def firstKey: Int
    def lastKey: Int
    def step: Int
    def aclCategories: Seq[String]
  }

  /** COMMAND INFO of a command from Redis 7.0: also its tips, key specifications and subcommands */
  sealed trait CommandInfo7_0 extends CommandInfo6_2 {
    def tips: Seq[String]
    def keySpecifications: Seq[KeySpecification]
    def subcommands: Seq[CommandInfo7_0]
  }

  private[laserdisc] final case class CommandInfoOf6_2(
      name: String,
      arity: Int,
      flags: Seq[String],
      firstKey: Int,
      lastKey: Int,
      step: Int,
      aclCategories: Seq[String]
  ) extends CommandInfo6_2
  private[laserdisc] final case class CommandInfoOf7_0(
      name: String,
      arity: Int,
      flags: Seq[String],
      firstKey: Int,
      lastKey: Int,
      step: Int,
      aclCategories: Seq[String],
      tips: Seq[String],
      keySpecifications: Seq[KeySpecification],
      subcommands: Seq[CommandInfo7_0]
  ) extends CommandInfo7_0

  private type CommandInfoReply =
    Arr.AsTupleThen[OneOf[Bulk *: Str *: EmptyTuple] *: Num *: Arr.Of[Str] *: Num *: Num *: Num *: Arr.Of[Str] *: EmptyTuple, RESP]
  private final val commandStart =
    Read[CommandInfoReply, (String *: Int *: Seq[String] *: Int *: Int *: Int *: Seq[String] *: EmptyTuple, Seq[RESP])]
  // what follows the first seven fields is left alone, as later versions add to it
  private implicit final val commandInfo6_2: Read[CommandInfoReply, CommandInfo6_2] = commandStart.map {
    case (name *: arity *: flags *: first *: last *: step *: categories *: _, _) =>
      CommandInfoOf6_2(name, arity, flags, first, last, step, categories)
  }
  private implicit final val commandInfo7_0: Read[CommandInfoReply, CommandInfo7_0] = Read.instance { reply =>
    commandStart.read(reply).flatMap { case (name *: arity *: flags *: first *: last *: step *: categories *: _, rest) =>
      val f = new Fields(List("tips", "key specifications", "subcommands").zip(rest).toMap)
      for {
        tips        <- f[Arr.Of[Bulk], Seq[String]]("tips")
        keySpecs    <- f[Records, Seq[KeySpecification]]("key specifications")
        subcommands <- f[Arr.Of[CommandInfoReply], Seq[CommandInfo7_0]]("subcommands")
      } yield CommandInfoOf7_0(name, arity, flags, first, last, step, categories, tips, keySpecs, subcommands)
    }
  }
  private[protocol] final val commandInfos6_2: Read[Arr.Of[OrNullBulk[CommandInfoReply]], Seq[Option[CommandInfo6_2]]] =
    Read[Arr.Of[OrNullBulk[CommandInfoReply]], Seq[Option[CommandInfo6_2]]]
  private[protocol] final val commandInfos7_0: Read[Arr.Of[OrNullBulk[CommandInfoReply]], Seq[Option[CommandInfo7_0]]] =
    Read[Arr.Of[OrNullBulk[CommandInfoReply]], Seq[Option[CommandInfo7_0]]]

  /** A key of COMMAND GETKEYSANDFLAGS, with what the command does to it */
  final case class KeyAndFlags(key: Key, flags: Seq[String])
  private implicit final val keyAndFlags: Read[Arr.AsTuple[Bulk *: Arr.Of[Str] *: EmptyTuple], KeyAndFlags] =
    Read[Arr.AsTuple[Bulk *: Arr.Of[Str] *: EmptyTuple], Key *: Seq[String] *: EmptyTuple].map { case key *: flags *: _ =>
      KeyAndFlags(key, flags)
    }
  private[protocol] final val keysAndFlags: Read[Arr.Of[Arr.AsTuple[Bulk *: Arr.Of[Str] *: EmptyTuple]], Seq[KeyAndFlags]] =
    Read[Arr.Of[Arr.AsTuple[Bulk *: Arr.Of[Str] *: EmptyTuple]], Seq[KeyAndFlags]]

  /** FILTERBY, for COMMAND LIST: the commands of a module, of an ACL category, or whose names match a pattern */
  sealed trait CommandFilter  { def params: List[String] }
  trait CommandFilterBuilders {
    def module(name: Key): CommandFilter
    def aclCategory(name: Key): CommandFilter
    def pattern(pattern: GlobPattern): CommandFilter
  }
  private[laserdisc] object CommandFilter extends CommandFilterBuilders {
    private final class Of(override final val params: List[String]) extends CommandFilter

    def module(name: Key): CommandFilter             = new Of(List("FILTERBY", "MODULE", name.value))
    def aclCategory(name: Key): CommandFilter        = new Of(List("FILTERBY", "ACLCAT", name.value))
    def pattern(pattern: GlobPattern): CommandFilter = new Of(List("FILTERBY", "PATTERN", pattern.value))
  }

  /** A module of MODULE LIST: its name and version */
  sealed trait ModuleInfo6_2 {
    def name: String
    def version: Long
  }

  /** A module of MODULE LIST from Redis 7.0: also its path and the arguments it was loaded with */
  sealed trait ModuleInfo7_0 extends ModuleInfo6_2 {
    def path: String
    def arguments: Seq[String]
  }

  private[laserdisc] final case class ModuleInfoOf6_2(name: String, version: Long) extends ModuleInfo6_2
  private[laserdisc] final case class ModuleInfoOf7_0(name: String, version: Long, path: String, arguments: Seq[String])
      extends ModuleInfo7_0

  private final def module(f: Fields): Either[RESPDecErr, ModuleInfoOf6_2] =
    for {
      name    <- f.bulk[String]("name")
      version <- f.num[Long]("ver")
    } yield ModuleInfoOf6_2(name, version)
  private implicit final val moduleInfo6_2: Read[Record, ModuleInfo6_2] = Read.record("module")(module)
  private implicit final val moduleInfo7_0: Read[Record, ModuleInfo7_0] = Read.record("module") { f =>
    for {
      m         <- module(f)
      path      <- f.bulk[String]("path")
      arguments <- f[Texts, Seq[String]]("args")
    } yield ModuleInfoOf7_0(m.name, m.version, path, arguments)
  }

  // Dragonfly's names are simple strings, and it only sends a module's name and version
  private final type DragonflyModuleReply = Arr.AsTuple[Str *: Str *: Str *: Num *: EmptyTuple]
  private implicit final val dragonflyModuleInfo: Read[DragonflyModuleReply, ModuleInfo6_2] =
    Read[DragonflyModuleReply, String *: String *: String *: Long *: EmptyTuple].map { case _ *: name *: _ *: version *: _ =>
      ModuleInfoOf6_2(name, version)
    }
  private[protocol] final val moduleInfos6_2: Read[OneOf[Records *: Arr.Of[DragonflyModuleReply] *: EmptyTuple], Seq[ModuleInfo6_2]] =
    Read[OneOf[Records *: Arr.Of[DragonflyModuleReply] *: EmptyTuple], Seq[ModuleInfo6_2]]
  private[protocol] final val moduleInfos7_0: Read[Records, Seq[ModuleInfo7_0]] = Read[Records, Seq[ModuleInfo7_0]]

  /** HELLO's reply as Dragonfly sends it: the server's name and version, the protocol, this connection's id and how the server runs, with
    * anything else the server sends as text
    */
  sealed trait HelloCore {
    def server: String
    def version: String
    def protocol: Long
    def id: NonNegLong
    def mode: String
    def role: String
    def others: Map[String, String]
  }

  /** HELLO's reply from Redis 6.2: also the server's modules */
  sealed trait Hello6_2 extends HelloCore {
    def modules: Seq[ModuleInfo6_2]
  }

  /** HELLO's reply from Redis 7.0: also each module's path and the arguments it was loaded with */
  sealed trait Hello7_0 extends Hello6_2 {
    override def modules: Seq[ModuleInfo7_0]
  }

  private[laserdisc] final case class HelloOfCore(
      server: String,
      version: String,
      protocol: Long,
      id: NonNegLong,
      mode: String,
      role: String,
      others: Map[String, String]
  ) extends HelloCore
  private[laserdisc] final case class HelloOf6_2(
      server: String,
      version: String,
      protocol: Long,
      id: NonNegLong,
      mode: String,
      role: String,
      modules: Seq[ModuleInfo6_2],
      others: Map[String, String]
  ) extends Hello6_2
  private[laserdisc] final case class HelloOf7_0(
      server: String,
      version: String,
      protocol: Long,
      id: NonNegLong,
      mode: String,
      role: String,
      modules: Seq[ModuleInfo7_0],
      others: Map[String, String]
  ) extends Hello7_0

  private final def helloOf(f: Fields): Either[RESPDecErr, HelloOfCore] =
    for {
      server  <- f.bulk[String]("server")
      version <- f.bulk[String]("version")
      proto   <- f.num[Long]("proto")
      id      <- f.num[NonNegLong]("id")
      mode    <- f.bulk[String]("mode")
      role    <- f.bulk[String]("role")
    } yield {
      val typed  = Set("server", "version", "proto", "id", "mode", "role", "modules")
      val others = f.others(typed).collect { case (k, Bulk(v)) => k -> v }
      HelloOfCore(server, version, proto, id, mode, role, others)
    }
  private[protocol] final val helloCore: Read[Record, HelloCore] = Read.record("hello")(helloOf)
  private[protocol] final val hello6_2: Read[Record, Hello6_2]   = Read.record("hello") { f =>
    for {
      h       <- helloOf(f)
      modules <- f[Records, Seq[ModuleInfo6_2]]("modules")
    } yield HelloOf6_2(h.server, h.version, h.protocol, h.id, h.mode, h.role, modules, h.others)
  }
  private[protocol] final val hello7_0: Read[Record, Hello7_0] = Read.record("hello") { f =>
    for {
      h       <- helloOf(f)
      modules <- f[Records, Seq[ModuleInfo7_0]]("modules")
    } yield HelloOf7_0(h.server, h.version, h.protocol, h.id, h.mode, h.role, modules, h.others)
  }

  /** SLOW, LARGE-REQUEST and LARGE-REPLY, for COMMANDLOG: Valkey 8.1 */
  sealed trait CommandLogType
  trait CommandLogTypeBuilders {
    val slow: CommandLogType
    val largeRequest: CommandLogType
    val largeReply: CommandLogType
  }
  private[laserdisc] object CommandLogType extends CommandLogTypeBuilders {
    object slow         extends CommandLogType
    object largeRequest extends CommandLogType
    object largeReply   extends CommandLogType

    implicit val commandLogTypeShow: Show[CommandLogType] = Show.instance {
      case `slow`         => "slow"
      case `largeRequest` => "large-request"
      case `largeReply`   => "large-reply"
    }
  }

  /** What HOTKEYS START tracks: CPU time, network bytes, or both */
  sealed trait HotKeysMetrics  { def params: List[String] }
  trait HotKeysMetricsBuilders {
    val cpu: HotKeysMetrics
    val net: HotKeysMetrics
    val cpuAndNet: HotKeysMetrics
  }
  private[laserdisc] object HotKeysMetrics extends HotKeysMetricsBuilders {
    private final class Of(override final val params: List[String]) extends HotKeysMetrics

    final val cpu: HotKeysMetrics       = new Of(List("METRICS", "1", "CPU"))
    final val net: HotKeysMetrics       = new Of(List("METRICS", "1", "NET"))
    final val cpuAndNet: HotKeysMetrics = new Of(List("METRICS", "2", "CPU", "NET"))
  }

  /** COUNT, DURATION, SAMPLE and SLOTS, for HOTKEYS START: how many keys to report, for how long to track, 1 in how many commands to look
    * at, and which slots to track
    */
  final class HotKeysOptions private (val params: List[String]) {
    def count(keys: PosInt): HotKeysOptions           = new HotKeysOptions(params ++ List("COUNT", Show[PosInt].show(keys)))
    def duration(seconds: PosInt): HotKeysOptions     = new HotKeysOptions(params ++ List("DURATION", Show[PosInt].show(seconds)))
    def sample(ratio: PosInt): HotKeysOptions         = new HotKeysOptions(params ++ List("SAMPLE", Show[PosInt].show(ratio)))
    def slots(slots: OneOrMore[Slot]): HotKeysOptions =
      new HotKeysOptions(params ++ ("SLOTS" :: Show[Int].show(slots.value.length) :: slots.value.map(Show[Slot].show)))
  }
  trait HotKeysOptionsBuilders {
    def count(keys: PosInt): HotKeysOptions
    def duration(seconds: PosInt): HotKeysOptions
    def sample(ratio: PosInt): HotKeysOptions
    def slots(slots: OneOrMore[Slot]): HotKeysOptions
  }
  private[laserdisc] object HotKeysOptions extends HotKeysOptionsBuilders {
    private final val none = new HotKeysOptions(Nil)

    def count(keys: PosInt): HotKeysOptions           = none.count(keys)
    def duration(seconds: PosInt): HotKeysOptions     = none.duration(seconds)
    def sample(ratio: PosInt): HotKeysOptions         = none.sample(ratio)
    def slots(slots: OneOrMore[Slot]): HotKeysOptions = none.slots(slots)
  }

  /** A report of HOTKEYS GET: the busiest keys by CPU time (in microseconds) and by network bytes, for the metrics being tracked, and the
    * other figures as text
    */
  final case class HotKeys(values: Map[String, String], byCpuTime: Seq[(Key, NonNegLong)], byNetBytes: Seq[(Key, NonNegLong)])
  private implicit final val hotKeysReport: Read[Record, HotKeys] = Read.record("hot keys report") { f =>
    val byCpuTime  = "by-cpu-time-us"
    val byNetBytes = "by-net-bytes"
    for {
      cpu    <- f.optional[Arr.OfPairs[Bulk, Num], Seq[(Key, NonNegLong)]](byCpuTime)
      net    <- f.optional[Arr.OfPairs[Bulk, Num], Seq[(Key, NonNegLong)]](byNetBytes)
      values <- f.others(Set(byCpuTime, byNetBytes)).foldLeft[Either[RESPDecErr, Map[String, String]]](Right(Map.empty)) {
        case (Right(vs), (k, Num(n)))  => Right(vs + (k -> n.toString))
        case (Right(vs), (k, Bulk(v))) => Right(vs + (k -> v))
        case (Right(vs), (_, _: Arr))  => Right(vs)
        case (Right(_), (k, other))    => Left(RESPDecErr(s"field $k: Unexpected value. Was $other"))
        case (left, _)                 => left
      }
    } yield HotKeys(values, cpu.getOrElse(Nil), net.getOrElse(Nil))
  }
  private[protocol] final val maybeHotKeys: RESPRead[Option[Seq[HotKeys]]] = RESPRead[OrNullBulk[Records], Option[Seq[HotKeys]]]

  private[protocol] final def argumentsOf(protocol: Single[Any]): List[String] =
    protocol.command :: protocol.parameters.toList.collect { case Bulk(s) => s }
}

/** SLOWLOG, under `slowlog`: Redis 6.2 */
trait SlowLogCommands {
  import ServerInspection.LogEntry6_2

  val help: Single[Seq[String]] = Protocol.help("SLOWLOG")

  /** The 10 most recent entries */
  val get: Single[Seq[LogEntry6_2]]                = Protocol("SLOWLOG", "GET").using(RESPRead.instance(ServerInspection.logEntries6_2))
  def get(count: PosInt): Single[Seq[LogEntry6_2]] =
    Protocol("SLOWLOG", "GET" *: count *: EmptyTuple).using(RESPRead.instance(ServerInspection.logEntries6_2))
  val len: Single[NonNegLong] = Protocol("SLOWLOG", "LEN").as[Num, NonNegLong]
  val reset: Single[OK]       = Protocol("SLOWLOG", "RESET").as[Str, OK]
}

/** SLOWLOG of Redis 8.10, whose entries also have how many arguments their command had */
trait SlowLogCommands8_10 extends SlowLogCommands {
  import ServerInspection.LogEntry8_10

  override val get: Single[Seq[LogEntry8_10]] = Protocol("SLOWLOG", "GET").using(RESPRead.instance(ServerInspection.logEntries8_10))
  override def get(count: PosInt): Single[Seq[LogEntry8_10]] =
    Protocol("SLOWLOG", "GET" *: count *: EmptyTuple).using(RESPRead.instance(ServerInspection.logEntries8_10))
}

/** SLOWLOG: Redis 6.2 */
trait ServerInspectionCoreP extends Features {

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("SLOWLOG GET", "SLOWLOG HELP", "SLOWLOG LEN", "SLOWLOG RESET")

  val slowlog: SlowLogCommands = new SlowLogCommands {}

}

/** MEMORY without DOCTOR and PURGE, under `memory`. Redis 6.2 */
trait MemoryCoreCommands extends Features {
  import ServerInspection.MemoryStats

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("MEMORY HELP", "MEMORY MALLOC-STATS", "MEMORY STATS", "MEMORY USAGE: SAMPLES")

  val help: Single[Seq[String]] = Protocol.help("MEMORY")

  /** The memory allocator's own statistics, as text */
  val mallocstats: Single[String] = Protocol("MEMORY", "MALLOC-STATS").as[Bulk, String]

  val stats: Single[MemoryStats] = Protocol("MEMORY", "STATS").using(RESPRead.instance(ServerInspection.memoryStats))

  /** How many bytes the key and its value use, or `None` when it doesn't exist */
  def usage(key: Key): Single[Option[NonNegLong]] =
    Protocol("MEMORY", "USAGE" :: key.value :: Nil).as[OrNullBulk[Num], Option[NonNegLong]]

  /** Like `usage`, estimating a collection from `samples` of its elements, or from all of them when 0 */
  def usage(key: Key, samples: NonNegInt): Single[Option[NonNegLong]] =
    Protocol("MEMORY", "USAGE" *: key *: "SAMPLES" *: samples *: EmptyTuple).as[OrNullBulk[Num], Option[NonNegLong]]
}

/** MEMORY, under `memory`. Redis 6.2 */
trait MemoryCommands extends MemoryCoreCommands {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("MEMORY DOCTOR", "MEMORY PURGE")

  /** The server's advice about its memory use, as text */
  val doctor: Single[String] = Protocol("MEMORY", "DOCTOR").as[Bulk, String]

  val purge: Single[OK] = Protocol("MEMORY", "PURGE").as[Str, OK]
}

/** MEMORY under `memory`, and MODULE under `module`. Redis 6.2 */
trait ServerInspectionBaseP extends ServerInspectionCoreP {
  override private[laserdisc] def supported: List[String] = super.supported ++ memory.supported ++ module.supported

  lazy val memory: MemoryCommands = new MemoryCommands {}

  lazy val module: ModuleCommands = new ModuleCommands {}
}

/** MODULE LIST, under `module`. Redis 6.2 */
trait ModuleListCommands extends Features {
  import ServerInspection.ModuleInfo6_2

  override private[laserdisc] def supported: List[String] = super.supported ++ List("MODULE LIST")

  val list: Single[Seq[ModuleInfo6_2]] = Protocol("MODULE", "LIST").using(RESPRead.instance(ServerInspection.moduleInfos6_2))
}

/** MODULE LIST with each module's path and arguments: Redis 7.0 */
trait ModuleList7_0Commands extends ModuleListCommands {
  override val list: Single[Seq[ServerInspection.ModuleInfo7_0]] =
    Protocol("MODULE", "LIST").using(RESPRead.instance(ServerInspection.moduleInfos7_0))
}

/** MODULE: Redis 6.2. Servers refuse LOAD and UNLOAD unless they were started with `enable-module-command` */
trait ModuleCommands extends ModuleListCommands {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("MODULE HELP", "MODULE LOAD", "MODULE UNLOAD")

  val help: Single[Seq[String]] = Protocol.help("MODULE")

  /** Loads the module at that path on the server */
  def load(path: Key): Single[OK]                         = Protocol("MODULE", "LOAD" :: path.value :: Nil).as[Str, OK]
  def load(path: Key, args: OneOrMore[Value]): Single[OK] =
    Protocol("MODULE", Bulk("LOAD") :: Bulk(path.value) :: args.value.map(_.bulk)).as[Str, OK]

  /** Unloads the module of that name */
  def unload(name: Key): Single[OK] = Protocol("MODULE", "UNLOAD" :: name.value :: Nil).as[Str, OK]
}

/** MODULE LOADEX: Redis 7.0 */
trait ModuleLoadExCommands extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("MODULE LOADEX: ARGS CONFIG")

  private def configs(values: OneOrMore[(Key, Value)]): List[Bulk] =
    values.value.flatMap { case (name, value) => List(Bulk("CONFIG"), Bulk(name.value), value.bulk) }
  private def arguments(values: OneOrMore[Value]): List[Bulk] = Bulk("ARGS") :: values.value.map(_.bulk)

  /** Loads the module at that path on the server, setting its configuration */
  def loadex(path: Key): Single[OK]                                  = Protocol("MODULE", "LOADEX" :: path.value :: Nil).as[Str, OK]
  def loadex(path: Key, config: OneOrMore[(Key, Value)]): Single[OK] =
    Protocol("MODULE", Bulk("LOADEX") :: Bulk(path.value) :: configs(config)).as[Str, OK]
  def loadexargs(path: Key, args: OneOrMore[Value]): Single[OK] =
    Protocol("MODULE", Bulk("LOADEX") :: Bulk(path.value) :: arguments(args)).as[Str, OK]
  def loadex(path: Key, config: OneOrMore[(Key, Value)], args: OneOrMore[Value]): Single[OK] =
    Protocol("MODULE", Bulk("LOADEX") :: Bulk(path.value) :: (configs(config) ++ arguments(args))).as[Str, OK]
}

/** LATENCY LATEST: Redis 6.2 */
trait LatencyLatestCommands extends Features {
  import ServerInspection.LatencyEvent

  override private[laserdisc] def supported: List[String] = super.supported ++ List("LATENCY LATEST")

  val latest: Single[Seq[LatencyEvent]] = Protocol("LATENCY", "LATEST").using(RESPRead.instance(ServerInspection.latencyEvents))
}

/** LATENCY: Redis 6.2 */
trait LatencyCommands extends LatencyLatestCommands {
  import ServerInspection.LatencySample

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("LATENCY DOCTOR", "LATENCY GRAPH", "LATENCY HELP", "LATENCY HISTORY", "LATENCY RESET")

  val help: Single[Seq[String]] = Protocol.help("LATENCY")

  /** The server's advice about its latency, as text */
  val doctor: Single[String] = Protocol("LATENCY", "DOCTOR").as[Bulk, String]

  /** A graph of an event's latency, as text */
  def graph(event: Key): Single[String] = Protocol("LATENCY", "GRAPH" :: event.value :: Nil).as[Bulk, String]

  def history(event: Key): Single[Seq[LatencySample]] =
    Protocol("LATENCY", "HISTORY" :: event.value :: Nil).using(RESPRead.instance(ServerInspection.latencySamples))

  /** Forgets every event, or the given ones, and replies how many it forgot */
  val reset: Single[NonNegInt]                         = Protocol("LATENCY", "RESET").as[Num, NonNegInt]
  def reset(events: OneOrMore[Key]): Single[NonNegInt] = Protocol("LATENCY", "RESET" :: events.value.map(_.value)).as[Num, NonNegInt]
}

/** LATENCY HISTOGRAM: Redis 7.0 */
trait LatencyHistogramCommands extends Features {
  import ServerInspection.LatencyHistogram

  override private[laserdisc] def supported: List[String] = super.supported ++ List("LATENCY HISTOGRAM")

  /** The histogram of every command that ran, or of the given ones, by command name */
  val histogram: Single[Map[String, LatencyHistogram]] =
    Protocol("LATENCY", "HISTOGRAM").using(RESPRead.instance(ServerInspection.latencyHistograms))
  def histogram(commands: OneOrMore[Key]): Single[Map[String, LatencyHistogram]] =
    Protocol("LATENCY", "HISTOGRAM" :: commands.value.map(_.value)).using(RESPRead.instance(ServerInspection.latencyHistograms))
}

/** COMMAND without GETKEYS: Redis 6.2 */
trait CommandCoreCommands extends Features {
  import ServerInspection.CommandInfo6_2

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("COMMAND COUNT", "COMMAND HELP", "COMMAND INFO")

  val help: Single[Seq[String]] = Protocol.help("COMMAND")

  val count: Single[NonNegInt] = Protocol("COMMAND", "COUNT").as[Num, NonNegInt]

  /** The information of each command, or `None` for a name the server doesn't know */
  def info(names: OneOrMore[Key]): Single[Seq[Option[CommandInfo6_2]]] =
    Protocol("COMMAND", "INFO" :: names.value.map(_.value)).using(RESPRead.instance(ServerInspection.commandInfos6_2))
}

/** COMMAND INFO with each command's tips, key specifications and subcommands: Redis 7.0 */
trait CommandInfo7_0Commands extends CommandCoreCommands {
  override def info(names: OneOrMore[Key]): Single[Seq[Option[ServerInspection.CommandInfo7_0]]] =
    Protocol("COMMAND", "INFO" :: names.value.map(_.value)).using(RESPRead.instance(ServerInspection.commandInfos7_0))
}

/** COMMAND: Redis 6.2 */
trait CommandCommands extends CommandCoreCommands {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("COMMAND GETKEYS")

  /** The keys a command would use, without running it */
  def getkeys(command: Single[Any]): Single[Seq[Key]] =
    Protocol("COMMAND", "GETKEYS" :: ServerInspection.argumentsOf(command)).as[Arr.Of[Bulk], Seq[Key]]
}

/** COMMAND DOCS, COMMAND LIST and COMMAND GETKEYSANDFLAGS: Redis 7.0 */
trait CommandListCommands extends Features {
  import ServerInspection.{CommandDoc, CommandFilter, KeyAndFlags}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("COMMAND DOCS", "COMMAND GETKEYSANDFLAGS", "COMMAND LIST: ACLCAT FILTERBY MODULE PATTERN")

  /** The documentation of every command, or of the given ones, leaving out names the server doesn't know */
  lazy val docs: Single[Seq[CommandDoc]] = Protocol("COMMAND", "DOCS").using(RESPRead.instance(ServerInspection.commandDocs))
  def docs(names: OneOrMore[Key]): Single[Seq[CommandDoc]] =
    Protocol("COMMAND", "DOCS" :: names.value.map(_.value)).using(RESPRead.instance(ServerInspection.commandDocs))

  /** The keys a command would use, with what it would do to each, without running it */
  def getkeysandflags(command: Single[Any]): Single[Seq[KeyAndFlags]] =
    Protocol("COMMAND", "GETKEYSANDFLAGS" :: ServerInspection.argumentsOf(command)).using(RESPRead.instance(ServerInspection.keysAndFlags))

  /** The names of every command, or of those the filter keeps */
  val list: Single[Seq[String]]                        = Protocol("COMMAND", "LIST").as[Arr.Of[Bulk], Seq[String]]
  def list(filter: CommandFilter): Single[Seq[String]] = Protocol("COMMAND", "LIST" :: filter.params).as[Arr.Of[Bulk], Seq[String]]
}

/** HOTKEYS: Redis 8.6 */
trait HotKeysCommands extends Features {
  import ServerInspection.{HotKeys, HotKeysMetrics, HotKeysOptions}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "HOTKEYS GET",
      "HOTKEYS HELP",
      "HOTKEYS RESET",
      "HOTKEYS START: COUNT CPU DURATION METRICS NET SAMPLE SLOTS",
      "HOTKEYS STOP"
    )

  /** Redis 8.6.1 */
  val help: Single[Seq[String]] = Protocol.help("HOTKEYS")

  def start(metrics: HotKeysMetrics): Single[OK]                          = Protocol("HOTKEYS", "START" :: metrics.params).as[Str, OK]
  def start(metrics: HotKeysMetrics, options: HotKeysOptions): Single[OK] =
    Protocol("HOTKEYS", "START" :: metrics.params ++ options.params).as[Str, OK]

  val stop: Single[OK] = Protocol("HOTKEYS", "STOP").as[Str, OK]

  /** The reports of the tracking, or `None` when there's none to report */
  val get: Single[Option[Seq[HotKeys]]] = Protocol("HOTKEYS", "GET").using(ServerInspection.maybeHotKeys)

  /** Frees what the tracking used */
  val reset: Single[OK] = Protocol("HOTKEYS", "RESET").as[Str, OK]
}

/** COMMANDLOG: Valkey 8.1 */
trait CommandLogCommands extends Features {
  import ServerInspection.{CommandLogType, LogEntry6_2}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "COMMANDLOG GET: LARGE-REPLY LARGE-REQUEST SLOW",
      "COMMANDLOG HELP",
      "COMMANDLOG LEN: LARGE-REPLY LARGE-REQUEST SLOW",
      "COMMANDLOG RESET: LARGE-REPLY LARGE-REQUEST SLOW"
    )

  val help: Single[Seq[String]] = Protocol.help("COMMANDLOG")

  /** Every entry of the log */
  def get(log: CommandLogType): Single[Seq[LogEntry6_2]] =
    Protocol("COMMANDLOG", "GET" *: -1 *: log *: EmptyTuple).using(RESPRead.instance(ServerInspection.logEntries6_2))
  def get(log: CommandLogType, count: PosInt): Single[Seq[LogEntry6_2]] =
    Protocol("COMMANDLOG", "GET" *: count *: log *: EmptyTuple).using(RESPRead.instance(ServerInspection.logEntries6_2))

  def len(log: CommandLogType): Single[NonNegLong] = Protocol("COMMANDLOG", "LEN" *: log *: EmptyTuple).as[Num, NonNegLong]

  def reset(log: CommandLogType): Single[OK] = Protocol("COMMANDLOG", "RESET" *: log *: EmptyTuple).as[Str, OK]
}
