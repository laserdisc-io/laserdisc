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

  /** An entry of SLOWLOG GET or COMMANDLOG GET. Newer versions send more fields after these, which are left out */
  final case class LogEntry(
      id: NonNegLong,
      timestamp: NonNegLong,
      microseconds: NonNegLong,
      arguments: Seq[String],
      clientAddress: String,
      clientName: String
  )
  private final type LogEntryReply = Arr.Rest[Num *: Num *: Num *: Arr.Of[Bulk] *: Bulk *: Bulk *: EmptyTuple, RESP]
  private implicit final val logEntry: Read[LogEntryReply, LogEntry] =
    Read[LogEntryReply, NonNegLong *: NonNegLong *: NonNegLong *: Seq[String] *: String *: String *: EmptyTuple].map {
      case id *: timestamp *: microseconds *: arguments *: address *: name *: _ =>
        LogEntry(id, timestamp, microseconds, arguments, address, name)
    }
  private[protocol] final val logEntries: Read[Arr.Of[LogEntryReply], Seq[LogEntry]] = Read[Arr.Of[LogEntryReply], Seq[LogEntry]]

  /** An event of LATENCY LATEST: when it last happened, how long it took then, and the longest it ever took */
  final case class LatencyEvent(name: String, timestamp: NonNegLong, latestMilliseconds: NonNegLong, maxMilliseconds: NonNegLong)
  private final type LatencyEventReply = Arr.Rest[Bulk *: Num *: Num *: Num *: EmptyTuple, RESP]
  private implicit final val latencyEvent: Read[LatencyEventReply, LatencyEvent] =
    Read[LatencyEventReply, String *: NonNegLong *: NonNegLong *: NonNegLong *: EmptyTuple].map {
      case name *: timestamp *: latest *: max *: _ =>
        LatencyEvent(name, timestamp, latest, max)
    }
  private[protocol] final val latencyEvents: Read[Arr.Of[LatencyEventReply], Seq[LatencyEvent]] =
    Read[Arr.Of[LatencyEventReply], Seq[LatencyEvent]]

  /** A sample of LATENCY HISTORY */
  final case class LatencySample(timestamp: NonNegLong, milliseconds: NonNegLong)
  private implicit final val latencySample: Read[Arr.Tuple[Num *: Num *: EmptyTuple], LatencySample] =
    Read[Arr.Tuple[Num *: Num *: EmptyTuple], NonNegLong *: NonNegLong *: EmptyTuple].map { case timestamp *: milliseconds *: _ =>
      LatencySample(timestamp, milliseconds)
    }
  private[protocol] final val latencySamples: Read[Arr.Of[Arr.Tuple[Num *: Num *: EmptyTuple]], Seq[LatencySample]] =
    Read[Arr.Of[Arr.Tuple[Num *: Num *: EmptyTuple]], Seq[LatencySample]]

  /** A command's LATENCY HISTOGRAM: how many calls it had, and for each bucket the number of calls that took at most that many microseconds
    */
  final case class LatencyHistogram(calls: NonNegLong, buckets: Seq[(NonNegLong, NonNegLong)])
  private implicit final val latencyHistogram: Read[Record, LatencyHistogram] = Read.record("latency histogram") { f =>
    for {
      calls   <- f.num[NonNegLong]("calls")
      buckets <- f[Arr.Pairs[Num, Num], Seq[(NonNegLong, NonNegLong)]]("histogram_usec")
    } yield LatencyHistogram(calls, buckets)
  }
  private[protocol] final val latencyHistograms: Read[Arr.Pairs[Bulk, Record], Map[String, LatencyHistogram]] =
    Read[Arr.Pairs[Bulk, Record], Map[String, LatencyHistogram]]

  /** MEMORY STATS: the figures for the whole server, and those of each database, as text since they mix whole and decimal numbers */
  final case class MemoryStats(values: Map[String, String], databases: Map[Int, Map[String, String]])
  private[protocol] final val memoryStats: Read[Record, MemoryStats] = {
    val value    = Read.instancePF[RESP, String]("a number") { case Num(n) => n.toString; case Bulk(s) => s }
    val database = Read.pairs(Read.bulk2StringRead, value).map(_.toMap)
    val Db       = raw"db\.(\d+)".r
    Read.record("memory stats") {
      _.others(Set.empty).foldLeft[Either[RESPDecErr, MemoryStats]](Right(MemoryStats(Map.empty, Map.empty))) {
        case (Right(stats), (k @ Db(n), fs: Arr)) =>
          database.read(fs) match {
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

  /** COMMAND INFO of a command: its arity (negative when it takes at least that many arguments), flags, where its keys are and its ACL
    * categories. Redis 7.0 also replies with tips, key specifications and subcommands, which are left out here
    */
  final case class CommandInfo(
      name: String,
      arity: Int,
      flags: Seq[String],
      firstKey: Int,
      lastKey: Int,
      step: Int,
      aclCategories: Seq[String]
  )
  private type CommandInfoReply = Arr.Rest[Bulk *: Num *: Arr.Of[Str] *: Num *: Num *: Num *: Arr.Of[Str] *: EmptyTuple, RESP]
  private implicit final val commandInfo: Read[CommandInfoReply, CommandInfo] =
    Read[CommandInfoReply, String *: Int *: Seq[String] *: Int *: Int *: Int *: Seq[String] *: EmptyTuple].map {
      case name *: arity *: flags *: first *: last *: step *: categories *: _ =>
        CommandInfo(name, arity, flags, first, last, step, categories)
    }
  private[protocol] final val commandInfos: Read[Arr.Of[OrNullBulk[CommandInfoReply]], Seq[Option[CommandInfo]]] =
    Read[Arr.Of[OrNullBulk[CommandInfoReply]], Seq[Option[CommandInfo]]]

  /** A key of COMMAND GETKEYSANDFLAGS, with what the command does to it */
  final case class KeyAndFlags(key: Key, flags: Seq[String])
  private implicit final val keyAndFlags: Read[Arr.Tuple[Bulk *: Arr.Of[Str] *: EmptyTuple], KeyAndFlags] =
    Read[Arr.Tuple[Bulk *: Arr.Of[Str] *: EmptyTuple], Key *: Seq[String] *: EmptyTuple].map { case key *: flags *: _ =>
      KeyAndFlags(key, flags)
    }
  private[protocol] final val keysAndFlags: Read[Arr.Of[Arr.Tuple[Bulk *: Arr.Of[Str] *: EmptyTuple]], Seq[KeyAndFlags]] =
    Read[Arr.Of[Arr.Tuple[Bulk *: Arr.Of[Str] *: EmptyTuple]], Seq[KeyAndFlags]]

  /** FILTERBY, for COMMAND LIST: the commands of a module, of an ACL category, or whose names match a pattern */
  sealed trait CommandFilter { def params: List[String] }
  object CommandFilter       {
    private final class Of(override final val params: List[String]) extends CommandFilter

    def module(name: Key): CommandFilter             = new Of(List("FILTERBY", "MODULE", name.value))
    def aclCategory(name: Key): CommandFilter        = new Of(List("FILTERBY", "ACLCAT", name.value))
    def pattern(pattern: GlobPattern): CommandFilter = new Of(List("FILTERBY", "PATTERN", pattern.value))
  }

  /** A module of MODULE LIST. Redis 7.0 also replies with its path and arguments */
  final case class ModuleInfo(name: String, version: Long, path: Option[String], arguments: Seq[String])
  private implicit final val moduleInfo: Read[Record, ModuleInfo] = Read.record("module") { f =>
    for {
      name      <- f.bulk[String]("name")
      version   <- f.num[Long]("ver")
      path      <- f.optionalBulk[String]("path")
      arguments <- f.optional[Texts, Seq[String]]("args")
    } yield ModuleInfo(name, version, path, arguments.getOrElse(Nil))
  }
  private[protocol] final val moduleInfos: Read[Records, Seq[ModuleInfo]] = Read[Records, Seq[ModuleInfo]]

  /** SLOW, LARGE-REQUEST and LARGE-REPLY, for COMMANDLOG: Valkey 8.1 */
  sealed trait CommandLogType
  object CommandLogType {
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
  sealed trait HotKeysMetrics { def params: List[String] }
  object HotKeysMetrics       {
    private final class Of(override final val params: List[String]) extends HotKeysMetrics

    final val cpu: HotKeysMetrics       = new Of(List("METRICS", "1", "CPU"))
    final val net: HotKeysMetrics       = new Of(List("METRICS", "1", "NET"))
    final val cpuAndNet: HotKeysMetrics = new Of(List("METRICS", "2", "CPU", "NET"))
  }

  /** COUNT, DURATION, SAMPLE and SLOTS, for HOTKEYS START: how many keys to report, for how long to track, 1 in how many commands to look
    * at, and which slots to track
    */
  final class HotKeysOptions private (val params: List[String]) {
    def count(keys: PosInt): HotKeysOptions           = new HotKeysOptions(params ++ List("COUNT", keys.value.toString))
    def duration(seconds: PosInt): HotKeysOptions     = new HotKeysOptions(params ++ List("DURATION", seconds.value.toString))
    def sample(ratio: PosInt): HotKeysOptions         = new HotKeysOptions(params ++ List("SAMPLE", ratio.value.toString))
    def slots(slots: OneOrMore[Slot]): HotKeysOptions =
      new HotKeysOptions(params ++ ("SLOTS" :: slots.value.length.toString :: slots.value.map(_.value.toString)))
  }
  object HotKeysOptions {
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
      cpu    <- f.optional[Arr.Pairs[Bulk, Num], Seq[(Key, NonNegLong)]](byCpuTime)
      net    <- f.optional[Arr.Pairs[Bulk, Num], Seq[(Key, NonNegLong)]](byNetBytes)
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

  private[protocol] final def argumentsOf(protocol: Single): List[String] =
    protocol.command :: protocol.parameters.toList.collect { case Bulk(s) => s }
}

/** SLOWLOG, MEMORY and MODULE LIST: Redis 6.2 */
trait ServerInspectionBaseP extends Features {
  import ServerInspection.{LogEntry, MemoryStats, ModuleInfo}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "MEMORY DOCTOR",
      "MEMORY MALLOC-STATS",
      "MEMORY PURGE",
      "MEMORY STATS",
      "MEMORY USAGE: SAMPLES",
      "MODULE LIST",
      "SLOWLOG GET",
      "SLOWLOG LEN",
      "SLOWLOG RESET"
    )

  object slowlog {

    /** The 10 most recent entries */
    val get: Single.Aux[Seq[LogEntry]]                = Protocol("SLOWLOG", "GET").using(RESPRead.instance(ServerInspection.logEntries))
    def get(count: PosInt): Single.Aux[Seq[LogEntry]] =
      Protocol("SLOWLOG", "GET" *: count *: EmptyTuple).using(RESPRead.instance(ServerInspection.logEntries))
    val len: Single.Aux[NonNegLong] = Protocol("SLOWLOG", "LEN").as[Num, NonNegLong]
    val reset: Single.Aux[OK]       = Protocol("SLOWLOG", "RESET").as[Str, OK]
  }

  object memory {

    /** The server's advice about its memory use, as text */
    val doctor: Single.Aux[String] = Protocol("MEMORY", "DOCTOR").as[Bulk, String]

    /** The memory allocator's own statistics, as text */
    val mallocstats: Single.Aux[String] = Protocol("MEMORY", "MALLOC-STATS").as[Bulk, String]

    val purge: Single.Aux[OK]          = Protocol("MEMORY", "PURGE").as[Str, OK]
    val stats: Single.Aux[MemoryStats] = Protocol("MEMORY", "STATS").using(RESPRead.instance(ServerInspection.memoryStats))

    /** How many bytes the key and its value use, or `None` when it doesn't exist */
    def usage(key: Key): Single.Aux[Option[NonNegLong]] =
      Protocol("MEMORY", "USAGE" :: key.value :: Nil).as[OrNullBulk[Num], Option[NonNegLong]]

    /** Like `usage`, estimating a collection from `samples` of its elements, or from all of them when 0 */
    def usage(key: Key, samples: NonNegInt): Single.Aux[Option[NonNegLong]] =
      Protocol("MEMORY", "USAGE" *: key *: "SAMPLES" *: samples *: EmptyTuple).as[OrNullBulk[Num], Option[NonNegLong]]
  }

  object module {
    val list: Single.Aux[Seq[ModuleInfo]] = Protocol("MODULE", "LIST").using(RESPRead.instance(ServerInspection.moduleInfos))
  }
}

/** LATENCY: Redis 6.2 */
trait LatencyCommands extends Features {
  import ServerInspection.{LatencyEvent, LatencySample}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("LATENCY DOCTOR", "LATENCY GRAPH", "LATENCY HISTORY", "LATENCY LATEST", "LATENCY RESET")

  /** The server's advice about its latency, as text */
  val doctor: Single.Aux[String] = Protocol("LATENCY", "DOCTOR").as[Bulk, String]

  /** A graph of an event's latency, as text */
  def graph(event: Key): Single.Aux[String] = Protocol("LATENCY", "GRAPH" :: event.value :: Nil).as[Bulk, String]

  def history(event: Key): Single.Aux[Seq[LatencySample]] =
    Protocol("LATENCY", "HISTORY" :: event.value :: Nil).using(RESPRead.instance(ServerInspection.latencySamples))

  val latest: Single.Aux[Seq[LatencyEvent]] = Protocol("LATENCY", "LATEST").using(RESPRead.instance(ServerInspection.latencyEvents))

  /** Forgets every event, or the given ones, and replies how many it forgot */
  val reset: Single.Aux[NonNegInt]                         = Protocol("LATENCY", "RESET").as[Num, NonNegInt]
  def reset(events: OneOrMore[Key]): Single.Aux[NonNegInt] = Protocol("LATENCY", "RESET" :: events.value.map(_.value)).as[Num, NonNegInt]
}

/** LATENCY HISTOGRAM: Redis 7.0 */
trait LatencyHistogramCommands extends Features {
  import ServerInspection.LatencyHistogram

  override private[laserdisc] def supported: List[String] = super.supported ++ List("LATENCY HISTOGRAM")

  /** The histogram of every command that ran, or of the given ones, by command name */
  val histogram: Single.Aux[Map[String, LatencyHistogram]] =
    Protocol("LATENCY", "HISTOGRAM").using(RESPRead.instance(ServerInspection.latencyHistograms))
  def histogram(commands: OneOrMore[Key]): Single.Aux[Map[String, LatencyHistogram]] =
    Protocol("LATENCY", "HISTOGRAM" :: commands.value.map(_.value)).using(RESPRead.instance(ServerInspection.latencyHistograms))
}

/** COMMAND: Redis 6.2 */
trait CommandCommands extends Features {
  import ServerInspection.CommandInfo

  override private[laserdisc] def supported: List[String] = super.supported ++ List("COMMAND COUNT", "COMMAND GETKEYS", "COMMAND INFO")

  val count: Single.Aux[NonNegInt] = Protocol("COMMAND", "COUNT").as[Num, NonNegInt]

  /** The keys a command would use, without running it */
  def getkeys(command: Single): Single.Aux[Seq[Key]] =
    Protocol("COMMAND", "GETKEYS" :: ServerInspection.argumentsOf(command)).as[Arr.Of[Bulk], Seq[Key]]

  /** The information of each command, or `None` for a name the server doesn't know */
  def info(names: OneOrMore[Key]): Single.Aux[Seq[Option[CommandInfo]]] =
    Protocol("COMMAND", "INFO" :: names.value.map(_.value)).using(RESPRead.instance(ServerInspection.commandInfos))
}

/** COMMAND LIST and COMMAND GETKEYSANDFLAGS: Redis 7.0 */
trait CommandListCommands extends Features {
  import ServerInspection.{CommandFilter, KeyAndFlags}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("COMMAND GETKEYSANDFLAGS", "COMMAND LIST: ACLCAT FILTERBY MODULE PATTERN")

  /** The keys a command would use, with what it would do to each, without running it */
  def getkeysandflags(command: Single): Single.Aux[Seq[KeyAndFlags]] =
    Protocol("COMMAND", "GETKEYSANDFLAGS" :: ServerInspection.argumentsOf(command)).using(RESPRead.instance(ServerInspection.keysAndFlags))

  /** The names of every command, or of those the filter keeps */
  val list: Single.Aux[Seq[String]]                        = Protocol("COMMAND", "LIST").as[Arr.Of[Bulk], Seq[String]]
  def list(filter: CommandFilter): Single.Aux[Seq[String]] = Protocol("COMMAND", "LIST" :: filter.params).as[Arr.Of[Bulk], Seq[String]]
}

/** HOTKEYS: Redis 8.6 */
trait HotKeysCommands extends Features {
  import ServerInspection.{HotKeys, HotKeysMetrics, HotKeysOptions}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("HOTKEYS GET", "HOTKEYS RESET", "HOTKEYS START: COUNT CPU DURATION METRICS NET SAMPLE SLOTS", "HOTKEYS STOP")

  def start(metrics: HotKeysMetrics): Single.Aux[OK]                          = Protocol("HOTKEYS", "START" :: metrics.params).as[Str, OK]
  def start(metrics: HotKeysMetrics, options: HotKeysOptions): Single.Aux[OK] =
    Protocol("HOTKEYS", "START" :: metrics.params ++ options.params).as[Str, OK]

  val stop: Single.Aux[OK] = Protocol("HOTKEYS", "STOP").as[Str, OK]

  /** The reports of the tracking, or `None` when there's none to report */
  val get: Single.Aux[Option[Seq[HotKeys]]] = Protocol("HOTKEYS", "GET").using(ServerInspection.maybeHotKeys)

  /** Frees what the tracking used */
  val reset: Single.Aux[OK] = Protocol("HOTKEYS", "RESET").as[Str, OK]
}

/** COMMANDLOG: Valkey 8.1 */
trait CommandLogCommands extends Features {
  import ServerInspection.{CommandLogType, LogEntry}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "COMMANDLOG GET: LARGE-REPLY LARGE-REQUEST SLOW",
      "COMMANDLOG LEN: LARGE-REPLY LARGE-REQUEST SLOW",
      "COMMANDLOG RESET: LARGE-REPLY LARGE-REQUEST SLOW"
    )

  /** Every entry of the log */
  def get(log: CommandLogType): Single.Aux[Seq[LogEntry]] =
    Protocol("COMMANDLOG", "GET" *: -1 *: log *: EmptyTuple).using(RESPRead.instance(ServerInspection.logEntries))
  def get(log: CommandLogType, count: PosInt): Single.Aux[Seq[LogEntry]] =
    Protocol("COMMANDLOG", "GET" *: count *: log *: EmptyTuple).using(RESPRead.instance(ServerInspection.logEntries))

  def len(log: CommandLogType): Single.Aux[NonNegLong] = Protocol("COMMANDLOG", "LEN" *: log *: EmptyTuple).as[Num, NonNegLong]

  def reset(log: CommandLogType): Single.Aux[OK] = Protocol("COMMANDLOG", "RESET" *: log *: EmptyTuple).as[Str, OK]
}
