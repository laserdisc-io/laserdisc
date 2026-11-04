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
  private final val nonNegLong = Read.num2NonNegLongRead
  private final val strings    = Read.arrOf(Read.bulk2StringRead)

  // the text of a Bulk or of a Str, as COMMAND INFO's flags and categories come as Str
  private final def text(resp: RESP): Option[String] = resp match {
    case Bulk(s) => Some(s)
    case Str(s)  => Some(s)
    case _       => None
  }
  private final def texts(resp: RESP): Option[Seq[String]] = resp match {
    case Arr(elements) =>
      val read = elements.map(text)
      if (read.forall(_.isDefined)) Some(read.flatten) else None
    case _ => None
  }

  /** An entry of SLOWLOG GET or COMMANDLOG GET. Newer versions send more fields after these, which are left out */
  final case class LogEntry(
      id: NonNegLong,
      timestamp: NonNegLong,
      microseconds: NonNegLong,
      arguments: Seq[String],
      clientAddress: String,
      clientName: String
  )
  private final val logEntry: Read[Arr, LogEntry] = Read.instancePF("Arr(log entry)") {
    case Arr(
          nonNegLong(Right(id)) +: nonNegLong(Right(timestamp)) +: nonNegLong(Right(microseconds)) +: strings(Right(arguments)) +:
          Bulk(address) +: Bulk(name) +: _
        ) =>
      LogEntry(id, timestamp, microseconds, arguments, address, name)
  }
  private[protocol] final val logEntries: Read[Arr, Seq[LogEntry]] = Read.arrOf(logEntry)

  /** An event of LATENCY LATEST: when it last happened, how long it took then, and the longest it ever took */
  final case class LatencyEvent(name: String, timestamp: NonNegLong, latestMilliseconds: NonNegLong, maxMilliseconds: NonNegLong)
  private final val latencyEvent: Read[Arr, LatencyEvent] = Read.instancePF("Arr(latency event)") {
    case Arr(Bulk(name) +: nonNegLong(Right(timestamp)) +: nonNegLong(Right(latest)) +: nonNegLong(Right(max)) +: _) =>
      LatencyEvent(name, timestamp, latest, max)
  }
  private[protocol] final val latencyEvents: Read[Arr, Seq[LatencyEvent]] = Read.arrOf(latencyEvent)

  /** A sample of LATENCY HISTORY */
  final case class LatencySample(timestamp: NonNegLong, milliseconds: NonNegLong)
  private final val latencySample: Read[Arr, LatencySample] = Read.instancePF("Arr(latency sample)") {
    case Arr(nonNegLong(Right(timestamp)) +: nonNegLong(Right(milliseconds)) +: Seq()) => LatencySample(timestamp, milliseconds)
  }
  private[protocol] final val latencySamples: Read[Arr, Seq[LatencySample]] = Read.arrOf(latencySample)

  /** A command's LATENCY HISTOGRAM: how many calls it had, and for each bucket the number of calls that took at most that many microseconds
    */
  final case class LatencyHistogram(calls: NonNegLong, buckets: Seq[(NonNegLong, NonNegLong)])
  private[protocol] final val latencyHistograms: Read[Arr, Map[String, LatencyHistogram]] = {
    val buckets   = Read.pairs(Read.num2NonNegLongRead, Read.num2NonNegLongRead)
    val histogram = Read.instancePF[Arr, LatencyHistogram]("Arr(latency histogram)") {
      case Arr(Bulk("calls") +: nonNegLong(Right(calls)) +: Bulk("histogram_usec") +: buckets(Right(bs)) +: Seq()) =>
        LatencyHistogram(calls, bs)
    }
    Read.pairs(Read.bulk2StringRead, histogram).map(_.toMap)
  }

  /** MEMORY STATS: the figures for the whole server, and those of each database, as text since they mix whole and decimal numbers */
  final case class MemoryStats(values: Map[String, String], databases: Map[Int, Map[String, String]])
  private[protocol] final val memoryStats: Read[Arr, MemoryStats] = {
    val value    = Read.instancePF[RESP, String]("a number") { case Num(n) => n.toString; case Bulk(s) => s }
    val database = Read.pairs(Read.bulk2StringRead, value).map(_.toMap)
    val Db       = raw"db\.(\d+)".r
    Read.instance { arr =>
      Read.fields.read(arr).flatMap {
        _.foldLeft[Either[RESPDecErr, MemoryStats]](Right(MemoryStats(Map.empty, Map.empty))) {
          case (Right(stats), (k @ Db(n), fs: Arr)) =>
            database.read(fs) match {
              case Right(d) => Right(stats.copy(databases = stats.databases + (n.toInt -> d)))
              case Left(e)  => Left(RESPDecErr(s"Unexpected memory stats for $k: ${e.message}"))
            }
          case (Right(stats), (k, v)) =>
            value.read(v) match {
              case Right(s) => Right(stats.copy(values = stats.values + (k -> s)))
              case Left(e)  => Left(RESPDecErr(s"Unexpected memory stat $k: ${e.message}"))
            }
          case (left, _) => left
        }
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
  private[protocol] final val commandInfos: Read[Arr, Seq[Option[CommandInfo]]] = Read.instance { case Arr(elements) =>
    elements.foldRight[Either[RESPDecErr, List[Option[CommandInfo]]]](Right(Nil)) {
      case (NilArr | NullBulk, Right(infos))                                                                      => Right(None :: infos)
      case (Arr(Bulk(name) +: Num(arity) +: flags +: Num(first) +: Num(last) +: Num(step) +: rest), Right(infos)) =>
        (texts(flags), rest.headOption.fold[Option[Seq[String]]](Some(Nil))(texts)) match {
          case (Some(fs), Some(categories)) =>
            Right(Some(CommandInfo(name, arity.toInt, fs, first.toInt, last.toInt, step.toInt, categories)) :: infos)
          case _ => Left(RESPDecErr(s"Unexpected flags or ACL categories for $name"))
        }
      case (other, Right(_)) => Left(RESPDecErr(s"Unexpected command info. Was $other"))
      case (_, left)         => left
    }
  }

  /** A key of COMMAND GETKEYSANDFLAGS, with what the command does to it */
  final case class KeyAndFlags(key: Key, flags: Seq[String])
  private[protocol] final val keysAndFlags: Read[Arr, Seq[KeyAndFlags]] = Read.instance { case Arr(elements) =>
    elements.foldRight[Either[RESPDecErr, List[KeyAndFlags]]](Right(Nil)) {
      case (Arr(Bulk(Key(key)) +: flags +: Seq()), Right(kfs)) =>
        texts(flags).fold[Either[RESPDecErr, List[KeyAndFlags]]](Left(RESPDecErr(s"Unexpected flags of $key")))(fs =>
          Right(KeyAndFlags(key, fs) :: kfs)
        )
      case (other, Right(_)) => Left(RESPDecErr(s"Unexpected key and flags. Was $other"))
      case (_, left)         => left
    }
  }

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
  private[protocol] final val hotKeys: Read[Arr, Seq[HotKeys]] = {
    val keysAndCounts                     = Read.pairs(Read.bulk2KeyRead, Read.num2NonNegLongRead)
    val hotKeysReport: Read[Arr, HotKeys] = Read.instance { arr =>
      Read.fields.read(arr).flatMap {
        _.foldLeft[Either[RESPDecErr, HotKeys]](Right(HotKeys(Map.empty, Nil, Nil))) {
          case (Right(h), ("by-cpu-time-us", keysAndCounts(Right(kns)))) => Right(h.copy(byCpuTime = kns))
          case (Right(h), ("by-net-bytes", keysAndCounts(Right(kns))))   => Right(h.copy(byNetBytes = kns))
          case (Right(h), (k, Num(n)))                                   => Right(h.copy(values = h.values + (k -> n.toString)))
          case (Right(h), (k, Bulk(v)))                                  => Right(h.copy(values = h.values + (k -> v)))
          case (Right(h), (k, _: Arr)) if k != "by-cpu-time-us" && k != "by-net-bytes" => Right(h)
          case (Right(_), other) => Left(RESPDecErr(s"Unexpected hot keys report. Was $other"))
          case (left, _)         => left
        }
      }
    }
    Read.arrOf(hotKeysReport)
  }

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
  val get: Single.Aux[Option[Seq[HotKeys]]] = Protocol("HOTKEYS", "GET").using(RESPRead.orNilArrOrNullBulk(ServerInspection.hotKeys))

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
