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

/** The replies and options of the cluster commands added after Redis 6.2, and of BACKUP */
object ClusterManagement {
  private[protocol] final val nodeId: Read[Bulk, NodeId] = Read.instancePF("Bulk(NodeId)") { case Bulk(NodeId(id)) => id }

  // Dragonfly sends a simple string
  private[protocol] final val nodeIdOfText: Read[OneOf[Bulk *: Str *: EmptyTuple], NodeId] = {
    implicit val bulk: Read[Bulk, NodeId] = nodeId
    implicit val str: Read[Str, NodeId]   = Read.instancePF("Str(NodeId)") { case Str(NodeId(id)) => id }
    Read[OneOf[Bulk *: Str *: EmptyTuple], NodeId]
  }

  private final val num = Read.num2NonNegLongRead

  private final def ranges(slots: OneOrMore[(Slot, Slot)]): List[String] = slots.value.flatMap { case (from, to) =>
    List(Show[Slot].show(from), Show[Slot].show(to))
  }

  /** A node of CLUSTER SHARDS. A node may have a port, a TLS port or both, and a hostname */
  final case class ShardNode(
      id: String,
      ip: String,
      endpoint: String,
      role: String,
      health: String,
      replicationOffset: Long,
      port: Option[Int],
      tlsPort: Option[Int],
      hostname: Option[String]
  )

  /** A shard of CLUSTER SHARDS: the slot ranges it serves and its nodes. Valkey also replies with the shard's id */
  final case class Shard(slots: Seq[(Slot, Slot)], nodes: Seq[ShardNode], id: Option[String])

  private implicit final val shardNode: Read[Record, ShardNode] = Read.record("shard node") { f =>
    for {
      id       <- f.bulk[String]("id")
      ip       <- f.bulk[String]("ip")
      endpoint <- f.bulk[String]("endpoint")
      role     <- f.bulk[String]("role")
      health   <- f.bulk[String]("health")
      offset   <- f.num[Long]("replication-offset")
      port     <- f.optionalNum[Int]("port")
      tlsPort  <- f.optionalNum[Int]("tls-port")
      hostname <- f.optionalBulk[String]("hostname")
    } yield ShardNode(id, ip, endpoint, role, health, offset, port, tlsPort, hostname)
  }
  private implicit final val shard: Read[Record, Shard] = Read.record("shard") { f =>
    for {
      slots <- f[Arr.OfPairs[Num, Num], Seq[(Slot, Slot)]]("slots")
      nodes <- f[Records, Seq[ShardNode]]("nodes")
      id    <- f.optionalBulk[String]("id")
    } yield Shard(slots, nodes, id)
  }
  private[protocol] final val shards: Read[Records, Seq[Shard]] = Read[Records, Seq[Shard]]

  /** A link of CLUSTER LINKS: a connection to (`to`) or from (`from`) another node, with its buffer use in bytes */
  final case class ClusterLink(
      direction: String,
      node: String,
      createTime: Long,
      events: String,
      sendBufferAllocated: Long,
      sendBufferUsed: Long
  )
  private implicit final val link: Read[Record, ClusterLink] = Read.record("cluster link") { f =>
    for {
      direction <- f.bulk[String]("direction")
      node      <- f.bulk[String]("node")
      created   <- f.num[Long]("create-time")
      events    <- f.bulk[String]("events")
      allocated <- f.num[Long]("send-buffer-allocated")
      used      <- f.num[Long]("send-buffer-used")
    } yield ClusterLink(direction, node, created, events, allocated, used)
  }
  private[protocol] final val links: Read[Records, Seq[ClusterLink]] = Read[Records, Seq[ClusterLink]]

  /** The figures of a slot of CLUSTER SLOT-STATS, by metric name, like `key-count` */
  final case class SlotStats(slot: Slot, metrics: Map[String, Long])
  private implicit final val slotStat: Read[Arr.AsTuple[Num *: Arr.OfPairs[Bulk, Num] *: EmptyTuple], SlotStats] =
    Read[Arr.AsTuple[Num *: Arr.OfPairs[Bulk, Num] *: EmptyTuple], Slot *: Map[String, Long] *: EmptyTuple].map {
      case slot *: metrics *: _ =>
        SlotStats(slot, metrics)
    }
  private[protocol] final val slotStats: Read[Arr.Of[Arr.AsTuple[Num *: Arr.OfPairs[Bulk, Num] *: EmptyTuple]], Seq[SlotStats]] =
    Read[Arr.Of[Arr.AsTuple[Num *: Arr.OfPairs[Bulk, Num] *: EmptyTuple]], Seq[SlotStats]]

  /** What CLUSTER SLOT-STATS orders slots by. `cpuUsec`, `networkBytesIn` and `networkBytesOut` need the server's
    * `cluster-slot-stats-enabled` setting
    */
  sealed trait SlotStatsMetric
  object SlotStatsMetric {
    object keyCount        extends SlotStatsMetric
    object cpuUsec         extends SlotStatsMetric
    object networkBytesIn  extends SlotStatsMetric
    object networkBytesOut extends SlotStatsMetric

    implicit val slotStatsMetricShow: Show[SlotStatsMetric] = Show.instance {
      case `keyCount`        => "key-count"
      case `cpuUsec`         => "cpu-usec"
      case `networkBytesIn`  => "network-bytes-in"
      case `networkBytesOut` => "network-bytes-out"
    }
  }

  /** A slot migration of CLUSTER MIGRATION STATUS (Redis) or CLUSTER GETSLOTMIGRATIONS (Valkey), by field name */
  final case class SlotMigration(fields: Map[String, String])
  private[protocol] final val slotMigrations: Read[Records, Seq[SlotMigration]] = {
    implicit val text: Read[RESP, String] = Read.instancePF("text or a whole number") {
      case Bulk(v) => v
      case Num(v)  => v.toString
      case Str(v)  => v
    }
    implicit val migration: Read[Record, SlotMigration] = Read[Record, Map[String, String]].map(SlotMigration(_))
    Read[Records, Seq[SlotMigration]]
  }

  /** ASYNC and SYNC, for CLUSTER FLUSHSLOT */
  sealed trait FlushMode
  object FlushMode {
    object async extends FlushMode
    object sync  extends FlushMode

    implicit val flushModeShow: Show[FlushMode] = Show.instance {
      case `async` => "ASYNC"
      case `sync`  => "SYNC"
    }
  }

  /** BACKUP STATUS: the state (like `idle` or `snapshotting`), the last error, and when the backup started and ended */
  final case class BackupStatus(state: String, error: String, startTime: Long, endTime: Long)
  private[protocol] final val backupStatus: Read[Record, BackupStatus] = Read.record("backup status") { f =>
    for {
      state <- f.bulk[String]("state")
      error <- f.bulk[String]("error")
      start <- f.num[Long]("start_time")
      end   <- f.num[Long]("end_time")
    } yield BackupStatus(state, error, start, end)
  }

  private[protocol] final def slotRanges(slots: OneOrMore[(Slot, Slot)]): List[String] = ranges(slots)
  private[protocol] final val nonNegLong: Read[Num, NonNegLong]                        = num

  /** Where a CLUSTERSCAN has got to */
  final case class ScanCursor(value: String)
  object ScanCursor {
    final val start: ScanCursor = ScanCursor("0")
  }

  /** A step of CLUSTERSCAN: the keys found, and the cursor for the next step. It's `finished` when the cursor is back at the start */
  final case class Scan(cursor: ScanCursor, keys: Seq[Key]) {
    def finished: Boolean = cursor == ScanCursor.start
  }
  private[protocol] final val scan: Read[Arr.AsTuple[Bulk *: Arr.Of[Bulk] *: EmptyTuple], Scan] =
    Read[Arr.AsTuple[Bulk *: Arr.Of[Bulk] *: EmptyTuple], String *: Seq[Key] *: EmptyTuple].map { case cursor *: keys *: _ =>
      Scan(ScanCursor(cursor), keys)
    }

  /** The options of CLUSTERSCAN: the keys matching a pattern, about how many to look at in a step, the keys of a type, and those of one
    * slot
    */
  final class ScanOptions private (val params: List[String]) {
    def pattern(pattern: GlobPattern): ScanOptions = new ScanOptions(params ++ List("MATCH", pattern.value))
    def count(keys: PosInt): ScanOptions           = new ScanOptions(params ++ List("COUNT", Show[PosInt].show(keys)))
    def ofType(keyType: KeyP.Type): ScanOptions    = new ScanOptions(params ++ List("TYPE", KeyP.Type.typeShow.show(keyType)))
    def slot(slot: Slot): ScanOptions              = new ScanOptions(params ++ List("SLOT", Show[Slot].show(slot)))
  }
  object ScanOptions {
    private final val none = new ScanOptions(Nil)

    def pattern(pattern: GlobPattern): ScanOptions = none.pattern(pattern)
    def count(keys: PosInt): ScanOptions           = none.count(keys)
    def ofType(keyType: KeyP.Type): ScanOptions    = none.ofType(keyType)
    def slot(slot: Slot): ScanOptions              = none.slot(slot)
  }
}

/** CLUSTERSCAN: Valkey 9.1 */
trait ClusterScanP extends Features {
  import ClusterManagement.{Scan, ScanCursor, ScanOptions}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLUSTERSCAN: COUNT MATCH SLOT TYPE")

  /** A step through the keys of the cluster, from `ClusterScanCursor.start`. A cursor names a slot, so the next step must go to the node
    * serving that slot, while a client connected to one node can only walk what that node serves
    */
  final def clusterscan(cursor: ScanCursor): Single.Aux[Scan] =
    Protocol("CLUSTERSCAN", cursor.value :: Nil).using(RESPRead.instance(ClusterManagement.scan))
  final def clusterscan(cursor: ScanCursor, options: ScanOptions): Single.Aux[Scan] =
    Protocol("CLUSTERSCAN", cursor.value :: options.params).using(RESPRead.instance(ClusterManagement.scan))
}

/** CLUSTER SHARDS, CLUSTER LINKS, CLUSTER ADDSLOTSRANGE and CLUSTER DELSLOTSRANGE: Redis 7.0 */
trait ClusterShardsCoreP extends Features {
  import ClusterManagement.Shard

  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLUSTER SHARDS")

  final val shards: Single.Aux[Seq[Shard]] = Protocol("CLUSTER", "SHARDS").using(RESPRead.instance(ClusterManagement.shards))
}

trait ClusterShardsP extends ClusterShardsCoreP {
  import ClusterManagement.{ClusterLink, slotRanges}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("CLUSTER ADDSLOTSRANGE", "CLUSTER DELSLOTSRANGE", "CLUSTER LINKS")

  /** Serves the slots of each range, from the first to the last of it */
  final def addslotsrange(ranges: OneOrMore[(Slot, Slot)]): Single.Aux[OK] =
    Protocol("CLUSTER", "ADDSLOTSRANGE" :: slotRanges(ranges)).as[Str, OK]
  final def delslotsrange(ranges: OneOrMore[(Slot, Slot)]): Single.Aux[OK] =
    Protocol("CLUSTER", "DELSLOTSRANGE" :: slotRanges(ranges)).as[Str, OK]

  final val links: Single.Aux[Seq[ClusterLink]] = Protocol("CLUSTER", "LINKS").using(RESPRead.instance(ClusterManagement.links))
}

/** CLUSTER MYSHARDID: Redis 7.2 */
trait ClusterMyShardIdP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLUSTER MYSHARDID")

  final val myshardid: Single.Aux[String] = Protocol("CLUSTER", "MYSHARDID").as[Bulk, String]
}

/** CLUSTER SLOT-STATS: Redis 8.2, Valkey 8.0 */
trait ClusterSlotStatsP extends Features {
  import ClusterManagement.{SlotStats, SlotStatsMetric}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLUSTER SLOT-STATS: ASC DESC LIMIT ORDERBY SLOTSRANGE")

  private final val stats = RESPRead.instance(ClusterManagement.slotStats)

  /** The figures of each slot from `from` to `to` that this node serves */
  final def slotstats(from: Slot, to: Slot): Single.Aux[Seq[SlotStats]] =
    Protocol("CLUSTER", "SLOT-STATS" *: "SLOTSRANGE" *: from *: to *: EmptyTuple).using(stats)

  /** The figures of this node's slots, ordered by a metric */
  final def slotstats(metric: SlotStatsMetric): Single.Aux[Seq[SlotStats]] =
    Protocol("CLUSTER", "SLOT-STATS" *: "ORDERBY" *: metric *: EmptyTuple).using(stats)
  final def slotstats(metric: SlotStatsMetric, limit: PosInt): Single.Aux[Seq[SlotStats]] =
    Protocol("CLUSTER", "SLOT-STATS" *: "ORDERBY" *: metric *: "LIMIT" *: limit *: EmptyTuple).using(stats)
  final def slotstats(metric: SlotStatsMetric, direction: Direction): Single.Aux[Seq[SlotStats]] =
    Protocol("CLUSTER", "SLOT-STATS" *: "ORDERBY" *: metric *: direction *: EmptyTuple).using(stats)
  final def slotstats(metric: SlotStatsMetric, limit: PosInt, direction: Direction): Single.Aux[Seq[SlotStats]] =
    Protocol("CLUSTER", "SLOT-STATS" *: "ORDERBY" *: metric *: "LIMIT" *: limit *: direction *: EmptyTuple).using(stats)
}

/** CLUSTER MIGRATION and TRIMSLOTS: Redis 8.4 */
trait ClusterMigrationP extends Features {
  import ClusterManagement.{SlotMigration, slotRanges}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("CLUSTER MIGRATION: ALL CANCEL ID IMPORT STATUS", "TRIMSLOTS: RANGES")

  object migration {

    /** Moves the slots of each range to this node from the nodes serving them, and replies with the migration's id */
    def importslots(ranges: OneOrMore[(Slot, Slot)]): Single.Aux[String] =
      Protocol("CLUSTER", "MIGRATION" :: "IMPORT" :: slotRanges(ranges)).as[Bulk, String]

    /** Cancels a migration, or all of them, and replies how many it cancelled */
    def cancel(id: Key): Single.Aux[NonNegLong] =
      Protocol("CLUSTER", "MIGRATION" :: "CANCEL" :: "ID" :: id.value :: Nil).as[Num, NonNegLong]
    val cancelall: Single.Aux[NonNegLong] = Protocol("CLUSTER", "MIGRATION" :: "CANCEL" :: "ALL" :: Nil).as[Num, NonNegLong]

    /** The migrations in progress, or the one with that id */
    val status: Single.Aux[Seq[SlotMigration]] =
      Protocol("CLUSTER", "MIGRATION" :: "STATUS" :: "ALL" :: Nil).using(RESPRead.instance(ClusterManagement.slotMigrations))
    def status(id: Key): Single.Aux[Seq[SlotMigration]] =
      Protocol("CLUSTER", "MIGRATION" :: "STATUS" :: "ID" :: id.value :: Nil).using(RESPRead.instance(ClusterManagement.slotMigrations))
  }

  /** Deletes the keys of the slots in each range from this node */
  final def trimslots(ranges: OneOrMore[(Slot, Slot)]): Single.Aux[OK] =
    Protocol("TRIMSLOTS", "RANGES" :: Show[Int].show(ranges.value.length) :: slotRanges(ranges)).as[Str, OK]
}

/** CLUSTER SETSLOT with TIMEOUT: Valkey 8.0 */
trait ClusterSetSlotTimeoutP extends Features {
  import ClusterP.SetSlotMode

  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLUSTER SETSLOT: TIMEOUT")

  /** Like `setslot`, waiting at most `milliseconds` for the replicas to take the change */
  final def setslot(slot: Slot, milliseconds: NonNegLong): Single.Aux[OK] =
    Protocol("CLUSTER", "SETSLOT" *: slot *: "STABLE" *: "TIMEOUT" *: milliseconds *: EmptyTuple).as[Str, OK]
  final def setslot(slot: Slot, mode: SetSlotMode, node: NodeId, milliseconds: NonNegLong): Single.Aux[OK] =
    Protocol("CLUSTER", "SETSLOT" *: slot *: mode *: node *: "TIMEOUT" *: milliseconds *: EmptyTuple).as[Str, OK]
}

/** CLUSTER REPLICATE NO ONE: Valkey 9.0 */
trait ClusterReplicateNoOneP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLUSTER REPLICATE: NO ONE")

  /** Makes this replica a primary with no slots */
  final val replicatenoone: Single.Aux[OK] = Protocol("CLUSTER", "REPLICATE" :: "NO" :: "ONE" :: Nil).as[Str, OK]
}

/** CLUSTER MIGRATESLOTS, CLUSTER GETSLOTMIGRATIONS, CLUSTER CANCELSLOTMIGRATIONS and CLUSTER FLUSHSLOT: Valkey 9.0 */
trait ClusterSlotMigrationsP extends Features {
  import ClusterManagement.{FlushMode, SlotMigration, slotRanges}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "CLUSTER CANCELSLOTMIGRATIONS",
      "CLUSTER FLUSHSLOT: ASYNC SYNC",
      "CLUSTER GETSLOTMIGRATIONS",
      "CLUSTER MIGRATESLOTS: NODE SLOTSRANGE"
    )

  /** Moves the slots of each range from this node to `node` */
  final def migrateslots(ranges: OneOrMore[(Slot, Slot)], node: NodeId): Single.Aux[OK] =
    Protocol("CLUSTER", "MIGRATESLOTS" :: "SLOTSRANGE" :: slotRanges(ranges) ++ List("NODE", node.value)).as[Str, OK]

  /** The migrations in progress and those finished recently */
  final val getslotmigrations: Single.Aux[Seq[SlotMigration]] =
    Protocol("CLUSTER", "GETSLOTMIGRATIONS").using(RESPRead.instance(ClusterManagement.slotMigrations))

  final val cancelslotmigrations: Single.Aux[OK] = Protocol("CLUSTER", "CANCELSLOTMIGRATIONS").as[Str, OK]

  /** Deletes the keys of a slot */
  final def flushslot(slot: Slot): Single.Aux[OK]                  = Protocol("CLUSTER", "FLUSHSLOT" *: slot *: EmptyTuple).as[Str, OK]
  final def flushslot(slot: Slot, mode: FlushMode): Single.Aux[OK] =
    Protocol("CLUSTER", "FLUSHSLOT" *: slot *: mode *: EmptyTuple).as[Str, OK]
}

/** BACKUP: Redis 8.10 */
trait ServerBackupP extends Features {
  import ClusterManagement.BackupStatus

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("BACKUP ABORT", "BACKUP CLEANUP", "BACKUP HELP", "BACKUP LIST", "BACKUP SEAL", "BACKUP START", "BACKUP STATUS")

  /** A backup of the server's data into its `backupdirname` directory: started, then sealed when complete, then cleaned up. An aborted
    * backup is left as failed until it is cleaned up
    */
  object backup {
    val help: Single.Aux[Seq[String]]    = Protocol.help("BACKUP")
    val start: Single.Aux[OK]            = Protocol("BACKUP", "START").as[Str, OK]
    val status: Single.Aux[BackupStatus] = Protocol("BACKUP", "STATUS").using(RESPRead.instance(ClusterManagement.backupStatus))
    val list: Single.Aux[Seq[String]]    = Protocol("BACKUP", "LIST").as[Arr.Of[Bulk], Seq[String]]
    val seal: Single.Aux[OK]             = Protocol("BACKUP", "SEAL").as[Str, OK]
    val abort: Single.Aux[OK]            = Protocol("BACKUP", "ABORT").as[Str, OK]
    val cleanup: Single.Aux[OK]          = Protocol("BACKUP", "CLEANUP").as[Str, OK]
  }
}
