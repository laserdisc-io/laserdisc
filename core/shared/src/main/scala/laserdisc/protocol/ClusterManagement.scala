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

  private final val num = Read.num2NonNegLongRead

  private final def long(s: String): Option[Long] = s match { case ToLong(l) => Some(l); case _ => None }
  private final def int(s: String): Option[Int]   = s match { case ToInt(i) => Some(i); case _ => None }

  private final def ranges(slots: OneOrMore[(Slot, Slot)]): List[String] = slots.value.flatMap { case (from, to) =>
    List(from.value.toString, to.value.toString)
  }

  // the pairs of a [name, value, name, value…] array, with values that are text or whole numbers kept as text
  private final def fields(elements: Seq[RESP]): Option[Map[String, String]] = {
    val read = elements.grouped(2).toList.map {
      case Seq(Bulk(k), Bulk(v)) => Some(k -> v)
      case Seq(Bulk(k), Num(v))  => Some(k -> v.toString)
      case Seq(Bulk(k), Str(v))  => Some(k -> v)
      case _                     => None
    }
    if (read.forall(_.isDefined)) Some(read.flatten.toMap) else None
  }

  private final def maps(name: String): Read[Arr, Seq[Map[String, String]]] = Read.instance { case Arr(elements) =>
    elements.foldRight[Either[RESPDecErr, List[Map[String, String]]]](Right(Nil)) {
      case (Arr(fs), Right(ms)) =>
        fields(fs).fold[Either[RESPDecErr, List[Map[String, String]]]](Left(RESPDecErr(s"Unexpected $name. Was $fs")))(m => Right(m :: ms))
      case (other, Right(_)) => Left(RESPDecErr(s"Unexpected $name. Was $other"))
      case (_, left)         => left
    }
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

  private[protocol] final val shards: Read[Arr, Seq[Shard]] = {
    def node(fs: Map[String, String]): Option[ShardNode] =
      for {
        id     <- fs.get("id")
        ip     <- fs.get("ip")
        end    <- fs.get("endpoint")
        role   <- fs.get("role")
        health <- fs.get("health")
        offset <- fs.get("replication-offset").flatMap(long)
      } yield ShardNode(id, ip, end, role, health, offset, fs.get("port").flatMap(int), fs.get("tls-port").flatMap(int), fs.get("hostname"))
    def slots(elements: Seq[RESP]): Option[Seq[(Slot, Slot)]] = {
      val read = elements.grouped(2).toList.map {
        case Seq(Num(ToInt(Slot(from))), Num(ToInt(Slot(to)))) => Some(from -> to)
        case _                                                 => None
      }
      if (read.forall(_.isDefined)) Some(read.flatten) else None
    }
    Read.instance { case Arr(shards) =>
      shards.foldRight[Either[RESPDecErr, List[Shard]]](Right(Nil)) {
        case (Arr(fs), Right(ss)) =>
          val byName = fs.grouped(2).collect { case Seq(Bulk(k), v) => k -> v }.toMap
          val read   = for {
            ranges <- byName.get("slots").collect { case Arr(es) => es }.flatMap(slots)
            nodes  <- byName.get("nodes").collect { case Arr(ns) => ns }.flatMap { ns =>
              val read = ns.toList.map { case Arr(nfs) => fields(nfs).flatMap(node); case _ => None }
              if (read.forall(_.isDefined)) Some(read.flatten) else None
            }
          } yield Shard(ranges, nodes, byName.get("id").collect { case Bulk(id) => id })
          read.fold[Either[RESPDecErr, List[Shard]]](Left(RESPDecErr(s"Unexpected shard. Was $fs")))(s => Right(s :: ss))
        case (other, Right(_)) => Left(RESPDecErr(s"Unexpected shard. Was $other"))
        case (_, left)         => left
      }
    }
  }

  /** A link of CLUSTER LINKS: a connection to (`to`) or from (`from`) another node, with its buffer use in bytes */
  final case class ClusterLink(
      direction: String,
      node: String,
      createTime: Long,
      events: String,
      sendBufferAllocated: Long,
      sendBufferUsed: Long
  )
  private[protocol] final val links: Read[Arr, Seq[ClusterLink]] = maps("cluster link").flatMap { ms =>
    val read = ms.toList.map { m =>
      for {
        direction <- m.get("direction")
        node      <- m.get("node")
        created   <- m.get("create-time").flatMap(long)
        events    <- m.get("events")
        allocated <- m.get("send-buffer-allocated").flatMap(long)
        used      <- m.get("send-buffer-used").flatMap(long)
      } yield ClusterLink(direction, node, created, events, allocated, used)
    }
    if (read.forall(_.isDefined)) Read.const(read.flatten) else Read.instance(_ => Left(RESPDecErr(s"Unexpected cluster links. Was $ms")))
  }

  /** The figures of a slot of CLUSTER SLOT-STATS, by metric name, like `key-count` */
  final case class SlotStats(slot: Slot, metrics: Map[String, Long])
  private[protocol] final val slotStats: Read[Arr, Seq[SlotStats]] = Read.instance { case Arr(slots) =>
    slots.foldRight[Either[RESPDecErr, List[SlotStats]]](Right(Nil)) {
      case (Arr(Num(ToInt(Slot(slot))) +: Arr(ms) +: Seq()), Right(ss)) =>
        val read = ms.grouped(2).toList.map { case Seq(Bulk(k), Num(v)) => Some(k -> v); case _ => None }
        if (read.forall(_.isDefined)) Right(SlotStats(slot, read.flatten.toMap) :: ss)
        else Left(RESPDecErr(s"Unexpected slot stats of $slot. Was $ms"))
      case (other, Right(_)) => Left(RESPDecErr(s"Unexpected slot stats. Was $other"))
      case (_, left)         => left
    }
  }

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
  private[protocol] final val slotMigrations: Read[Arr, Seq[SlotMigration]] = maps("slot migration").map(_.map(SlotMigration(_)))

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
  private[protocol] final val backupStatus: Read[Arr, BackupStatus] = Read.instance { case Arr(fs) =>
    fields(fs)
      .flatMap { m =>
        for {
          state <- m.get("state")
          error <- m.get("error")
          start <- m.get("start_time").flatMap(long)
          end   <- m.get("end_time").flatMap(long)
        } yield BackupStatus(state, error, start, end)
      }
      .toRight(RESPDecErr(s"Unexpected backup status. Was $fs"))
  }

  private[protocol] final def slotRanges(slots: OneOrMore[(Slot, Slot)]): List[String] = ranges(slots)
  private[protocol] final val nonNegLong: Read[Num, NonNegLong]                        = num
}

/** CLUSTER SHARDS, CLUSTER LINKS, CLUSTER ADDSLOTSRANGE and CLUSTER DELSLOTSRANGE: Redis 7.0 */
trait ClusterShardsP extends Features {
  import ClusterManagement.{ClusterLink, Shard, slotRanges}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("CLUSTER ADDSLOTSRANGE", "CLUSTER DELSLOTSRANGE", "CLUSTER LINKS", "CLUSTER SHARDS")

  /** Serves the slots of each range, from the first to the last of it */
  final def addslotsrange(ranges: OneOrMore[(Slot, Slot)]): Single.Aux[OK] =
    Protocol("CLUSTER", "ADDSLOTSRANGE" :: slotRanges(ranges)).as[Str, OK]
  final def delslotsrange(ranges: OneOrMore[(Slot, Slot)]): Single.Aux[OK] =
    Protocol("CLUSTER", "DELSLOTSRANGE" :: slotRanges(ranges)).as[Str, OK]

  final val links: Single.Aux[Seq[ClusterLink]] = Protocol("CLUSTER", "LINKS").using(RESPRead.instance(ClusterManagement.links))
  final val shards: Single.Aux[Seq[Shard]]      = Protocol("CLUSTER", "SHARDS").using(RESPRead.instance(ClusterManagement.shards))
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
    Protocol("TRIMSLOTS", "RANGES" :: ranges.value.length.toString :: slotRanges(ranges)).as[Str, OK]
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
    super.supported ++ List("BACKUP ABORT", "BACKUP CLEANUP", "BACKUP LIST", "BACKUP SEAL", "BACKUP START", "BACKUP STATUS")

  /** A backup of the server's data into its `backupdirname` directory: started, then sealed when complete, then cleaned up. An aborted
    * backup is left as failed until it is cleaned up
    */
  object backup {
    val start: Single.Aux[OK]            = Protocol("BACKUP", "START").as[Str, OK]
    val status: Single.Aux[BackupStatus] = Protocol("BACKUP", "STATUS").using(RESPRead.instance(ClusterManagement.backupStatus))
    val list: Single.Aux[Seq[String]]    = Protocol("BACKUP", "LIST").as[Arr.Of[Bulk], Seq[String]]
    val seal: Single.Aux[OK]             = Protocol("BACKUP", "SEAL").as[Str, OK]
    val abort: Single.Aux[OK]            = Protocol("BACKUP", "ABORT").as[Str, OK]
    val cleanup: Single.Aux[OK]          = Protocol("BACKUP", "CLEANUP").as[Str, OK]
  }
}
