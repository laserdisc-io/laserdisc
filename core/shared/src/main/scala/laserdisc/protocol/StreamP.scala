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

object StreamP {

  /** The id of a stream entry: the time it was added, in milliseconds, and a sequence number for entries added in the same millisecond */
  final case class Id(milliseconds: NonNegLong, sequence: NonNegLong)
  object Id {
    private final val IdRegex = raw"(\d+)-(\d+)".r

    implicit final val idShow: Show[Id]            = Show.instance(id => s"${id.milliseconds.value}-${id.sequence.value}")
    implicit final val bulk2IdRead: Read[Bulk, Id] = Read.instancePF("Bulk(stream id)") {
      case Bulk(IdRegex(ToLong(NonNegLong(ms)), ToLong(NonNegLong(seq)))) => Id(ms, seq)
    }
  }

  /** The id of an entry to add at the given milliseconds, for XADD: the server picks the sequence */
  final case class IdAt private[laserdisc] (milliseconds: NonNegLong)
  trait IdAtBuilders {
    def apply(milliseconds: NonNegLong): IdAt
  }
  private[laserdisc] object IdAt extends IdAtBuilders {
    def apply(milliseconds: NonNegLong): IdAt = new IdAt(milliseconds)

    implicit final val idAtShow: Show[IdAt] = Show.instance(id => s"${id.milliseconds.value}-*")
  }

  /** An entry of a stream: its id and its fields, in the order they were added. An entry has at least one field, so empty fields mean the
    * entry was deleted while still pending, as XREADGROUP shows it when reading a consumer's history
    */
  final case class Entry[A](id: Id, fields: Seq[(Key, A)])

  /** Where XRANGE and XREVRANGE start or end: the smallest or largest id, an id itself, or the first id after or before it */
  sealed trait Bound { def param: String }
  object Bound       {
    private final class Of(override final val param: String) extends Bound

    final val min: Bound         = new Of("-")
    final val max: Bound         = new Of("+")
    def inclusive(id: Id): Bound = new Of(Show[Id].show(id))
    def exclusive(id: Id): Bound = new Of(s"(${Show[Id].show(id)}")
  }

  /** Where XREAD reads a stream from: after an id, or only entries added after the read (`$`) */
  sealed trait From { def param: String }
  object From       {
    private final class Of(override final val param: String) extends From

    def after(id: Id): From = new Of(Show[Id].show(id))
    final val latest: From  = new Of("$")
  }

  /** MAXLEN and MINID, for XADD and XTRIM: keep at most so many entries, or only entries from an id on. The approximate forms (`~`) let the
    * server trim less, which is faster, and LIMIT caps how many entries one trim removes. The exact forms (`=`) trim as the plain ones do,
    * saying so explicitly
    */
  sealed trait Trim { def params: List[String] }
  object Trim       {
    private final class Of(override final val params: List[String]) extends Trim

    def maxLen(count: NonNegLong): Trim                       = new Of(List("MAXLEN", Show[NonNegLong].show(count)))
    def maxLenExact(count: NonNegLong): Trim                  = new Of(List("MAXLEN", "=", Show[NonNegLong].show(count)))
    def maxLenApprox(count: NonNegLong): Trim                 = new Of(List("MAXLEN", "~", Show[NonNegLong].show(count)))
    def maxLenApprox(count: NonNegLong, limit: PosLong): Trim =
      new Of(List("MAXLEN", "~", Show[NonNegLong].show(count), "LIMIT", Show[PosLong].show(limit)))
    def minId(id: Id): Trim                       = new Of(List("MINID", Show[Id].show(id)))
    def minIdExact(id: Id): Trim                  = new Of(List("MINID", "=", Show[Id].show(id)))
    def minIdApprox(id: Id): Trim                 = new Of(List("MINID", "~", Show[Id].show(id)))
    def minIdApprox(id: Id, limit: PosLong): Trim = new Of(List("MINID", "~", Show[Id].show(id), "LIMIT", Show[PosLong].show(limit)))
  }

  /** Where XREADGROUP reads a stream from: entries never delivered to the group yet (`>`), or the consumer's pending entries after an id */
  sealed trait GroupFrom { def param: String }
  object GroupFrom       {
    private final class Of(override final val param: String) extends GroupFrom

    final val newEntries: GroupFrom     = new Of(">")
    def pendingAfter(id: Id): GroupFrom = new Of(Show[Id].show(id))
  }

  /** MKSTREAM, for XGROUP CREATE: create the stream when it doesn't exist */
  sealed trait MkStream
  object MkStream extends MkStream {
    implicit val mkStreamShow: Show[MkStream] = Show.const("MKSTREAM")
  }

  /** NOACK, for XREADGROUP: don't keep the entries read as pending, as if they were acknowledged at once */
  sealed trait NoAck
  object NoAck extends NoAck {
    implicit val noAckShow: Show[NoAck] = Show.const("NOACK")
  }

  /** XPENDING's summary: how many entries are pending, the smallest and largest of their ids, and how many each consumer has */
  final case class PendingSummary(count: NonNegLong, smallest: Option[Id], largest: Option[Id], consumers: Seq[(Key, NonNegLong)])

  /** A pending entry of XPENDING: its id, the consumer it was delivered to, how long ago in milliseconds, and how many times. An entry
    * given back with XNACK has no consumer or idle time
    */
  final case class PendingEntry(id: Id, consumer: Option[Key], idleMilliseconds: Option[NonNegLong], deliveries: NonNegLong)

  // Dragonfly sends how many entries a consumer has as a number
  private final type PendingSummaryReply = Arr.AsTuple[
    Num *: OrNullBulk[Bulk] *: OrNullBulk[Bulk] *: OrNilArr[Arr.Of[Arr.AsTuple[Bulk *: OneOf[Bulk *: Num *: EmptyTuple] *: EmptyTuple]]] *:
      EmptyTuple
  ]
  private[protocol] final val pendingSummary: Read[PendingSummaryReply, PendingSummary] =
    Read[PendingSummaryReply, NonNegLong *: Option[Id] *: Option[Id] *: Option[Seq[(Key, NonNegLong)]] *: EmptyTuple].map {
      case count *: smallest *: largest *: consumers *: _ => PendingSummary(count, smallest, largest, consumers.getOrElse(Nil))
    }

  private final type PendingEntryReply = Arr.AsTuple[Bulk *: Bulk *: Num *: Num *: EmptyTuple]
  // an entry given back with XNACK has no consumer, sent as an empty name, and no idle time, sent as -1
  private final val pendingConsumer: Read[Bulk, Option[Key]] = Read.instance {
    case Bulk("") => Right(None)
    case other    => Read[Bulk, Key].read(other).map(Some(_))
  }
  private final val pendingIdle: Read[Num, Option[NonNegLong]] = Read.instance {
    case Num(-1L) => Right(None)
    case other    => Read[Num, NonNegLong].read(other).map(Some(_))
  }
  private implicit final val pendingEntry: Read[PendingEntryReply, PendingEntry] = Read.instance { reply =>
    Read[PendingEntryReply, Id *: Bulk *: Num *: NonNegLong *: EmptyTuple].read(reply).flatMap {
      case id *: consumer *: idle *: deliveries *: _ =>
        for {
          consumer <- pendingConsumer.read(consumer)
          idle     <- pendingIdle.read(idle)
        } yield PendingEntry(id, consumer, idle, deliveries)
    }
  }
  private[protocol] final val pendingEntries: Read[Arr.Of[PendingEntryReply], Seq[PendingEntry]] =
    Read[Arr.Of[PendingEntryReply], Seq[PendingEntry]]

  /** IDLE, TIME, RETRYCOUNT, FORCE and LASTID, for XCLAIM: set how long the entries have been idle (or since when), how many times they
    * were delivered, claim them even if they weren't pending, and move the group's last delivered id on
    */
  final class ClaimOptions private (val params: List[String]) {
    def idle(milliseconds: NonNegLong): ClaimOptions = new ClaimOptions(params ++ List("IDLE", Show[NonNegLong].show(milliseconds)))
    def time(millisecondsTimestamp: NonNegLong): ClaimOptions =
      new ClaimOptions(params ++ List("TIME", Show[NonNegLong].show(millisecondsTimestamp)))
    def retryCount(count: NonNegLong): ClaimOptions = new ClaimOptions(params ++ List("RETRYCOUNT", Show[NonNegLong].show(count)))
    def force: ClaimOptions                         = new ClaimOptions(params :+ "FORCE")
    def lastId(id: Id): ClaimOptions                = new ClaimOptions(params ++ List("LASTID", Show[Id].show(id)))
  }
  object ClaimOptions {
    final val none: ClaimOptions = new ClaimOptions(Nil)
  }

  /** XAUTOCLAIM's reply: the id to start the next call from (0-0 when done), the entries claimed, and the ids of deleted entries it dropped
    * from the pending list. Redis 6.2 doesn't report those, so they're empty there
    */
  final case class AutoClaimed[A](next: Id, claimed: Seq[A], deleted: Seq[Id])

  /** XINFO STREAM: its length, the radix tree's size, the last id added, how many groups it has, its first and last entries, and the other
    * figures the server reports (like `entries-added` from Redis 7.0), as text
    */
  final case class StreamInfo[A](
      length: NonNegLong,
      radixTreeKeys: NonNegLong,
      radixTreeNodes: NonNegLong,
      lastGeneratedId: Id,
      groups: NonNegLong,
      firstEntry: Option[Entry[A]],
      lastEntry: Option[Entry[A]],
      others: Map[String, String]
  )

  /** XINFO STREAM FULL: its length, the radix tree's size, the last id added, its entries from the first on, its groups, and the other
    * figures the server reports (like `entries-added` from Redis 7.0), as text
    */
  final case class StreamInfoFull[A](
      length: NonNegLong,
      radixTreeKeys: NonNegLong,
      radixTreeNodes: NonNegLong,
      lastGeneratedId: Id,
      entries: Seq[Entry[A]],
      groups: Seq[GroupInfoFull],
      others: Map[String, String]
  )

  /** A group of XINFO STREAM FULL. Redis 7.0 adds how many entries it read and how far behind it is, which may be unknown */
  final case class GroupInfoFull(
      name: Key,
      lastDeliveredId: Id,
      entriesRead: Option[NonNegLong],
      lag: Option[NonNegLong],
      pendingCount: NonNegLong,
      pending: Seq[GroupPendingFull],
      consumers: Seq[ConsumerInfoFull],
      others: Map[String, String]
  )

  /** A pending entry of a group in XINFO STREAM FULL: its id, its consumer, when it was last delivered, in milliseconds since the epoch,
    * and how many times. An entry given back with XNACK has no consumer, and 0 for the time and the deliveries
    */
  final case class GroupPendingFull(id: Id, consumer: Option[Key], deliveredAt: NonNegLong, deliveries: NonNegLong)

  /** A consumer of XINFO STREAM FULL: when it last tried a read and, from Redis 7.2, when it last read something, if ever, both in
    * milliseconds since the epoch, and its pending entries
    */
  final case class ConsumerInfoFull(
      name: Key,
      seenAt: NonNegLong,
      activeAt: Option[NonNegLong],
      pendingCount: NonNegLong,
      pending: Seq[ConsumerPendingFull]
  )

  /** A pending entry of a consumer in XINFO STREAM FULL: its id, when it was last delivered, in milliseconds since the epoch, and how many
    * times
    */
  final case class ConsumerPendingFull(id: Id, deliveredAt: NonNegLong, deliveries: NonNegLong)

  /** A group of XINFO GROUPS. Redis 7.0 adds how many entries it read and how far behind it is, which may be unknown */
  final case class GroupInfo(
      name: Key,
      consumers: NonNegLong,
      pending: NonNegLong,
      lastDeliveredId: Id,
      entriesRead: Option[NonNegLong],
      lag: Option[NonNegLong]
  )

  /** A consumer of XINFO CONSUMERS: its pending entries and how long ago, in milliseconds, it last tried a read. Redis 7.2 adds how long
    * ago it last read something
    */
  final case class ConsumerInfo(name: Key, pending: NonNegLong, idle: NonNegLong, inactive: Option[Long])

  /** KEEPREF, DELREF and ACKED, for XADD, XTRIM, XDELEX and XACKDEL: when deleting entries, keep the groups' references to them, delete
    * those too, or only delete entries every group has acknowledged. Redis 8.2
    */
  sealed trait RefPolicy
  trait RefPolicyBuilders {
    val keepRef: RefPolicy
    val delRef: RefPolicy
    val acked: RefPolicy
  }
  private[laserdisc] object RefPolicy extends RefPolicyBuilders {
    object keepRef extends RefPolicy
    object delRef  extends RefPolicy
    object acked   extends RefPolicy

    implicit val refPolicyShow: Show[RefPolicy] = Show.instance {
      case `keepRef` => "KEEPREF"
      case `delRef`  => "DELREF"
      case `acked`   => "ACKED"
    }
  }

  /** What XDELEX and XACKDEL did to an entry: deleted it, found no such entry, or kept it because a group still needs it */
  sealed trait DeleteResult
  object DeleteResult {
    case object Deleted    extends DeleteResult
    case object NotFound   extends DeleteResult
    case object Referenced extends DeleteResult

    implicit final val num2DeleteResultRead: Read[Num, DeleteResult] = Read.instance {
      case Num(1)     => Right(Deleted)
      case Num(-1)    => Right(NotFound)
      case Num(2)     => Right(Referenced)
      case Num(other) => Left(RESPDecErr(s"Unexpected stream delete result. Was $other"))
    }
  }

  /** IDMP and IDMPAUTO, for XADD: add the entry only once for a producer and an idempotency id (or one the server derives from the fields),
    * replying with the first entry's id on a repeat. Redis 8.6, with ids the server picks
    */
  sealed trait Idmp  { def params: List[String] }
  trait IdmpBuilders {
    def of(producer: Key, id: Key): Idmp
    def auto(producer: Key): Idmp
  }
  private[laserdisc] object Idmp extends IdmpBuilders {
    private final class Of(override final val params: List[String]) extends Idmp

    def of(producer: Key, id: Key): Idmp = new Of(List("IDMP", producer.value, id.value))
    def auto(producer: Key): Idmp        = new Of(List("IDMPAUTO", producer.value))
  }

  /** IDMP-DURATION and IDMP-MAXSIZE, for XCFGSET: how long, in seconds, and how many idempotency ids a stream remembers per producer */
  final class IdmpConfig private (val params: List[String]) {
    def duration(seconds: PosLong): IdmpConfig = new IdmpConfig(params ++ List("IDMP-DURATION", Show[PosLong].show(seconds)))
    def maxSize(ids: PosLong): IdmpConfig      = new IdmpConfig(params ++ List("IDMP-MAXSIZE", Show[PosLong].show(ids)))
  }
  trait IdmpConfigBuilders {
    def duration(seconds: PosLong): IdmpConfig
    def maxSize(ids: PosLong): IdmpConfig
  }
  private[laserdisc] object IdmpConfig extends IdmpConfigBuilders {
    def duration(seconds: PosLong): IdmpConfig = new IdmpConfig(List("IDMP-DURATION", Show[PosLong].show(seconds)))
    def maxSize(ids: PosLong): IdmpConfig      = new IdmpConfig(List("IDMP-MAXSIZE", Show[PosLong].show(ids)))
  }

  /** SILENT, FAIL and FATAL, for XNACK: the consumer gave the entries back without processing them, failed to process them, or found they
    * can never be processed. Redis 8.8
    */
  sealed trait NackMode
  trait NackModeBuilders {
    val silent: NackMode
    val fail: NackMode
    val fatal: NackMode
  }
  private[laserdisc] object NackMode extends NackModeBuilders {
    object silent extends NackMode
    object fail   extends NackMode
    object fatal  extends NackMode

    implicit val nackModeShow: Show[NackMode] = Show.instance {
      case `silent` => "SILENT"
      case `fail`   => "FAIL"
      case `fatal`  => "FATAL"
    }
  }

  /** RETRYCOUNT and FORCE, for XNACK: set how many times the entries were delivered, and also give back entries the group doesn't have
    * pending, such as acknowledged ones. Redis 8.8
    */
  final class NackOptions private (val params: List[String]) {
    def retryCount(count: NonNegLong): NackOptions = new NackOptions(params ++ List("RETRYCOUNT", Show[NonNegLong].show(count)))
    def force: NackOptions                         = new NackOptions(params :+ "FORCE")
  }
  trait NackOptionsBuilders {
    def retryCount(count: NonNegLong): NackOptions
    def force: NackOptions
  }
  private[laserdisc] object NackOptions extends NackOptionsBuilders {
    def retryCount(count: NonNegLong): NackOptions = new NackOptions(List("RETRYCOUNT", Show[NonNegLong].show(count)))
    def force: NackOptions                         = new NackOptions(List("FORCE"))
  }

  /** An entry read with XREADGROUP's CLAIM: its id and fields, how long it had been idle, in milliseconds, and how many times it had been
    * delivered. Both are 0 for a new entry. Redis 8.4
    */
  final case class ClaimedEntry[A](id: Id, fields: Seq[(Key, A)], idleMilliseconds: NonNegLong, deliveries: NonNegLong)

  /** MAXCOUNT and MAXSIZE, for XREAD and XREADGROUP: end the whole read, across all its streams, after so many entries, or once it has
    * reached about so many bytes, which always lets at least one entry through. COUNT, instead, applies to each stream. Redis 8.10
    */
  final class ReadLimits private (val params: List[String]) {
    def maxCount(entries: PosLong): ReadLimits = new ReadLimits(params ++ List("MAXCOUNT", Show[PosLong].show(entries)))
    def maxSize(bytes: PosLong): ReadLimits    = new ReadLimits(params ++ List("MAXSIZE", Show[PosLong].show(bytes)))
  }
  trait ReadLimitsBuilders {
    def maxCount(entries: PosLong): ReadLimits
    def maxSize(bytes: PosLong): ReadLimits
  }
  private[laserdisc] object ReadLimits extends ReadLimitsBuilders {
    def maxCount(entries: PosLong): ReadLimits = new ReadLimits(List("MAXCOUNT", Show[PosLong].show(entries)))
    def maxSize(bytes: PosLong): ReadLimits    = new ReadLimits(List("MAXSIZE", Show[PosLong].show(bytes)))
  }

  /** NOMKSTREAM, for XADD: don't create the stream when it doesn't exist */
  sealed trait NoMkStream
  object NoMkStream extends NoMkStream {
    implicit val noMkStreamShow: Show[NoMkStream] = Show.const("NOMKSTREAM")
  }

  // an entry's id and its fields
  private[protocol] final type EntryReply = Arr.AsTuple[Bulk *: OrNilArr[Arr.OfPairs[Bulk, Bulk]] *: EmptyTuple]
  private implicit final def entry[A: Read[Bulk, _]]: Read[EntryReply, Entry[A]] =
    Read[EntryReply, Id *: Option[Seq[(Key, A)]] *: EmptyTuple].map { case id *: fields *: _ => Entry(id, fields.getOrElse(Nil)) }

  private[protocol] final def entries[A: Read[Bulk, _]]: Read[Arr.Of[EntryReply], Seq[Entry[A]]] = Read[Arr.Of[EntryReply], Seq[Entry[A]]]

  private[protocol] final def streams[
      A: Read[Bulk, _]
  ]: Read[Arr.Of[Arr.AsTuple[Bulk *: Arr.Of[EntryReply] *: EmptyTuple]], Seq[KV[Seq[Entry[A]]]]] =
    Read[Arr.Of[Arr.AsTuple[Bulk *: Arr.Of[EntryReply] *: EmptyTuple]], Seq[KV[Seq[Entry[A]]]]]

  // the streams' entries, or nil when there are none
  private[protocol] final def maybeStreams[A: Read[Bulk, _]]: RESPRead[Option[Seq[KV[Seq[Entry[A]]]]]] =
    RESPRead[OrNilArr[Arr.Of[Arr.AsTuple[Bulk *: Arr.Of[EntryReply] *: EmptyTuple]]], Option[Seq[KV[Seq[Entry[A]]]]]]

  // Redis 6.2 replies with nil for each deleted entry it claims; Redis 7.0 leaves them out
  private[protocol] final def claimedOrNil[A: Read[Bulk, _]]: Read[Arr.Of[OrNullBulk[EntryReply]], Seq[Entry[A]]] =
    Read[Arr.Of[OrNullBulk[EntryReply]], Seq[Option[Entry[A]]]].map(_.flatten)

  private[protocol] final val ids: Read[Arr.Of[Bulk], Seq[Id]] = Read[Arr.Of[Bulk], Seq[Id]]

  private final type ClaimedEntryReply = Arr.AsTuple[Bulk *: OrNilArr[Arr.OfPairs[Bulk, Bulk]] *: Num *: Num *: EmptyTuple]
  private implicit final def claimedEntry[A: Read[Bulk, _]]: Read[ClaimedEntryReply, ClaimedEntry[A]] =
    Read[ClaimedEntryReply, Id *: Option[Seq[(Key, A)]] *: NonNegLong *: NonNegLong *: EmptyTuple].map {
      case id *: fields *: idle *: deliveries *: _ => ClaimedEntry(id, fields.getOrElse(Nil), idle, deliveries)
    }

  // the entries with XREADGROUP's CLAIM, by stream, or nil when there are none
  private[protocol] final def maybeClaimed[A: Read[Bulk, _]]: RESPRead[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    RESPRead[OrNilArr[Arr.Of[Arr.AsTuple[Bulk *: Arr.Of[ClaimedEntryReply] *: EmptyTuple]]], Option[Seq[KV[Seq[ClaimedEntry[A]]]]]]

  private[protocol] final def streamsParams[K](streams: OneOrMore[(Key, K)])(param: K => String): List[String] =
    "STREAMS" :: streams.value.map(_._1.value) ++ streams.value.map(s => param(s._2))

  // CLAIM only applies to reading new entries, so each key is read from `>`
  private[protocol] final def claimParams(
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      keys: OneOrMoreKeys,
      before: List[String],
      after: List[String]
  ): List[String] =
    "GROUP" :: group.value :: consumer.value :: before ++ ("CLAIM" :: Show[NonNegLong].show(minIdleMilliseconds) :: after) ++
      ("STREAMS" :: keys.value.map(_.value) ++ keys.value.map(_ => ">"))

  // Redis 7.0 adds the ids of the entries it found deleted
  private[protocol] final def autoClaimedWithDeleted[E: RESPSub, A](
      claims: Read[E, Seq[A]]
  ): Read[Arr.AsTuple[Bulk *: E *: Arr.Of[Bulk] *: EmptyTuple], AutoClaimed[A]] = {
    implicit val read: Read[E, Seq[A]] = claims
    Read[Arr.AsTuple[Bulk *: E *: Arr.Of[Bulk] *: EmptyTuple], Id *: Seq[A] *: Seq[Id] *: EmptyTuple].map {
      case next *: cs *: deleted *: _ =>
        AutoClaimed(next, cs, deleted)
    }
  }

  // either reply, for Redis 6.2 and for a newer server that hasn't had its import moved on yet
  private[protocol] final def autoClaimed[E: RESPSub, A](
      claims: Read[E, Seq[A]]
  ): Read[OneOf[Arr.AsTuple[Bulk *: E *: EmptyTuple] *: Arr.AsTuple[Bulk *: E *: Arr.Of[Bulk] *: EmptyTuple] *: EmptyTuple], AutoClaimed[A]] = {
    implicit val read: Read[E, Seq[A]]                                              = claims
    implicit val before: Read[Arr.AsTuple[Bulk *: E *: EmptyTuple], AutoClaimed[A]] =
      Read[Arr.AsTuple[Bulk *: E *: EmptyTuple], Id *: Seq[A] *: EmptyTuple].map { case next *: cs *: _ => AutoClaimed(next, cs, Nil) }
    implicit val since: Read[Arr.AsTuple[Bulk *: E *: Arr.Of[Bulk] *: EmptyTuple], AutoClaimed[A]] = autoClaimedWithDeleted(claims)
    Read[OneOf[Arr.AsTuple[Bulk *: E *: EmptyTuple] *: Arr.AsTuple[Bulk *: E *: Arr.Of[Bulk] *: EmptyTuple] *: EmptyTuple], AutoClaimed[A]]
  }

  private[protocol] final def streamInfo[A: Read[Bulk, _]]: Read[Record, StreamInfo[A]] = {
    val typed = Set("length", "radix-tree-keys", "radix-tree-nodes", "last-generated-id", "groups", "first-entry", "last-entry")
    Read.record("stream info") { f =>
      for {
        length <- f.num[NonNegLong]("length")
        keys   <- f.num[NonNegLong]("radix-tree-keys")
        nodes  <- f.num[NonNegLong]("radix-tree-nodes")
        last   <- f.bulk[Id]("last-generated-id")
        groups <- f.num[NonNegLong]("groups")
        first  <- f[OrNullBulk[EntryReply], Option[Entry[A]]]("first-entry")
        lastE  <- f[OrNullBulk[EntryReply], Option[Entry[A]]]("last-entry")
      } yield StreamInfo(length, keys, nodes, last, groups, first, lastE, othersAsText(f, typed))
    }
  }

  private final def othersAsText(f: Fields, typed: Set[String]): Map[String, String] = f.others(typed).collect {
    case (k, Bulk(v)) => k -> v
    case (k, Num(v))  => k -> v.toString
  }

  private implicit final val groupPendingFull: Read[PendingEntryReply, GroupPendingFull] = Read.instance { reply =>
    Read[PendingEntryReply, Id *: Bulk *: NonNegLong *: NonNegLong *: EmptyTuple].read(reply).flatMap {
      case id *: consumer *: time *: deliveries *: _ => pendingConsumer.read(consumer).map(GroupPendingFull(id, _, time, deliveries))
    }
  }

  private implicit final val consumerPendingFull: Read[Arr.AsTuple[Bulk *: Num *: Num *: EmptyTuple], ConsumerPendingFull] =
    Read[Arr.AsTuple[Bulk *: Num *: Num *: EmptyTuple], Id *: NonNegLong *: NonNegLong *: EmptyTuple].map {
      case id *: time *: deliveries *: _ => ConsumerPendingFull(id, time, deliveries)
    }

  private implicit final val consumerInfoFull: Read[Record, ConsumerInfoFull] = Read.record("consumer info") { f =>
    // a consumer that hasn't read anything yet has -1
    implicit val activeAt: Read[Num, Option[NonNegLong]] = Read.numMinusOneIsNone[NonNegLong]
    for {
      name    <- f.bulk[Key]("name")
      seen    <- f.num[NonNegLong]("seen-time")
      active  <- f.optional[Num, Option[NonNegLong]]("active-time") // from Redis 7.2
      count   <- f.num[NonNegLong]("pel-count")
      pending <- f[Arr.Of[Arr.AsTuple[Bulk *: Num *: Num *: EmptyTuple]], Seq[ConsumerPendingFull]]("pending")
    } yield ConsumerInfoFull(name, seen, active.flatten, count, pending)
  }

  private implicit final val groupInfoFull: Read[Record, GroupInfoFull] = {
    val typed = Set("name", "last-delivered-id", "entries-read", "lag", "pel-count", "pending", "consumers")
    Read.record("group info") { f =>
      for {
        name        <- f.bulk[Key]("name")
        last        <- f.bulk[Id]("last-delivered-id")
        entriesRead <- f.optional[OrNullBulk[Num], Option[NonNegLong]]("entries-read") // from Redis 7.0, and nil when unknown
        lag         <- f.optional[OrNullBulk[Num], Option[NonNegLong]]("lag")
        count       <- f.num[NonNegLong]("pel-count")
        pending     <- f[Arr.Of[PendingEntryReply], Seq[GroupPendingFull]]("pending")
        consumers   <- f[Records, Seq[ConsumerInfoFull]]("consumers")
      } yield GroupInfoFull(name, last, entriesRead.flatten, lag.flatten, count, pending, consumers, othersAsText(f, typed))
    }
  }

  private[protocol] final def streamInfoFull[A: Read[Bulk, _]]: Read[Record, StreamInfoFull[A]] = {
    val typed = Set("length", "radix-tree-keys", "radix-tree-nodes", "last-generated-id", "entries", "groups")
    Read.record("stream info") { f =>
      for {
        length  <- f.num[NonNegLong]("length")
        keys    <- f.num[NonNegLong]("radix-tree-keys")
        nodes   <- f.num[NonNegLong]("radix-tree-nodes")
        last    <- f.bulk[Id]("last-generated-id")
        entries <- f[Arr.Of[EntryReply], Seq[Entry[A]]]("entries")
        groups  <- f[Records, Seq[GroupInfoFull]]("groups")
      } yield StreamInfoFull(length, keys, nodes, last, entries, groups, othersAsText(f, typed))
    }
  }

  private implicit final val groupInfo: Read[Record, GroupInfo] = Read.record("group info") { f =>
    for {
      name        <- f.bulk[Key]("name")
      consumers   <- f.num[NonNegLong]("consumers")
      pending     <- f.num[NonNegLong]("pending")
      last        <- f.bulk[Id]("last-delivered-id")
      entriesRead <- f.optional[OrNullBulk[Num], Option[NonNegLong]]("entries-read") // from Redis 7.0, and nil when unknown
      lag         <- f.optional[OrNullBulk[Num], Option[NonNegLong]]("lag")
    } yield GroupInfo(name, consumers, pending, last, entriesRead.flatten, lag.flatten)
  }
  private[protocol] final val groupInfos: Read[Records, Seq[GroupInfo]] = Read[Records, Seq[GroupInfo]]

  private implicit final val consumerInfo: Read[Record, ConsumerInfo] = Read.record("consumer info") { f =>
    for {
      name     <- f.bulk[Key]("name")
      pending  <- f.num[NonNegLong]("pending")
      idle     <- f.num[NonNegLong]("idle")
      inactive <- f.optionalNum[Long]("inactive")
    } yield ConsumerInfo(name, pending, idle, inactive)
  }
  private[protocol] final val consumerInfos: Read[Records, Seq[ConsumerInfo]] = Read[Records, Seq[ConsumerInfo]]

  private[protocol] final def fieldsParams[A: Show](fields: OneOrMore[(Key, A)]): List[String] =
    fields.value.flatMap { case (k, v) => List(k.value, Show[A].show(v)) }

  // the builders each feature's commands take, and the types of their replies
  trait StreamTypes {
    final type StreamBound          = StreamP.Bound
    final type StreamEntry[A]       = StreamP.Entry[A]
    final type StreamFrom           = StreamP.From
    final type StreamGroupFrom      = StreamP.GroupFrom
    final type StreamMkStream       = StreamP.MkStream
    final type StreamNoAck          = StreamP.NoAck
    final type StreamClaimOptions   = StreamP.ClaimOptions
    final type StreamPending        = StreamP.PendingEntry
    final type StreamPendingSummary = StreamP.PendingSummary
    final type StreamId             = StreamP.Id
    final type StreamNoMkStream     = StreamP.NoMkStream
    final type StreamTrim           = StreamP.Trim
    final val StreamBound          = StreamP.Bound
    final val StreamEntry          = StreamP.Entry
    final val StreamFrom           = StreamP.From
    final val StreamGroupFrom      = StreamP.GroupFrom
    final val StreamMkStream       = StreamP.MkStream
    final val StreamNoAck          = StreamP.NoAck
    final val StreamClaimOptions   = StreamP.ClaimOptions
    final val StreamPending        = StreamP.PendingEntry
    final val StreamPendingSummary = StreamP.PendingSummary
    final val StreamId             = StreamP.Id
    final val StreamNoMkStream     = StreamP.NoMkStream
    final val StreamTrim           = StreamP.Trim
  }
  trait StreamIdAtTypes {
    final type StreamIdAt = StreamP.IdAt
    final val StreamIdAt: StreamP.IdAtBuilders = StreamP.IdAt
  }
  trait StreamRefPolicyTypes {
    final type StreamRefPolicy    = StreamP.RefPolicy
    final type StreamDeleteResult = StreamP.DeleteResult
    final val StreamRefPolicy: StreamP.RefPolicyBuilders = StreamP.RefPolicy
    final val StreamDeleteResult                         = StreamP.DeleteResult
  }
  trait StreamClaimTypes {
    final type StreamClaimedEntry[A] = StreamP.ClaimedEntry[A]
    final val StreamClaimedEntry = StreamP.ClaimedEntry
  }
  trait StreamIdmpTypes {
    final type StreamIdmp       = StreamP.Idmp
    final type StreamIdmpConfig = StreamP.IdmpConfig
    final val StreamIdmp: StreamP.IdmpBuilders             = StreamP.Idmp
    final val StreamIdmpConfig: StreamP.IdmpConfigBuilders = StreamP.IdmpConfig
  }
  trait StreamNackTypes {
    final type StreamNackMode    = StreamP.NackMode
    final type StreamNackOptions = StreamP.NackOptions
    final val StreamNackMode: StreamP.NackModeBuilders       = StreamP.NackMode
    final val StreamNackOptions: StreamP.NackOptionsBuilders = StreamP.NackOptions
  }
  trait StreamReadLimitsTypes {
    final type StreamReadLimits = StreamP.ReadLimits
    final val StreamReadLimits: StreamP.ReadLimitsBuilders = StreamP.ReadLimits
  }

  // the same names for laserdisc's own code, which can't read the members a version overrides while it's being built
  private[protocol] object internal
      extends StreamTypes
      with StreamIdAtTypes
      with StreamRefPolicyTypes
      with StreamClaimTypes
      with StreamIdmpTypes
      with StreamNackTypes
      with StreamReadLimitsTypes
}

trait StreamBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "XADD: * = LIMIT MAXLEN MINID NOMKSTREAM ~",
      "XDEL",
      "XLEN",
      "XRANGE: COUNT",
      "XREAD: COUNT STREAMS",
      "XREVRANGE: COUNT",
      "XTRIM: = LIMIT MAXLEN MINID ~"
    )

  import StreamP.{Bound, Entry, From, Id, NoMkStream, Trim, fieldsParams}

  /** The builders of this family's commands, and the types of their replies */
  val streamtypes: StreamP.StreamTypes = new StreamP.StreamTypes {}

  private final def entries[A: Read[Bulk, _]] = RESPRead.instance(StreamP.entries[A])

  /** Adds an entry with an id the server picks, and replies with it */
  final def xadd[A: Show](key: Key, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key.value :: "*" :: fieldsParams(fields)).as[Bulk, Id]
  final def xadd[A: Show](key: Key, id: Id, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key *: id *: fieldsParams(fields) *: EmptyTuple).as[Bulk, Id]
  final def xadd[A: Show](key: Key, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key.value :: trim.params ++ ("*" :: fieldsParams(fields))).as[Bulk, Id]
  final def xadd[A: Show](key: Key, id: Id, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key *: trim.params *: id *: fieldsParams(fields) *: EmptyTuple).as[Bulk, Id]

  /** XADD only to an existing stream: `None` when it doesn't exist */
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: "*" *: fieldsParams(fields) *: EmptyTuple).as[OrNullBulk[Bulk], Option[Id]]
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, id: Id, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: id *: fieldsParams(fields) *: EmptyTuple).as[OrNullBulk[Bulk], Option[Id]]
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: trim.params *: "*" *: fieldsParams(fields) *: EmptyTuple).as[OrNullBulk[Bulk], Option[Id]]
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, id: Id, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: trim.params *: id *: fieldsParams(fields) *: EmptyTuple).as[OrNullBulk[Bulk], Option[Id]]

  /** Deletes the entries, and replies how many it deleted */
  final def xdel(key: Key, ids: OneOrMore[Id]): Single.Aux[NonNegLong] =
    Protocol("XDEL", key *: ids.value *: EmptyTuple).as[Num, NonNegLong]
  final def xdel(key: Key, id: Id, ids: Id*): Single.Aux[NonNegLong] =
    xdel(key, OneOrMore.unsafeFrom(id :: ids.toList))

  final def xlen(key: Key): Single.Aux[NonNegLong] = Protocol("XLEN", key).as[Num, NonNegLong]

  final def xrange[A: Read[Bulk, _]](key: Key, start: Bound, end: Bound): Single.Aux[Seq[Entry[A]]] =
    Protocol("XRANGE", key.value :: start.param :: end.param :: Nil).using(entries[A])
  final def xrange[A: Read[Bulk, _]](key: Key, start: Bound, end: Bound, count: PosLong): Single.Aux[Seq[Entry[A]]] =
    Protocol("XRANGE", key.value :: start.param :: end.param :: "COUNT" :: Show[PosLong].show(count) :: Nil).using(entries[A])

  /** XRANGE in pages of `count` entries, as a [[Loop]]: each page starts after the last entry of the one before, until a page comes short
    */
  final def xrangeAll[A: Read[Bulk, _]](key: Key, count: PosLong): Loop[Entry[A]] = xrangeAll[A](key, Bound.min, Bound.max, count)
  final def xrangeAll[A: Read[Bulk, _]](key: Key, start: Bound, end: Bound, count: PosLong): Loop[Entry[A]] =
    Loop(start)(xrange[A](key, _, end, count)) { (_, page) =>
      page -> (if (page.size < count.value) None else Some(Bound.exclusive(page.last.id)))
    }

  /** The streams' entries after the given ids, by stream, or `None` when none of them has any */
  final def xread[A: Read[Bulk, _]](streams: OneOrMore[(Key, From)]): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREAD", "STREAMS" :: streams.value.map(_._1.value) ++ streams.value.map(_._2.param))
      .using(StreamP.maybeStreams[A])
  final def xread[A: Read[Bulk, _]](streams: OneOrMore[(Key, From)], count: PosLong): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREAD", "COUNT" :: Show[PosLong].show(count) :: "STREAMS" :: streams.value.map(_._1.value) ++ streams.value.map(_._2.param))
      .using(StreamP.maybeStreams[A])

  /** XRANGE from the end: `end` is the larger bound */
  final def xrevrange[A: Read[Bulk, _]](key: Key, end: Bound, start: Bound): Single.Aux[Seq[Entry[A]]] =
    Protocol("XREVRANGE", key.value :: end.param :: start.param :: Nil).using(entries[A])
  final def xrevrange[A: Read[Bulk, _]](key: Key, end: Bound, start: Bound, count: PosLong): Single.Aux[Seq[Entry[A]]] =
    Protocol("XREVRANGE", key.value :: end.param :: start.param :: "COUNT" :: Show[PosLong].show(count) :: Nil).using(entries[A])

  /** XREVRANGE in pages of `count` entries, as a [[Loop]]: each page starts before the last entry of the one before, until a page comes
    * short
    */
  final def xrevrangeAll[A: Read[Bulk, _]](key: Key, count: PosLong): Loop[Entry[A]] = xrevrangeAll[A](key, Bound.max, Bound.min, count)
  final def xrevrangeAll[A: Read[Bulk, _]](key: Key, end: Bound, start: Bound, count: PosLong): Loop[Entry[A]] =
    Loop(end)(xrevrange[A](key, _, start, count)) { (_, page) =>
      page -> (if (page.size < count.value) None else Some(Bound.exclusive(page.last.id)))
    }

  /** Trims the stream, and replies how many entries it deleted */
  final def xtrim(key: Key, trim: Trim): Single.Aux[NonNegLong] = Protocol("XTRIM", key.value :: trim.params).as[Num, NonNegLong]
}

/** Consumer groups: XGROUP, XREADGROUP, XACK and XPENDING. Redis 6.2 */
trait StreamGroupBaseP extends Features {
  import StreamP.{Bound, Entry, From, GroupFrom, Id, MkStream, NoAck, PendingEntry, PendingSummary}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "XACK",
      "XGROUP CREATE: $ MKSTREAM",
      "XGROUP CREATECONSUMER",
      "XGROUP DELCONSUMER",
      "XGROUP DESTROY",
      "XGROUP HELP",
      "XGROUP SETID: $",
      "XPENDING: IDLE",
      "XREADGROUP: COUNT GROUP NOACK STREAMS"
    )

  final val xgrouphelp: Single.Aux[Seq[String]] = Protocol.help("XGROUP")

  /** Acknowledges the entries, and replies how many were pending */
  final def xack(key: Key, group: Key, ids: OneOrMore[Id]): Single.Aux[NonNegLong] =
    Protocol("XACK", key *: group *: ids.value *: EmptyTuple).as[Num, NonNegLong]
  final def xack(key: Key, group: Key, id: Id, ids: Id*): Single.Aux[NonNegLong] =
    xack(key, group, OneOrMore.unsafeFrom(id :: ids.toList))

  /** Creates a group that will read the stream after `from` (`StreamFrom.latest` for entries added from now on) */
  final def xgroupcreate(key: Key, group: Key, from: From): Single.Aux[OK] =
    Protocol("XGROUP", "CREATE" :: key.value :: group.value :: from.param :: Nil).as[Str, OK]
  final def xgroupcreate(key: Key, group: Key, from: From, mkStream: MkStream): Single.Aux[OK] =
    Protocol("XGROUP", "CREATE" *: key *: group *: from.param *: mkStream *: EmptyTuple).as[Str, OK]

  /** Creates a consumer in the group, and replies whether it didn't exist yet */
  final def xgroupcreateconsumer(key: Key, group: Key, consumer: Key): Single.Aux[Boolean] =
    Protocol("XGROUP", "CREATECONSUMER" :: key.value :: group.value :: consumer.value :: Nil).as[Num, Boolean]

  /** Deletes a consumer from the group, and replies how many entries it had pending */
  final def xgroupdelconsumer(key: Key, group: Key, consumer: Key): Single.Aux[NonNegLong] =
    Protocol("XGROUP", "DELCONSUMER" :: key.value :: group.value :: consumer.value :: Nil).as[Num, NonNegLong]

  /** Deletes the group, and replies whether it existed */
  final def xgroupdestroy(key: Key, group: Key): Single.Aux[Boolean] =
    Protocol("XGROUP", "DESTROY" :: key.value :: group.value :: Nil).as[Num, Boolean]

  /** Makes the group read the stream after `from` from now on */
  final def xgroupsetid(key: Key, group: Key, from: From): Single.Aux[OK] =
    Protocol("XGROUP", "SETID" :: key.value :: group.value :: from.param :: Nil).as[Str, OK]

  /** How many entries are pending in the group, and for which consumers */
  final def xpending(key: Key, group: Key): Single.Aux[PendingSummary] =
    Protocol("XPENDING", key.value :: group.value :: Nil).using(RESPRead.instance(StreamP.pendingSummary))

  /** The pending entries from `start` to `end`, at most `count`, for one consumer or all of them, and only those idle for at least
    * `idleMilliseconds`
    */
  final def xpending(key: Key, group: Key, start: Bound, end: Bound, count: PosLong): Single.Aux[Seq[PendingEntry]] =
    Protocol("XPENDING", key.value :: group.value :: start.param :: end.param :: Show[PosLong].show(count) :: Nil)
      .using(RESPRead.instance(StreamP.pendingEntries))
  final def xpending(key: Key, group: Key, start: Bound, end: Bound, count: PosLong, consumer: Key): Single.Aux[Seq[PendingEntry]] =
    Protocol("XPENDING", key.value :: group.value :: start.param :: end.param :: Show[PosLong].show(count) :: consumer.value :: Nil)
      .using(RESPRead.instance(StreamP.pendingEntries))
  final def xpending(
      key: Key,
      group: Key,
      idleMilliseconds: NonNegLong,
      start: Bound,
      end: Bound,
      count: PosLong
  ): Single.Aux[Seq[PendingEntry]] =
    Protocol(
      "XPENDING",
      key.value :: group.value :: "IDLE" :: Show[NonNegLong]
        .show(idleMilliseconds) :: start.param :: end.param :: Show[PosLong].show(count) :: Nil
    )
      .using(RESPRead.instance(StreamP.pendingEntries))
  final def xpending(
      key: Key,
      group: Key,
      idleMilliseconds: NonNegLong,
      start: Bound,
      end: Bound,
      count: PosLong,
      consumer: Key
  ): Single.Aux[Seq[PendingEntry]] =
    Protocol(
      "XPENDING",
      key.value :: group.value :: "IDLE" :: Show[NonNegLong]
        .show(idleMilliseconds) :: start.param :: end.param :: Show[PosLong].show(count) :: consumer.value :: Nil
    ).using(RESPRead.instance(StreamP.pendingEntries))

  private final def groupStreams(streams: OneOrMore[(Key, GroupFrom)]): List[String] =
    "STREAMS" :: streams.value.map(_._1.value) ++ streams.value.map(_._2.param)

  /** Reads the streams for a consumer of the group, by stream, or `None` when none of them has entries for it */
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)]
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREADGROUP", "GROUP" :: group.value :: consumer.value :: groupStreams(streams))
      .using(StreamP.maybeStreams[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      count: PosLong
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREADGROUP", "GROUP" :: group.value :: consumer.value :: "COUNT" :: Show[PosLong].show(count) :: groupStreams(streams))
      .using(StreamP.maybeStreams[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREADGROUP", "GROUP" :: group.value :: consumer.value :: Show[NoAck].show(noAck) :: groupStreams(streams))
      .using(StreamP.maybeStreams[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      count: PosLong,
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      "GROUP" :: group.value :: consumer.value :: "COUNT" :: Show[PosLong].show(count) :: Show[NoAck].show(noAck) :: groupStreams(streams)
    ).using(StreamP.maybeStreams[A])
}

/** XCLAIM, XAUTOCLAIM, XINFO and XSETID: Redis 6.2 */
trait StreamClaimAndInfoBaseP extends Features {
  // what XCLAIM and XAUTOCLAIM reply with on Redis 6.2, which also takes what later versions reply with, so that a server can be upgraded
  // before the import moves on
  protected def claimedEntries[A: Read[Bulk, _]]: RESPRead[Seq[StreamP.Entry[A]]] = RESPRead.instance(StreamP.claimedOrNil[A])
  protected def autoClaimedEntries[A: Read[Bulk, _]]: RESPRead[StreamP.AutoClaimed[StreamP.Entry[A]]] =
    RESPRead.instance(StreamP.autoClaimed(StreamP.claimedOrNil[A]))
  protected def autoClaimedIds: RESPRead[StreamP.AutoClaimed[StreamP.Id]] = RESPRead.instance(StreamP.autoClaimed(StreamP.ids))

  import StreamP.{AutoClaimed, ClaimOptions, ConsumerInfo, Entry, GroupInfo, Id, StreamInfo, StreamInfoFull}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "XAUTOCLAIM: COUNT JUSTID",
      "XCLAIM: FORCE IDLE JUSTID LASTID RETRYCOUNT TIME",
      "XINFO CONSUMERS",
      "XINFO GROUPS",
      "XINFO HELP",
      "XINFO STREAM: COUNT FULL",
      "XSETID"
    )

  final val xinfohelp: Single.Aux[Seq[String]] = Protocol.help("XINFO")

  private final def claim(key: Key, group: Key, consumer: Key, minIdleMilliseconds: NonNegLong, ids: OneOrMore[Id]): List[String] =
    key.value :: group.value :: consumer.value :: Show[NonNegLong].show(minIdleMilliseconds) :: ids.value.map(Show[Id].show)

  /** Gives the consumer the pending entries that have been idle for at least `minIdleMilliseconds`, and replies with them */
  final def xclaim[A: Read[Bulk, _]](
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      ids: OneOrMore[Id]
  ): Single.Aux[Seq[Entry[A]]] =
    Protocol("XCLAIM", claim(key, group, consumer, minIdleMilliseconds, ids)).using(claimedEntries[A])
  final def xclaim[A: Read[Bulk, _]](
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      id: Id,
      ids: Id*
  ): Single.Aux[Seq[Entry[A]]] =
    xclaim[A](key, group, consumer, minIdleMilliseconds, OneOrMore.unsafeFrom(id :: ids.toList))
  final def xclaim[A: Read[Bulk, _]](
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      ids: OneOrMore[Id],
      options: ClaimOptions
  ): Single.Aux[Seq[Entry[A]]] =
    Protocol("XCLAIM", claim(key, group, consumer, minIdleMilliseconds, ids) ++ options.params).using(claimedEntries[A])

  /** XCLAIM that replies with the ids only */
  final def xclaimjustid(key: Key, group: Key, consumer: Key, minIdleMilliseconds: NonNegLong, ids: OneOrMore[Id]): Single.Aux[Seq[Id]] =
    Protocol("XCLAIM", claim(key, group, consumer, minIdleMilliseconds, ids) :+ "JUSTID").using(RESPRead.instance(StreamP.ids))
  final def xclaimjustid(key: Key, group: Key, consumer: Key, minIdleMilliseconds: NonNegLong, id: Id, ids: Id*): Single.Aux[Seq[Id]] =
    xclaimjustid(key, group, consumer, minIdleMilliseconds, OneOrMore.unsafeFrom(id :: ids.toList))
  final def xclaimjustid(
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      ids: OneOrMore[Id],
      options: ClaimOptions
  ): Single.Aux[Seq[Id]] =
    Protocol("XCLAIM", (claim(key, group, consumer, minIdleMilliseconds, ids) ++ options.params) :+ "JUSTID")
      .using(RESPRead.instance(StreamP.ids))

  private final def autoclaim(key: Key, group: Key, consumer: Key, minIdleMilliseconds: NonNegLong, start: Id): List[String] =
    List(key.value, group.value, consumer.value, Show[NonNegLong].show(minIdleMilliseconds), Show[Id].show(start))

  /** Like XCLAIM for the pending entries from `start` on, 100 at a time or `count`, and replies where to start the next call */
  final def xautoclaim[A: Read[Bulk, _]](
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      start: Id
  ): Single.Aux[AutoClaimed[Entry[A]]] =
    Protocol("XAUTOCLAIM", autoclaim(key, group, consumer, minIdleMilliseconds, start))
      .using(autoClaimedEntries[A])
  final def xautoclaim[A: Read[Bulk, _]](
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      start: Id,
      count: PosLong
  ): Single.Aux[AutoClaimed[Entry[A]]] =
    Protocol("XAUTOCLAIM", autoclaim(key, group, consumer, minIdleMilliseconds, start) ++ List("COUNT", Show[PosLong].show(count)))
      .using(autoClaimedEntries[A])

  /** XAUTOCLAIM that replies with the ids only */
  final def xautoclaimjustid(key: Key, group: Key, consumer: Key, minIdleMilliseconds: NonNegLong, start: Id): Single.Aux[AutoClaimed[Id]] =
    Protocol("XAUTOCLAIM", autoclaim(key, group, consumer, minIdleMilliseconds, start) :+ "JUSTID")
      .using(autoClaimedIds)
  final def xautoclaimjustid(
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      start: Id,
      count: PosLong
  ): Single.Aux[AutoClaimed[Id]] =
    Protocol(
      "XAUTOCLAIM",
      autoclaim(key, group, consumer, minIdleMilliseconds, start) ++ List("COUNT", Show[PosLong].show(count), "JUSTID")
    )
      .using(autoClaimedIds)

  final def xinfoconsumers(key: Key, group: Key): Single.Aux[Seq[ConsumerInfo]] =
    Protocol("XINFO", "CONSUMERS" :: key.value :: group.value :: Nil).using(RESPRead.instance(StreamP.consumerInfos))

  final def xinfogroups(key: Key): Single.Aux[Seq[GroupInfo]] =
    Protocol("XINFO", "GROUPS" :: key.value :: Nil).using(RESPRead.instance(StreamP.groupInfos))

  final def xinfostream[A: Read[Bulk, _]](key: Key): Single.Aux[StreamInfo[A]] =
    Protocol("XINFO", "STREAM" :: key.value :: Nil).using(RESPRead.instance(StreamP.streamInfo[A]))

  /** The stream with its entries and groups in full: the first 10 entries and pending entries of each group and consumer, or `count` of
    * them (0 for all of them)
    */
  final def xinfostreamfull[A: Read[Bulk, _]](key: Key): Single.Aux[StreamInfoFull[A]] =
    Protocol("XINFO", "STREAM" :: key.value :: "FULL" :: Nil).using(RESPRead.instance(StreamP.streamInfoFull[A]))
  final def xinfostreamfull[A: Read[Bulk, _]](key: Key, count: NonNegLong): Single.Aux[StreamInfoFull[A]] =
    Protocol("XINFO", "STREAM" :: key.value :: "FULL" :: "COUNT" :: Show[NonNegLong].show(count) :: Nil)
      .using(RESPRead.instance(StreamP.streamInfoFull[A]))

  /** Sets the stream's last id, which new entries' ids must be after */
  final def xsetid(key: Key, lastId: Id): Single.Aux[OK] = Protocol("XSETID", key *: lastId *: EmptyTuple).as[Str, OK]
}

/** XSETID with ENTRIESADDED and MAXDELETEDID: Redis 7.0 */
trait StreamSetIdEntriesP extends Features {
  import StreamP.Id

  override private[laserdisc] def supported: List[String] = super.supported ++ List("XSETID: ENTRIESADDED MAXDELETEDID")

  /** Like `xsetid`, also setting how many entries were ever added and the largest id deleted */
  final def xsetid(key: Key, lastId: Id, entriesAdded: NonNegLong, maxDeletedId: Id): Single.Aux[OK] =
    Protocol(
      "XSETID",
      key.value :: Show[Id]
        .show(lastId) :: "ENTRIESADDED" :: Show[NonNegLong].show(entriesAdded) :: "MAXDELETEDID" :: Show[Id].show(maxDeletedId) :: Nil
    )
      .as[Str, OK]
}

/** XADD with an id whose sequence the server picks: Redis 7.0 */
trait StreamIdAtP extends Features {
  import StreamP.{Id, IdAt, NoMkStream, Trim, fieldsParams}

  final def xadd[A: Show](key: Key, id: IdAt, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key *: id *: fieldsParams(fields) *: EmptyTuple).as[Bulk, Id]
  final def xadd[A: Show](key: Key, id: IdAt, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key *: trim.params *: id *: fieldsParams(fields) *: EmptyTuple).as[Bulk, Id]
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, id: IdAt, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: id *: fieldsParams(fields) *: EmptyTuple).as[OrNullBulk[Bulk], Option[Id]]
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, id: IdAt, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: trim.params *: id *: fieldsParams(fields) *: EmptyTuple).as[OrNullBulk[Bulk], Option[Id]]
}

/** KEEPREF, DELREF and ACKED for XADD and XTRIM, and XDELEX and XACKDEL: Redis 8.2 */
trait StreamRefPolicyP extends Features {
  import StreamP.{DeleteResult, Id, NoMkStream, RefPolicy, Trim, fieldsParams}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "XACKDEL: ACKED DELREF IDS KEEPREF",
      "XADD: ACKED DELREF KEEPREF",
      "XDELEX: ACKED DELREF IDS KEEPREF",
      "XTRIM: ACKED DELREF KEEPREF"
    )

  private final val results = RESPRead[Arr.Of[Num], Seq[DeleteResult]]

  private final def idsParams(ids: OneOrMore[Id]): List[String] = "IDS" :: Show[Int].show(ids.value.length) :: ids.value.map(Show[Id].show)

  /** XADD trimming with a policy for the groups' references */
  final def xadd[A: Show](key: Key, trim: Trim, policy: RefPolicy, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key *: policy *: trim.params *: "*" *: fieldsParams(fields) *: EmptyTuple).as[Bulk, Id]
  final def xadd[A: Show](key: Key, id: Id, trim: Trim, policy: RefPolicy, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key *: policy *: trim.params *: id *: fieldsParams(fields) *: EmptyTuple).as[Bulk, Id]
  final def xadd[A: Show](
      key: Key,
      noMkStream: NoMkStream,
      trim: Trim,
      policy: RefPolicy,
      fields: OneOrMore[(Key, A)]
  ): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: policy *: trim.params *: "*" *: fieldsParams(fields) *: EmptyTuple)
      .as[OrNullBulk[Bulk], Option[Id]]
  final def xadd[A: Show](
      key: Key,
      noMkStream: NoMkStream,
      id: Id,
      trim: Trim,
      policy: RefPolicy,
      fields: OneOrMore[(Key, A)]
  ): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: policy *: trim.params *: id *: fieldsParams(fields) *: EmptyTuple)
      .as[OrNullBulk[Bulk], Option[Id]]
  final def xadd[A: Show](key: Key, id: StreamP.IdAt, trim: Trim, policy: RefPolicy, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key *: policy *: trim.params *: id *: fieldsParams(fields) *: EmptyTuple).as[Bulk, Id]
  final def xadd[A: Show](
      key: Key,
      noMkStream: NoMkStream,
      id: StreamP.IdAt,
      trim: Trim,
      policy: RefPolicy,
      fields: OneOrMore[(Key, A)]
  ): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: policy *: trim.params *: id *: fieldsParams(fields) *: EmptyTuple)
      .as[OrNullBulk[Bulk], Option[Id]]

  /** Acknowledges the entries for the group and deletes them, with what it did to each */
  final def xackdel(key: Key, group: Key, ids: OneOrMore[Id]): Single.Aux[Seq[DeleteResult]] =
    Protocol("XACKDEL", key.value :: group.value :: idsParams(ids)).using(results)
  final def xackdel(key: Key, group: Key, id: Id, ids: Id*): Single.Aux[Seq[DeleteResult]] =
    xackdel(key, group, OneOrMore.unsafeFrom(id :: ids.toList))
  final def xackdel(key: Key, group: Key, ids: OneOrMore[Id], policy: RefPolicy): Single.Aux[Seq[DeleteResult]] =
    Protocol("XACKDEL", key.value :: group.value :: Show[RefPolicy].show(policy) :: idsParams(ids)).using(results)

  /** Deletes the entries, with what it did to each */
  final def xdelex(key: Key, ids: OneOrMore[Id]): Single.Aux[Seq[DeleteResult]] =
    Protocol("XDELEX", key.value :: idsParams(ids)).using(results)
  final def xdelex(key: Key, id: Id, ids: Id*): Single.Aux[Seq[DeleteResult]] =
    xdelex(key, OneOrMore.unsafeFrom(id :: ids.toList))
  final def xdelex(key: Key, ids: OneOrMore[Id], policy: RefPolicy): Single.Aux[Seq[DeleteResult]] =
    Protocol("XDELEX", key.value :: Show[RefPolicy].show(policy) :: idsParams(ids)).using(results)

  final def xtrim(key: Key, trim: Trim, policy: RefPolicy): Single.Aux[NonNegLong] =
    Protocol("XTRIM", key.value :: trim.params ++ List(Show[RefPolicy].show(policy))).as[Num, NonNegLong]
}

/** IDMP and IDMPAUTO for XADD, and XCFGSET: Redis 8.6 */
trait StreamIdmpP extends Features {
  import StreamP.{Id, Idmp, IdmpConfig, NoMkStream, RefPolicy, Trim, fieldsParams}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("XADD: IDMP IDMPAUTO", "XCFGSET: IDMP-DURATION IDMP-MAXSIZE")

  /** XADD that adds the entry only once for the producer and idempotency id, and replies with that entry's id */
  final def xadd[A: Show](key: Key, idmp: Idmp, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key.value :: idmp.params ++ ("*" :: fieldsParams(fields))).as[Bulk, Id]
  final def xadd[A: Show](key: Key, idmp: Idmp, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key.value :: idmp.params ++ trim.params ++ ("*" :: fieldsParams(fields))).as[Bulk, Id]
  final def xadd[A: Show](key: Key, idmp: Idmp, trim: Trim, policy: RefPolicy, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key *: policy *: idmp.params *: trim.params *: "*" *: fieldsParams(fields) *: EmptyTuple).as[Bulk, Id]

  /** Only to an existing stream: `None` when it doesn't exist */
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, idmp: Idmp, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: idmp.params *: "*" *: fieldsParams(fields) *: EmptyTuple).as[OrNullBulk[Bulk], Option[Id]]
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, idmp: Idmp, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: idmp.params *: trim.params *: "*" *: fieldsParams(fields) *: EmptyTuple)
      .as[OrNullBulk[Bulk], Option[Id]]
  final def xadd[A: Show](
      key: Key,
      noMkStream: NoMkStream,
      idmp: Idmp,
      trim: Trim,
      policy: RefPolicy,
      fields: OneOrMore[(Key, A)]
  ): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: policy *: idmp.params *: trim.params *: "*" *: fieldsParams(fields) *: EmptyTuple)
      .as[OrNullBulk[Bulk], Option[Id]]

  final def xcfgset(key: Key, config: IdmpConfig): Single.Aux[OK] = Protocol("XCFGSET", key.value :: config.params).as[Str, OK]
}

/** XNACK: Redis 8.8 */
trait StreamNackP extends Features {
  import StreamP.{Id, NackMode, NackOptions}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("XNACK: FAIL FATAL FORCE IDS RETRYCOUNT SILENT")

  private final def params(key: Key, group: Key, mode: NackMode, ids: OneOrMore[Id]): List[String] =
    key.value :: group.value :: Show[NackMode].show(mode) :: "IDS" :: Show[Int].show(ids.value.length) :: ids.value.map(Show[Id].show)

  /** Gives the entries back to the group for another consumer to read, with how many it gave back */
  final def xnack(key: Key, group: Key, mode: NackMode, ids: OneOrMore[Id]): Single.Aux[NonNegLong] =
    Protocol("XNACK", params(key, group, mode, ids)).as[Num, NonNegLong]
  final def xnack(key: Key, group: Key, mode: NackMode, id: Id, ids: Id*): Single.Aux[NonNegLong] =
    xnack(key, group, mode, OneOrMore.unsafeFrom(id :: ids.toList))
  final def xnack(key: Key, group: Key, mode: NackMode, ids: OneOrMore[Id], options: NackOptions): Single.Aux[NonNegLong] =
    Protocol("XNACK", params(key, group, mode, ids) ++ options.params).as[Num, NonNegLong]
}

/** XREADGROUP with CLAIM: Redis 8.4 */
trait StreamReadGroupClaimP extends Features {
  import StreamP.{ClaimedEntry, NoAck, claimParams, maybeClaimed}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("XREADGROUP: CLAIM")

  private final def countParams(count: PosLong): List[String] = List("COUNT", Show[PosLong].show(count))

  /** Reads new entries for a consumer of the group, by stream, or `None` when there are none, first taking over other consumers' entries
    * that have been pending for at least `minIdleMilliseconds`
    */
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong
  ): Single.Aux[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol("XREADGROUP", claimParams(group, consumer, minIdleMilliseconds, keys, Nil, Nil)).using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      count: PosLong
  ): Single.Aux[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol("XREADGROUP", claimParams(group, consumer, minIdleMilliseconds, keys, countParams(count), Nil)).using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol("XREADGROUP", claimParams(group, consumer, minIdleMilliseconds, keys, Nil, List(Show[NoAck].show(noAck))))
      .using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      count: PosLong,
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol("XREADGROUP", claimParams(group, consumer, minIdleMilliseconds, keys, countParams(count), List(Show[NoAck].show(noAck))))
      .using(maybeClaimed[A])
}

/** XREAD and XREADGROUP with MAXCOUNT and MAXSIZE: Redis 8.10 */
trait StreamReadLimitsP extends Features {
  import StreamP.{ClaimedEntry, Entry, From, GroupFrom, NoAck, ReadLimits, claimParams, maybeClaimed, maybeStreams, streamsParams}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("XREAD: MAXCOUNT MAXSIZE", "XREADGROUP: MAXCOUNT MAXSIZE")

  private final def countParams(count: PosLong): List[String] = List("COUNT", Show[PosLong].show(count))

  /** The streams' entries after the given ids, by stream, up to the limits for the whole read, or `None` when none of them has any */
  final def xread[A: Read[Bulk, _]](streams: OneOrMore[(Key, From)], limits: ReadLimits): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREAD", limits.params ++ streamsParams(streams)(_.param)).using(maybeStreams[A])
  final def xread[A: Read[Bulk, _]](
      streams: OneOrMore[(Key, From)],
      count: PosLong,
      limits: ReadLimits
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREAD", countParams(count) ++ limits.params ++ streamsParams(streams)(_.param)).using(maybeStreams[A])

  private final def group(group: Key, consumer: Key): List[String] = List("GROUP", group.value, consumer.value)

  /** Reads the streams for a consumer of the group, by stream, up to the limits for the whole read, or `None` when none of them has entries
    * for it
    */
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      limits: ReadLimits
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREADGROUP", this.group(group, consumer) ++ limits.params ++ streamsParams(streams)(_.param)).using(maybeStreams[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      count: PosLong,
      limits: ReadLimits
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREADGROUP", this.group(group, consumer) ++ countParams(count) ++ limits.params ++ streamsParams(streams)(_.param))
      .using(maybeStreams[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      limits: ReadLimits,
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREADGROUP", this.group(group, consumer) ++ limits.params ++ (Show[NoAck].show(noAck) :: streamsParams(streams)(_.param)))
      .using(maybeStreams[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      count: PosLong,
      limits: ReadLimits,
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ countParams(count) ++ limits.params ++ (Show[NoAck].show(noAck) :: streamsParams(streams)(_.param))
    ).using(maybeStreams[A])

  /** XREADGROUP with CLAIM, up to the limits for the whole read */
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      limits: ReadLimits
  ): Single.Aux[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol("XREADGROUP", claimParams(group, consumer, minIdleMilliseconds, keys, limits.params, Nil)).using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      count: PosLong,
      limits: ReadLimits
  ): Single.Aux[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol("XREADGROUP", claimParams(group, consumer, minIdleMilliseconds, keys, countParams(count) ++ limits.params, Nil))
      .using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      limits: ReadLimits,
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol("XREADGROUP", claimParams(group, consumer, minIdleMilliseconds, keys, limits.params, List(Show[NoAck].show(noAck))))
      .using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      count: PosLong,
      limits: ReadLimits,
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      claimParams(group, consumer, minIdleMilliseconds, keys, countParams(count) ++ limits.params, List(Show[NoAck].show(noAck)))
    ).using(maybeClaimed[A])
}

/** XGROUP CREATE and XGROUP SETID with ENTRIESREAD: Redis 7.0 */
trait StreamGroupEntriesReadP extends Features {
  import StreamP.{From, MkStream}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("XGROUP CREATE: ENTRIESREAD", "XGROUP SETID: ENTRIESREAD")

  /** Like `xgroupcreate`, telling the group how many entries it has already read, so that it can report its lag */
  final def xgroupcreate(key: Key, group: Key, from: From, entriesRead: NonNegLong): Single.Aux[OK] =
    Protocol("XGROUP", "CREATE" :: key.value :: group.value :: from.param :: "ENTRIESREAD" :: Show[NonNegLong].show(entriesRead) :: Nil)
      .as[Str, OK]
  final def xgroupcreate(key: Key, group: Key, from: From, mkStream: MkStream, entriesRead: NonNegLong): Single.Aux[OK] =
    Protocol("XGROUP", "CREATE" *: key *: group *: from.param *: mkStream *: "ENTRIESREAD" *: entriesRead *: EmptyTuple).as[Str, OK]

  final def xgroupsetid(key: Key, group: Key, from: From, entriesRead: NonNegLong): Single.Aux[OK] =
    Protocol("XGROUP", "SETID" :: key.value :: group.value :: from.param :: "ENTRIESREAD" :: Show[NonNegLong].show(entriesRead) :: Nil)
      .as[Str, OK]
}

/** XCLAIM and XAUTOCLAIM as they reply from Redis 7.0: deleted entries are left out, and XAUTOCLAIM also lists their ids */
trait StreamClaimDeletedP extends StreamClaimAndInfoBaseP {
  override protected def claimedEntries[A: Read[Bulk, _]]: RESPRead[Seq[StreamP.Entry[A]]] = RESPRead.instance(StreamP.entries[A])
  override protected def autoClaimedEntries[A: Read[Bulk, _]]: RESPRead[StreamP.AutoClaimed[StreamP.Entry[A]]] =
    RESPRead.instance(StreamP.autoClaimedWithDeleted(StreamP.entries[A]))
  override protected def autoClaimedIds: RESPRead[StreamP.AutoClaimed[StreamP.Id]] =
    RESPRead.instance(StreamP.autoClaimedWithDeleted(StreamP.ids))
}

trait StreamP
    extends StreamBaseP
    with StreamGroupBaseP
    with StreamClaimAndInfoBaseP
    with StreamClaimDeletedP
    with StreamGroupEntriesReadP
    with StreamSetIdEntriesP
    with StreamIdAtP
    with StreamRefPolicyP
    with StreamReadGroupClaimP
    with StreamIdmpP
    with StreamNackP
    with StreamReadLimitsP {
  override val streamtypes: StreamP.StreamTypes
    with StreamP.StreamIdAtTypes
    with StreamP.StreamRefPolicyTypes
    with StreamP.StreamClaimTypes
    with StreamP.StreamIdmpTypes
    with StreamP.StreamNackTypes
    with StreamP.StreamReadLimitsTypes = new StreamP.StreamTypes
    with StreamP.StreamIdAtTypes
    with StreamP.StreamRefPolicyTypes
    with StreamP.StreamClaimTypes
    with StreamP.StreamIdmpTypes
    with StreamP.StreamNackTypes
    with StreamP.StreamReadLimitsTypes {}
}
