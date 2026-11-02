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
  final case class Id(milliseconds: NonNegLong, sequence: NonNegLong) {
    override final def toString: String = s"${milliseconds.value}-${sequence.value}"
  }
  object Id {
    private final val IdRegex = raw"(\d+)-(\d+)".r

    implicit final val idShow: Show[Id]            = Show.instance(_.toString)
    implicit final val bulk2IdRead: Read[Bulk, Id] = Read.instancePF("Bulk(stream id)") {
      case Bulk(IdRegex(ToLong(NonNegLong(ms)), ToLong(NonNegLong(seq)))) => Id(ms, seq)
    }
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
    def inclusive(id: Id): Bound = new Of(id.toString)
    def exclusive(id: Id): Bound = new Of(s"(${id.toString}")
  }

  /** Where XREAD reads a stream from: after an id, or only entries added after the read (`$`) */
  sealed trait From { def param: String }
  object From       {
    private final class Of(override final val param: String) extends From

    def after(id: Id): From = new Of(id.toString)
    final val latest: From  = new Of("$")
  }

  /** MAXLEN and MINID, for XADD and XTRIM: keep at most so many entries, or only entries from an id on. The approximate forms (`~`) let the
    * server trim less, which is faster, and LIMIT caps how many entries one trim removes
    */
  sealed trait Trim { def params: List[String] }
  object Trim       {
    private final class Of(override final val params: List[String]) extends Trim

    def maxLen(count: NonNegLong): Trim                       = new Of(List("MAXLEN", count.value.toString))
    def maxLenApprox(count: NonNegLong): Trim                 = new Of(List("MAXLEN", "~", count.value.toString))
    def maxLenApprox(count: NonNegLong, limit: PosLong): Trim =
      new Of(List("MAXLEN", "~", count.value.toString, "LIMIT", limit.value.toString))
    def minId(id: Id): Trim                       = new Of(List("MINID", id.toString))
    def minIdApprox(id: Id): Trim                 = new Of(List("MINID", "~", id.toString))
    def minIdApprox(id: Id, limit: PosLong): Trim = new Of(List("MINID", "~", id.toString, "LIMIT", limit.value.toString))
  }

  /** Where XREADGROUP reads a stream from: entries never delivered to the group yet (`>`), or the consumer's pending entries after an id */
  sealed trait GroupFrom { def param: String }
  object GroupFrom       {
    private final class Of(override final val param: String) extends GroupFrom

    final val newEntries: GroupFrom     = new Of(">")
    def pendingAfter(id: Id): GroupFrom = new Of(id.toString)
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

  /** A pending entry of XPENDING: its id, the consumer it was delivered to, how long ago in milliseconds, and how many times */
  final case class PendingEntry(id: Id, consumer: Key, idleMilliseconds: NonNegLong, deliveries: NonNegLong)

  private[protocol] final val pendingSummary: Read[Arr, PendingSummary] = {
    val count                                           = Read.num2NonNegLongRead
    implicit val consumer: Read[Arr, (Key, NonNegLong)] = Read.arr2Tuple2Read[Key, NonNegLong]
    val consumers                                       = Read.arrOfArr2Seq[(Key, NonNegLong)]
    Read.instancePF("Arr(pending summary)") {
      case Arr(count(Right(n)) +: NullBulk +: NullBulk +: NilArr +: Seq()) => PendingSummary(n, None, None, Nil)
      case Arr(count(Right(n)) +: Id.bulk2IdRead(Right(smallest)) +: Id.bulk2IdRead(Right(largest)) +: consumers(Right(cs)) +: Seq()) =>
        PendingSummary(n, Some(smallest), Some(largest), cs)
    }
  }

  private[protocol] final val pendingEntries: Read[Arr, Seq[PendingEntry]] = {
    val number                                = Read.num2NonNegLongRead
    implicit val one: Read[Arr, PendingEntry] = Read.instancePF("Arr(pending entry)") {
      case Arr(Id.bulk2IdRead(Right(id)) +: Bulk(Key(consumer)) +: number(Right(idle)) +: number(Right(deliveries)) +: Seq()) =>
        PendingEntry(id, consumer, idle, deliveries)
    }
    Read[Arr, Seq[PendingEntry]]
  }

  /** IDLE, TIME, RETRYCOUNT, FORCE and LASTID, for XCLAIM: set how long the entries have been idle (or since when), how many times they
    * were delivered, claim them even if they weren't pending, and move the group's last delivered id on
    */
  final class ClaimOptions private (val params: List[String]) {
    def idle(milliseconds: NonNegLong): ClaimOptions          = new ClaimOptions(params ++ List("IDLE", milliseconds.value.toString))
    def time(millisecondsTimestamp: NonNegLong): ClaimOptions =
      new ClaimOptions(params ++ List("TIME", millisecondsTimestamp.value.toString))
    def retryCount(count: NonNegLong): ClaimOptions = new ClaimOptions(params ++ List("RETRYCOUNT", count.value.toString))
    def force: ClaimOptions                         = new ClaimOptions(params :+ "FORCE")
    def lastId(id: Id): ClaimOptions                = new ClaimOptions(params ++ List("LASTID", id.toString))
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
  object RefPolicy {
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
  sealed trait Idmp { def params: List[String] }
  object Idmp       {
    private final class Of(override final val params: List[String]) extends Idmp

    def of(producer: Key, id: Key): Idmp = new Of(List("IDMP", producer.value, id.value))
    def auto(producer: Key): Idmp        = new Of(List("IDMPAUTO", producer.value))
  }

  /** IDMP-DURATION and IDMP-MAXSIZE, for XCFGSET: how long, in seconds, and how many idempotency ids a stream remembers per producer */
  final class IdmpConfig private (val params: List[String]) {
    def duration(seconds: PosLong): IdmpConfig = new IdmpConfig(params ++ List("IDMP-DURATION", seconds.value.toString))
    def maxSize(ids: PosLong): IdmpConfig      = new IdmpConfig(params ++ List("IDMP-MAXSIZE", ids.value.toString))
  }
  object IdmpConfig {
    def duration(seconds: PosLong): IdmpConfig = new IdmpConfig(List("IDMP-DURATION", seconds.value.toString))
    def maxSize(ids: PosLong): IdmpConfig      = new IdmpConfig(List("IDMP-MAXSIZE", ids.value.toString))
  }

  /** NOMKSTREAM, for XADD: don't create the stream when it doesn't exist */
  sealed trait NoMkStream
  object NoMkStream extends NoMkStream {
    implicit val noMkStreamShow: Show[NoMkStream] = Show.const("NOMKSTREAM")
  }

  private[protocol] final def entry[A](implicit R: Read[Bulk, A]): Read[Arr, Entry[A]] = {
    val fields = Read.arr2Tuple2Seq[Key, A]
    Read.instancePF("Arr(stream entry)") {
      case Arr(Id.bulk2IdRead(Right(id)) +: fields(Right(fs)) +: Seq()) => Entry(id, fs)
      case Arr(Id.bulk2IdRead(Right(id)) +: NilArr +: Seq())            => Entry(id, Nil)
    }
  }

  private[protocol] final def entries[A: Read[Bulk, _]]: Read[Arr, Seq[Entry[A]]] = {
    implicit val one: Read[Arr, Entry[A]] = entry[A]
    Read[Arr, Seq[Entry[A]]]
  }

  private[protocol] final def streams[A: Read[Bulk, _]]: Read[Arr, Seq[KV[Seq[Entry[A]]]]] = {
    val es                                         = entries[A]
    implicit val one: Read[Arr, KV[Seq[Entry[A]]]] = Read.instancePF("Arr(stream)") { case Arr(Bulk(Key(k)) +: es(Right(read)) +: Seq()) =>
      KV(k, read)
    }
    Read[Arr, Seq[KV[Seq[Entry[A]]]]]
  }

  // Redis 6.2 replies with nil for each deleted entry it claims; later versions leave them out
  private[protocol] final def claimed[A: Read[Bulk, _]]: Read[Arr, Seq[Entry[A]]] = {
    val one = entry[A]
    Read.instance { case Arr(elements) =>
      elements.foldRight[Either[RESPDecErr, List[Entry[A]]]](Right(Nil)) {
        case (NullBulk | NilArr, Right(es)) => Right(es)
        case (one(Right(e)), Right(es))     => Right(e :: es)
        case (other, Right(_))              => Left(RESPDecErr(s"Unexpected claimed entry. Was $other"))
        case (_, left)                      => left
      }
    }
  }

  private[protocol] final val ids: Read[Arr, Seq[Id]] = Read[Arr, Seq[Id]]

  private[protocol] final def autoClaimed[A](claims: Read[Arr, Seq[A]]): Read[Arr, AutoClaimed[A]] = Read.instancePF("Arr(auto claimed)") {
    case Arr(Id.bulk2IdRead(Right(next)) +: claims(Right(cs)) +: Seq())                        => AutoClaimed(next, cs, Nil)
    case Arr(Id.bulk2IdRead(Right(next)) +: claims(Right(cs)) +: ids(Right(deleted)) +: Seq()) => AutoClaimed(next, cs, deleted)
  }

  private final def byName(fields: Seq[RESP]): Map[String, RESP] = fields.grouped(2).collect { case Seq(Bulk(k), v) => k -> v }.toMap

  private final def number(resp: Option[RESP]): Option[NonNegLong] = resp.collect { case Num(NonNegLong(n)) => n }

  private[protocol] final def streamInfo[A: Read[Bulk, _]]: Read[Arr, StreamInfo[A]] = {
    val one   = entry[A]
    val typed = Set("length", "radix-tree-keys", "radix-tree-nodes", "last-generated-id", "groups", "first-entry", "last-entry")
    def optionalEntry(resp: Option[RESP]): Option[Option[Entry[A]]] = resp match {
      case Some(NilArr | NullBulk) | None => Some(None)
      case Some(one(Right(e)))            => Some(Some(e))
      case _                              => None
    }
    Read.instance { case Arr(fields) =>
      val m = byName(fields)
      (for {
        length <- number(m.get("length"))
        keys   <- number(m.get("radix-tree-keys"))
        nodes  <- number(m.get("radix-tree-nodes"))
        last   <- m.get("last-generated-id").collect { case Id.bulk2IdRead(Right(id)) => id }
        groups <- number(m.get("groups"))
        first  <- optionalEntry(m.get("first-entry"))
        lastE  <- optionalEntry(m.get("last-entry"))
      } yield {
        val others = m.collect {
          case (k, Bulk(v)) if !typed(k) => k -> v
          case (k, Num(v)) if !typed(k)  => k -> v.toString
        }
        StreamInfo(length, keys, nodes, last, groups, first, lastE, others)
      }).toRight(RESPDecErr(s"Unexpected stream info. Was $fields"))
    }
  }

  private[protocol] final val groupInfos: Read[Arr, Seq[GroupInfo]] = Read.instance { case Arr(groups) =>
    groups.foldRight[Either[RESPDecErr, List[GroupInfo]]](Right(Nil)) {
      case (Arr(fields), Right(gs)) =>
        val m = byName(fields)
        (for {
          name      <- m.get("name").collect { case Bulk(Key(k)) => k }
          consumers <- number(m.get("consumers"))
          pending   <- number(m.get("pending"))
          last      <- m.get("last-delivered-id").collect { case Id.bulk2IdRead(Right(id)) => id }
        } yield GroupInfo(name, consumers, pending, last, number(m.get("entries-read")), number(m.get("lag"))))
          .fold[Either[RESPDecErr, List[GroupInfo]]](Left(RESPDecErr(s"Unexpected group info. Was $fields")))(g => Right(g :: gs))
      case (other, Right(_)) => Left(RESPDecErr(s"Unexpected group info. Was $other"))
      case (_, left)         => left
    }
  }

  private[protocol] final val consumerInfos: Read[Arr, Seq[ConsumerInfo]] = Read.instance { case Arr(consumers) =>
    consumers.foldRight[Either[RESPDecErr, List[ConsumerInfo]]](Right(Nil)) {
      case (Arr(fields), Right(cs)) =>
        val m = byName(fields)
        (for {
          name    <- m.get("name").collect { case Bulk(Key(k)) => k }
          pending <- number(m.get("pending"))
          idle    <- number(m.get("idle"))
        } yield ConsumerInfo(name, pending, idle, m.get("inactive").collect { case Num(n) => n }))
          .fold[Either[RESPDecErr, List[ConsumerInfo]]](Left(RESPDecErr(s"Unexpected consumer info. Was $fields")))(c => Right(c :: cs))
      case (other, Right(_)) => Left(RESPDecErr(s"Unexpected consumer info. Was $other"))
      case (_, left)         => left
    }
  }

  private[protocol] final def fieldsParams[A: Show](fields: OneOrMore[(Key, A)]): List[String] =
    fields.value.flatMap { case (k, v) => List(k.value, Show[A].show(v)) }
}

trait StreamBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "XADD: * LIMIT MAXLEN MINID NOMKSTREAM ~",
      "XDEL",
      "XLEN",
      "XRANGE: COUNT",
      "XREAD: COUNT STREAMS",
      "XREVRANGE: COUNT",
      "XTRIM: LIMIT MAXLEN MINID ~"
    )

  import StreamP.{Bound, Entry, From, Id, NoMkStream, Trim, fieldsParams}

  object streamtypes {
    final type StreamBound          = StreamP.Bound
    final type StreamEntry[A]       = StreamP.Entry[A]
    final type StreamFrom           = StreamP.From
    final type StreamGroupFrom      = StreamP.GroupFrom
    final type StreamMkStream       = StreamP.MkStream
    final type StreamNoAck          = StreamP.NoAck
    final type StreamClaimOptions   = StreamP.ClaimOptions
    final type StreamDeleteResult   = StreamP.DeleteResult
    final type StreamIdmp           = StreamP.Idmp
    final type StreamIdmpConfig     = StreamP.IdmpConfig
    final type StreamRefPolicy      = StreamP.RefPolicy
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
    final val StreamDeleteResult   = StreamP.DeleteResult
    final val StreamIdmp           = StreamP.Idmp
    final val StreamIdmpConfig     = StreamP.IdmpConfig
    final val StreamRefPolicy      = StreamP.RefPolicy
    final val StreamPending        = StreamP.PendingEntry
    final val StreamPendingSummary = StreamP.PendingSummary
    final val StreamId             = StreamP.Id
    final val StreamNoMkStream     = StreamP.NoMkStream
    final val StreamTrim           = StreamP.Trim
  }

  private final def entries[A: Read[Bulk, _]] = RESPRead.instance(StreamP.entries[A])

  /** Adds an entry with an id the server picks, and replies with it */
  final def xadd[A: Show](key: Key, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key.value :: "*" :: fieldsParams(fields)).as[Bulk, Id]
  final def xadd[A: Show](key: Key, id: Id, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key.value :: id.toString :: fieldsParams(fields)).as[Bulk, Id]
  final def xadd[A: Show](key: Key, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key.value :: trim.params ++ ("*" :: fieldsParams(fields))).as[Bulk, Id]
  final def xadd[A: Show](key: Key, id: Id, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key.value :: trim.params ++ (id.toString :: fieldsParams(fields))).as[Bulk, Id]

  /** XADD only to an existing stream: `None` when it doesn't exist */
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: "*" *: fieldsParams(fields) *: EmptyTuple).asOpt[Bulk, Id]
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, id: Id, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: id.toString *: fieldsParams(fields) *: EmptyTuple).asOpt[Bulk, Id]
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: trim.params *: "*" *: fieldsParams(fields) *: EmptyTuple).asOpt[Bulk, Id]
  final def xadd[A: Show](key: Key, noMkStream: NoMkStream, id: Id, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: trim.params *: id.toString *: fieldsParams(fields) *: EmptyTuple).asOpt[Bulk, Id]

  /** Deletes the entries, and replies how many it deleted */
  final def xdel(key: Key, ids: OneOrMore[Id]): Single.Aux[NonNegLong] =
    Protocol("XDEL", key.value :: ids.value.map(_.toString)).as[Num, NonNegLong]

  final def xlen(key: Key): Single.Aux[NonNegLong] = Protocol("XLEN", key).as[Num, NonNegLong]

  final def xrange[A: Read[Bulk, _]](key: Key, start: Bound, end: Bound): Single.Aux[Seq[Entry[A]]] =
    Protocol("XRANGE", key.value :: start.param :: end.param :: Nil).using(entries[A])
  final def xrange[A: Read[Bulk, _]](key: Key, start: Bound, end: Bound, count: PosLong): Single.Aux[Seq[Entry[A]]] =
    Protocol("XRANGE", key.value :: start.param :: end.param :: "COUNT" :: count.value.toString :: Nil).using(entries[A])

  /** The streams' entries after the given ids, by stream, or `None` when none of them has any */
  final def xread[A: Read[Bulk, _]](streams: OneOrMore[(Key, From)]): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREAD", "STREAMS" :: streams.value.map(_._1.value) ++ streams.value.map(_._2.param))
      .using(RESPRead.nullableArr(StreamP.streams[A]))
  final def xread[A: Read[Bulk, _]](streams: OneOrMore[(Key, From)], count: PosLong): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREAD", "COUNT" :: count.value.toString :: "STREAMS" :: streams.value.map(_._1.value) ++ streams.value.map(_._2.param))
      .using(RESPRead.nullableArr(StreamP.streams[A]))

  /** XRANGE from the end: `end` is the larger bound */
  final def xrevrange[A: Read[Bulk, _]](key: Key, end: Bound, start: Bound): Single.Aux[Seq[Entry[A]]] =
    Protocol("XREVRANGE", key.value :: end.param :: start.param :: Nil).using(entries[A])
  final def xrevrange[A: Read[Bulk, _]](key: Key, end: Bound, start: Bound, count: PosLong): Single.Aux[Seq[Entry[A]]] =
    Protocol("XREVRANGE", key.value :: end.param :: start.param :: "COUNT" :: count.value.toString :: Nil).using(entries[A])

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
      "XGROUP SETID: $",
      "XPENDING: IDLE",
      "XREADGROUP: COUNT GROUP NOACK STREAMS"
    )

  /** Acknowledges the entries, and replies how many were pending */
  final def xack(key: Key, group: Key, ids: OneOrMore[Id]): Single.Aux[NonNegLong] =
    Protocol("XACK", key.value :: group.value :: ids.value.map(_.toString)).as[Num, NonNegLong]

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
    Protocol("XPENDING", key.value :: group.value :: start.param :: end.param :: count.value.toString :: Nil)
      .using(RESPRead.instance(StreamP.pendingEntries))
  final def xpending(key: Key, group: Key, start: Bound, end: Bound, count: PosLong, consumer: Key): Single.Aux[Seq[PendingEntry]] =
    Protocol("XPENDING", key.value :: group.value :: start.param :: end.param :: count.value.toString :: consumer.value :: Nil)
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
      key.value :: group.value :: "IDLE" :: idleMilliseconds.value.toString :: start.param :: end.param :: count.value.toString :: Nil
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
      key.value :: group.value :: "IDLE" :: idleMilliseconds.value.toString :: start.param :: end.param :: count.value.toString :: consumer.value :: Nil
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
      .using(RESPRead.nullableArr(StreamP.streams[A]))
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      count: PosLong
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREADGROUP", "GROUP" :: group.value :: consumer.value :: "COUNT" :: count.value.toString :: groupStreams(streams))
      .using(RESPRead.nullableArr(StreamP.streams[A]))
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREADGROUP", "GROUP" :: group.value :: consumer.value :: Show[NoAck].show(noAck) :: groupStreams(streams))
      .using(RESPRead.nullableArr(StreamP.streams[A]))
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      count: PosLong,
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      "GROUP" :: group.value :: consumer.value :: "COUNT" :: count.value.toString :: Show[NoAck].show(noAck) :: groupStreams(streams)
    ).using(RESPRead.nullableArr(StreamP.streams[A]))
}

/** XCLAIM, XAUTOCLAIM, XINFO and XSETID: Redis 6.2 */
trait StreamClaimAndInfoBaseP extends Features {
  import StreamP.{AutoClaimed, ClaimOptions, ConsumerInfo, Entry, GroupInfo, Id, StreamInfo}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "XAUTOCLAIM: COUNT JUSTID",
      "XCLAIM: FORCE IDLE JUSTID LASTID RETRYCOUNT TIME",
      "XINFO CONSUMERS",
      "XINFO GROUPS",
      "XINFO STREAM",
      "XSETID"
    )

  private final def claim(key: Key, group: Key, consumer: Key, minIdleMilliseconds: NonNegLong, ids: OneOrMore[Id]): List[String] =
    key.value :: group.value :: consumer.value :: minIdleMilliseconds.value.toString :: ids.value.map(_.toString)

  /** Gives the consumer the pending entries that have been idle for at least `minIdleMilliseconds`, and replies with them */
  final def xclaim[A: Read[Bulk, _]](
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      ids: OneOrMore[Id]
  ): Single.Aux[Seq[Entry[A]]] =
    Protocol("XCLAIM", claim(key, group, consumer, minIdleMilliseconds, ids)).using(RESPRead.instance(StreamP.claimed[A]))
  final def xclaim[A: Read[Bulk, _]](
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      ids: OneOrMore[Id],
      options: ClaimOptions
  ): Single.Aux[Seq[Entry[A]]] =
    Protocol("XCLAIM", claim(key, group, consumer, minIdleMilliseconds, ids) ++ options.params).using(RESPRead.instance(StreamP.claimed[A]))

  /** XCLAIM that replies with the ids only */
  final def xclaimjustid(key: Key, group: Key, consumer: Key, minIdleMilliseconds: NonNegLong, ids: OneOrMore[Id]): Single.Aux[Seq[Id]] =
    Protocol("XCLAIM", claim(key, group, consumer, minIdleMilliseconds, ids) :+ "JUSTID").using(RESPRead.instance(StreamP.ids))
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
    List(key.value, group.value, consumer.value, minIdleMilliseconds.value.toString, start.toString)

  /** Like XCLAIM for the pending entries from `start` on, 100 at a time or `count`, and replies where to start the next call */
  final def xautoclaim[A: Read[Bulk, _]](
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      start: Id
  ): Single.Aux[AutoClaimed[Entry[A]]] =
    Protocol("XAUTOCLAIM", autoclaim(key, group, consumer, minIdleMilliseconds, start))
      .using(RESPRead.instance(StreamP.autoClaimed(StreamP.claimed[A])))
  final def xautoclaim[A: Read[Bulk, _]](
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      start: Id,
      count: PosLong
  ): Single.Aux[AutoClaimed[Entry[A]]] =
    Protocol("XAUTOCLAIM", autoclaim(key, group, consumer, minIdleMilliseconds, start) ++ List("COUNT", count.value.toString))
      .using(RESPRead.instance(StreamP.autoClaimed(StreamP.claimed[A])))

  /** XAUTOCLAIM that replies with the ids only */
  final def xautoclaimjustid(key: Key, group: Key, consumer: Key, minIdleMilliseconds: NonNegLong, start: Id): Single.Aux[AutoClaimed[Id]] =
    Protocol("XAUTOCLAIM", autoclaim(key, group, consumer, minIdleMilliseconds, start) :+ "JUSTID")
      .using(RESPRead.instance(StreamP.autoClaimed(StreamP.ids)))
  final def xautoclaimjustid(
      key: Key,
      group: Key,
      consumer: Key,
      minIdleMilliseconds: NonNegLong,
      start: Id,
      count: PosLong
  ): Single.Aux[AutoClaimed[Id]] =
    Protocol("XAUTOCLAIM", autoclaim(key, group, consumer, minIdleMilliseconds, start) ++ List("COUNT", count.value.toString, "JUSTID"))
      .using(RESPRead.instance(StreamP.autoClaimed(StreamP.ids)))

  final def xinfoconsumers(key: Key, group: Key): Single.Aux[Seq[ConsumerInfo]] =
    Protocol("XINFO", "CONSUMERS" :: key.value :: group.value :: Nil).using(RESPRead.instance(StreamP.consumerInfos))

  final def xinfogroups(key: Key): Single.Aux[Seq[GroupInfo]] =
    Protocol("XINFO", "GROUPS" :: key.value :: Nil).using(RESPRead.instance(StreamP.groupInfos))

  final def xinfostream[A: Read[Bulk, _]](key: Key): Single.Aux[StreamInfo[A]] =
    Protocol("XINFO", "STREAM" :: key.value :: Nil).using(RESPRead.instance(StreamP.streamInfo[A]))

  /** Sets the stream's last id, which new entries' ids must be after */
  final def xsetid(key: Key, lastId: Id): Single.Aux[OK] = Protocol("XSETID", key.value :: lastId.toString :: Nil).as[Str, OK]
}

/** XSETID with ENTRIESADDED and MAXDELETEDID: Redis 7.0 */
trait StreamSetIdEntriesP extends Features {
  import StreamP.Id

  override private[laserdisc] def supported: List[String] = super.supported ++ List("XSETID: ENTRIESADDED MAXDELETEDID")

  /** Like `xsetid`, also setting how many entries were ever added and the largest id deleted */
  final def xsetid(key: Key, lastId: Id, entriesAdded: NonNegLong, maxDeletedId: Id): Single.Aux[OK] =
    Protocol(
      "XSETID",
      key.value :: lastId.toString :: "ENTRIESADDED" :: entriesAdded.value.toString :: "MAXDELETEDID" :: maxDeletedId.toString :: Nil
    )
      .as[Str, OK]
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

  private final val results = RESPRead.instance(Read.arrOfNum2Seq[DeleteResult])

  private final def idsParams(ids: OneOrMore[Id]): List[String] = "IDS" :: ids.value.length.toString :: ids.value.map(_.toString)

  /** XADD trimming with a policy for the groups' references */
  final def xadd[A: Show](key: Key, trim: Trim, policy: RefPolicy, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key *: policy *: trim.params *: "*" *: fieldsParams(fields) *: EmptyTuple).as[Bulk, Id]
  final def xadd[A: Show](key: Key, id: Id, trim: Trim, policy: RefPolicy, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key *: policy *: trim.params *: id.toString *: fieldsParams(fields) *: EmptyTuple).as[Bulk, Id]
  final def xadd[A: Show](
      key: Key,
      noMkStream: NoMkStream,
      trim: Trim,
      policy: RefPolicy,
      fields: OneOrMore[(Key, A)]
  ): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: policy *: trim.params *: "*" *: fieldsParams(fields) *: EmptyTuple).asOpt[Bulk, Id]
  final def xadd[A: Show](
      key: Key,
      noMkStream: NoMkStream,
      id: Id,
      trim: Trim,
      policy: RefPolicy,
      fields: OneOrMore[(Key, A)]
  ): Single.Aux[Option[Id]] =
    Protocol("XADD", key *: noMkStream *: policy *: trim.params *: id.toString *: fieldsParams(fields) *: EmptyTuple).asOpt[Bulk, Id]

  /** Acknowledges the entries for the group and deletes them, with what it did to each */
  final def xackdel(key: Key, group: Key, ids: OneOrMore[Id]): Single.Aux[Seq[DeleteResult]] =
    Protocol("XACKDEL", key.value :: group.value :: idsParams(ids)).using(results)
  final def xackdel(key: Key, group: Key, ids: OneOrMore[Id], policy: RefPolicy): Single.Aux[Seq[DeleteResult]] =
    Protocol("XACKDEL", key.value :: group.value :: Show[RefPolicy].show(policy) :: idsParams(ids)).using(results)

  /** Deletes the entries, with what it did to each */
  final def xdelex(key: Key, ids: OneOrMore[Id]): Single.Aux[Seq[DeleteResult]] =
    Protocol("XDELEX", key.value :: idsParams(ids)).using(results)
  final def xdelex(key: Key, ids: OneOrMore[Id], policy: RefPolicy): Single.Aux[Seq[DeleteResult]] =
    Protocol("XDELEX", key.value :: Show[RefPolicy].show(policy) :: idsParams(ids)).using(results)

  final def xtrim(key: Key, trim: Trim, policy: RefPolicy): Single.Aux[NonNegLong] =
    Protocol("XTRIM", key.value :: trim.params ++ List(Show[RefPolicy].show(policy))).as[Num, NonNegLong]
}

/** IDMP and IDMPAUTO for XADD, and XCFGSET: Redis 8.6 */
trait StreamIdmpP extends Features {
  import StreamP.{Id, Idmp, IdmpConfig, Trim, fieldsParams}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("XADD: IDMP IDMPAUTO", "XCFGSET: IDMP-DURATION IDMP-MAXSIZE")

  /** XADD that adds the entry only once for the producer and idempotency id, and replies with that entry's id */
  final def xadd[A: Show](key: Key, idmp: Idmp, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key.value :: idmp.params ++ ("*" :: fieldsParams(fields))).as[Bulk, Id]
  final def xadd[A: Show](key: Key, idmp: Idmp, trim: Trim, fields: OneOrMore[(Key, A)]): Single.Aux[Id] =
    Protocol("XADD", key.value :: idmp.params ++ trim.params ++ ("*" :: fieldsParams(fields))).as[Bulk, Id]

  final def xcfgset(key: Key, config: IdmpConfig): Single.Aux[OK] = Protocol("XCFGSET", key.value :: config.params).as[Str, OK]
}

/** XGROUP CREATE and XGROUP SETID with ENTRIESREAD: Redis 7.0 */
trait StreamGroupEntriesReadP extends Features {
  import StreamP.{From, MkStream}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("XGROUP CREATE: ENTRIESREAD", "XGROUP SETID: ENTRIESREAD")

  /** Like `xgroupcreate`, telling the group how many entries it has already read, so that it can report its lag */
  final def xgroupcreate(key: Key, group: Key, from: From, entriesRead: NonNegLong): Single.Aux[OK] =
    Protocol("XGROUP", "CREATE" :: key.value :: group.value :: from.param :: "ENTRIESREAD" :: entriesRead.value.toString :: Nil).as[Str, OK]
  final def xgroupcreate(key: Key, group: Key, from: From, mkStream: MkStream, entriesRead: NonNegLong): Single.Aux[OK] =
    Protocol("XGROUP", "CREATE" *: key *: group *: from.param *: mkStream *: "ENTRIESREAD" *: entriesRead *: EmptyTuple).as[Str, OK]

  final def xgroupsetid(key: Key, group: Key, from: From, entriesRead: NonNegLong): Single.Aux[OK] =
    Protocol("XGROUP", "SETID" :: key.value :: group.value :: from.param :: "ENTRIESREAD" :: entriesRead.value.toString :: Nil).as[Str, OK]
}

trait StreamP
    extends StreamBaseP
    with StreamGroupBaseP
    with StreamClaimAndInfoBaseP
    with StreamGroupEntriesReadP
    with StreamSetIdEntriesP
    with StreamRefPolicyP
    with StreamIdmpP
