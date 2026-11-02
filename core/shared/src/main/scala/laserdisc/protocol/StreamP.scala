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

trait StreamP extends StreamBaseP with StreamGroupBaseP with StreamGroupEntriesReadP
