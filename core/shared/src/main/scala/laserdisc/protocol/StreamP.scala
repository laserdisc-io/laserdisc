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

  /** An entry of a stream: its id and its fields, in the order they were added */
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

  /** NOMKSTREAM, for XADD: don't create the stream when it doesn't exist */
  sealed trait NoMkStream
  object NoMkStream extends NoMkStream {
    implicit val noMkStreamShow: Show[NoMkStream] = Show.const("NOMKSTREAM")
  }

  private[protocol] final def entry[A](implicit R: Read[Bulk, A]): Read[Arr, Entry[A]] = {
    val fields = Read.arr2Tuple2Seq[Key, A]
    Read.instancePF("Arr(stream entry)") { case Arr(Id.bulk2IdRead(Right(id)) +: fields(Right(fs)) +: Seq()) => Entry(id, fs) }
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
    final type StreamBound      = StreamP.Bound
    final type StreamEntry[A]   = StreamP.Entry[A]
    final type StreamFrom       = StreamP.From
    final type StreamId         = StreamP.Id
    final type StreamNoMkStream = StreamP.NoMkStream
    final type StreamTrim       = StreamP.Trim

    final val StreamBound      = StreamP.Bound
    final val StreamEntry      = StreamP.Entry
    final val StreamFrom       = StreamP.From
    final val StreamId         = StreamP.Id
    final val StreamNoMkStream = StreamP.NoMkStream
    final val StreamTrim       = StreamP.Trim
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

trait StreamP extends StreamBaseP
