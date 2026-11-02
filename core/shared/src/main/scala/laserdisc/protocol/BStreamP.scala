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

/** XREAD and XREADGROUP that wait, up to `blockMilliseconds` (0 for as long as it takes), for entries to read. While they wait, the
  * connection is held: commands sent after them on the same client get their replies only once they return, and the send's timeout must be
  * longer than the wait
  */
trait BStreamBaseP extends Features {
  import StreamP.{Entry, From, GroupFrom, NoAck}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("XREAD: BLOCK COUNT STREAMS", "XREADGROUP: BLOCK COUNT GROUP NOACK STREAMS")

  private final def streamsParams[K](streams: OneOrMore[(Key, K)])(param: K => String): List[String] =
    "STREAMS" :: streams.value.map(_._1.value) ++ streams.value.map(s => param(s._2))

  private final def entries[A: Read[Bulk, _]] = RESPRead.nullableArr(StreamP.streams[A])

  /** The streams' entries after the given ids, by stream, or `None` when none came within the wait */
  final def xread[A: Read[Bulk, _]](
      streams: OneOrMore[(Key, From)],
      blockMilliseconds: NonNegLong
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREAD", "BLOCK" :: blockMilliseconds.value.toString :: streamsParams(streams)(_.param)).using(entries[A])
  final def xread[A: Read[Bulk, _]](
      streams: OneOrMore[(Key, From)],
      blockMilliseconds: NonNegLong,
      count: PosLong
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREAD", "COUNT" :: count.value.toString :: "BLOCK" :: blockMilliseconds.value.toString :: streamsParams(streams)(_.param))
      .using(entries[A])

  private final def group(group: Key, consumer: Key): List[String] = List("GROUP", group.value, consumer.value)

  /** Reads the streams for a consumer of the group, by stream, or `None` when none had entries for it within the wait */
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: NonNegLong
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREADGROUP", this.group(group, consumer) ++ ("BLOCK" :: blockMilliseconds.value.toString :: streamsParams(streams)(_.param)))
      .using(entries[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: NonNegLong,
      count: PosLong
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ ("COUNT" :: count.value.toString :: "BLOCK" :: blockMilliseconds.value.toString :: streamsParams(
        streams
      )(_.param))
    ).using(entries[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: NonNegLong,
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ ("BLOCK" :: blockMilliseconds.value.toString :: Show[NoAck].show(noAck) :: streamsParams(streams)(
        _.param
      ))
    ).using(entries[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: NonNegLong,
      count: PosLong,
      noAck: NoAck
  ): Single.Aux[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ ("COUNT" :: count.value.toString :: "BLOCK" :: blockMilliseconds.value.toString :: Show[NoAck].show(
        noAck
      ) ::
        streamsParams(streams)(_.param))
    ).using(entries[A])
}

trait BStreamP extends BStreamBaseP
