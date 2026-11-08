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

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

/** XREAD and XREADGROUP that wait, up to `blockMilliseconds` (0 for as long as it takes), for entries to read. While they wait, the
  * connection is held: commands sent after them on the same client get their replies only once they return, and the send's timeout must be
  * longer than the wait
  */
trait BStreamBaseP extends Features {
  import StreamP.{Entry, From, GroupFrom, NoAck, streamsParams}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("XREAD: BLOCK COUNT STREAMS", "XREADGROUP: BLOCK COUNT GROUP NOACK STREAMS")

  private final def entries[A: Read[Bulk, _]] = StreamP.maybeStreams[A]

  /** The streams' entries after the given ids, by stream, or `None` when none came within the wait */
  final def xread[A: Read[Bulk, _]](
      streams: OneOrMore[(Key, From)],
      blockMilliseconds: NonNegLong
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREAD", "BLOCK" :: Show[NonNegLong].show(blockMilliseconds) :: streamsParams(streams)(_.param)).using(entries[A])
  final def xread[A: Read[Bulk, _]](
      streams: OneOrMore[(Key, From)],
      blockMilliseconds: NonNegLong,
      count: PosLong
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREAD",
      "COUNT" :: Show[PosLong].show(count) :: "BLOCK" :: Show[NonNegLong].show(blockMilliseconds) :: streamsParams(streams)(_.param)
    )
      .using(entries[A])

  private final def group(group: Key, consumer: Key): List[String] = List("GROUP", group.value, consumer.value)

  /** Reads the streams for a consumer of the group, by stream, or `None` when none had entries for it within the wait */
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: NonNegLong
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ ("BLOCK" :: Show[NonNegLong].show(blockMilliseconds) :: streamsParams(streams)(_.param))
    )
      .using(entries[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: NonNegLong,
      count: PosLong
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ ("COUNT" :: Show[PosLong]
        .show(count) :: "BLOCK" :: Show[NonNegLong].show(blockMilliseconds) :: streamsParams(
        streams
      )(_.param))
    ).using(entries[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: NonNegLong,
      noAck: NoAck
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ ("BLOCK" :: Show[NonNegLong].show(blockMilliseconds) :: Show[NoAck].show(noAck) :: streamsParams(
        streams
      )(
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
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ ("COUNT" :: Show[PosLong]
        .show(count) :: "BLOCK" :: Show[NonNegLong].show(blockMilliseconds) :: Show[NoAck].show(
        noAck
      ) ::
        streamsParams(streams)(_.param))
    ).using(entries[A])

  private final def block(blockMilliseconds: PosLong): NonNegLong = NonNegLong.unsafeFrom(blockMilliseconds.value)

  private final def follow[K, A](blockMilliseconds: PosLong, streams: OneOrMore[(Key, K)])(
      read: OneOrMore[(Key, K)] => Protocol[Option[Seq[KV[Seq[Entry[A]]]]]]
  )(next: (K, Option[Seq[Entry[A]]]) => K): Loop[KV[Entry[A]]] =
    Loop.blocking(FiniteDuration(blockMilliseconds.value, MILLISECONDS))(streams)(read) { (current, reply) =>
      val byKey = reply.getOrElse(Nil).map(kv => kv.key -> kv.value).toMap
      val moved = current.value.map { case (key, from) => key -> next(from, byKey.get(key)) }
      reply.getOrElse(Nil).flatMap(kv => kv.value.map(KV(kv.key, _))) -> Some(OneOrMore.unsafeFrom(moved))
    }

  /** Follows the streams with XREAD, as a [[Loop]] that doesn't end: each read waits up to `blockMilliseconds` and starts after the last
    * entry read from each stream. From `From.latest`, entries added between two reads are missed until the stream's first entry comes
    */
  final def xreadFollow[A: Read[Bulk, _]](streams: OneOrMore[(Key, From)], blockMilliseconds: PosLong): Loop[KV[Entry[A]]] =
    follow(blockMilliseconds, streams)(xread[A](_, block(blockMilliseconds)))(after)
  final def xreadFollow[A: Read[Bulk, _]](streams: OneOrMore[(Key, From)], blockMilliseconds: PosLong, count: PosLong): Loop[KV[Entry[A]]] =
    follow(blockMilliseconds, streams)(xread[A](_, block(blockMilliseconds), count))(after)

  private final def after[A](from: From, read: Option[Seq[Entry[A]]]): From =
    read.flatMap(_.lastOption).fold(from)(entry => From.after(entry.id))

  /** Follows the streams with XREADGROUP for a consumer of the group, as a [[Loop]] that doesn't end: each read waits up to
    * `blockMilliseconds`. A stream read from `GroupFrom.pendingAfter` first gives the consumer's pending entries, then new entries
    */
  final def xreadgroupFollow[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: PosLong
  ): Loop[KV[Entry[A]]] =
    follow(blockMilliseconds, streams)(xreadgroup[A](group, consumer, _, block(blockMilliseconds)))(pendingThenNew)
  final def xreadgroupFollow[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: PosLong,
      count: PosLong
  ): Loop[KV[Entry[A]]] =
    follow(blockMilliseconds, streams)(xreadgroup[A](group, consumer, _, block(blockMilliseconds), count))(pendingThenNew)
  final def xreadgroupFollow[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: PosLong,
      noAck: NoAck
  ): Loop[KV[Entry[A]]] =
    follow(blockMilliseconds, streams)(xreadgroup[A](group, consumer, _, block(blockMilliseconds), noAck))(pendingThenNew)
  final def xreadgroupFollow[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: PosLong,
      count: PosLong,
      noAck: NoAck
  ): Loop[KV[Entry[A]]] =
    follow(blockMilliseconds, streams)(xreadgroup[A](group, consumer, _, block(blockMilliseconds), count, noAck))(pendingThenNew)

  // the pending entries are done when a read of them comes back empty
  private final def pendingThenNew[A](from: GroupFrom, read: Option[Seq[Entry[A]]]): GroupFrom =
    if (from eq GroupFrom.newEntries) from
    else read.flatMap(_.lastOption).fold(GroupFrom.newEntries)(entry => GroupFrom.pendingAfter(entry.id))
}

/** XREADGROUP with CLAIM that waits, up to `blockMilliseconds`: Redis 8.4 */
trait BStreamReadGroupClaimP extends Features {
  import StreamP.{ClaimedEntry, NoAck, claimParams, maybeClaimed}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("XREADGROUP: CLAIM")

  private final def countParams(count: PosLong): List[String]                = List("COUNT", Show[PosLong].show(count))
  private final def blockParams(blockMilliseconds: NonNegLong): List[String] = List("BLOCK", Show[NonNegLong].show(blockMilliseconds))

  /** Reads new entries for a consumer of the group, by stream, or `None` when none came within the wait, first taking over other consumers'
    * entries that have been pending for at least `minIdleMilliseconds`
    */
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      blockMilliseconds: NonNegLong
  ): Single[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol("XREADGROUP", claimParams(group, consumer, minIdleMilliseconds, keys, blockParams(blockMilliseconds), Nil))
      .using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      blockMilliseconds: NonNegLong,
      count: PosLong
  ): Single[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      claimParams(group, consumer, minIdleMilliseconds, keys, countParams(count) ++ blockParams(blockMilliseconds), Nil)
    ).using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      blockMilliseconds: NonNegLong,
      noAck: NoAck
  ): Single[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      claimParams(group, consumer, minIdleMilliseconds, keys, blockParams(blockMilliseconds), List(Show[NoAck].show(noAck)))
    ).using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      blockMilliseconds: NonNegLong,
      count: PosLong,
      noAck: NoAck
  ): Single[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      claimParams(
        group,
        consumer,
        minIdleMilliseconds,
        keys,
        countParams(count) ++ blockParams(blockMilliseconds),
        List(Show[NoAck].show(noAck))
      )
    ).using(maybeClaimed[A])
}

/** XREAD and XREADGROUP with MAXCOUNT and MAXSIZE that wait, up to `blockMilliseconds`: Redis 8.10 */
trait BStreamReadLimitsP extends Features {
  import StreamP.{ClaimedEntry, Entry, From, GroupFrom, NoAck, ReadLimits, claimParams, maybeClaimed, maybeStreams, streamsParams}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("XREAD: MAXCOUNT MAXSIZE", "XREADGROUP: MAXCOUNT MAXSIZE")

  private final def countParams(count: PosLong): List[String]                = List("COUNT", Show[PosLong].show(count))
  private final def blockParams(blockMilliseconds: NonNegLong): List[String] = List("BLOCK", Show[NonNegLong].show(blockMilliseconds))

  /** The streams' entries after the given ids, by stream, up to the limits for the whole read, or `None` when none came within the wait */
  final def xread[A: Read[Bulk, _]](
      streams: OneOrMore[(Key, From)],
      blockMilliseconds: NonNegLong,
      limits: ReadLimits
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREAD", limits.params ++ blockParams(blockMilliseconds) ++ streamsParams(streams)(_.param)).using(maybeStreams[A])
  final def xread[A: Read[Bulk, _]](
      streams: OneOrMore[(Key, From)],
      blockMilliseconds: NonNegLong,
      count: PosLong,
      limits: ReadLimits
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol("XREAD", countParams(count) ++ limits.params ++ blockParams(blockMilliseconds) ++ streamsParams(streams)(_.param))
      .using(maybeStreams[A])

  private final def group(group: Key, consumer: Key): List[String] = List("GROUP", group.value, consumer.value)

  /** Reads the streams for a consumer of the group, by stream, up to the limits for the whole read, or `None` when none had entries for it
    * within the wait
    */
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: NonNegLong,
      limits: ReadLimits
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ limits.params ++ blockParams(blockMilliseconds) ++ streamsParams(streams)(_.param)
    ).using(maybeStreams[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: NonNegLong,
      count: PosLong,
      limits: ReadLimits
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ countParams(count) ++ limits.params ++ blockParams(blockMilliseconds) ++ streamsParams(streams)(
        _.param
      )
    ).using(maybeStreams[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: NonNegLong,
      limits: ReadLimits,
      noAck: NoAck
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ limits.params ++ blockParams(blockMilliseconds) ++
        (Show[NoAck].show(noAck) :: streamsParams(streams)(_.param))
    ).using(maybeStreams[A])
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      blockMilliseconds: NonNegLong,
      count: PosLong,
      limits: ReadLimits,
      noAck: NoAck
  ): Single[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      this.group(group, consumer) ++ countParams(count) ++ limits.params ++ blockParams(blockMilliseconds) ++
        (Show[NoAck].show(noAck) :: streamsParams(streams)(_.param))
    ).using(maybeStreams[A])

  /** XREADGROUP with CLAIM that waits, up to the limits for the whole read */
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      blockMilliseconds: NonNegLong,
      limits: ReadLimits
  ): Single[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol("XREADGROUP", claimParams(group, consumer, minIdleMilliseconds, keys, limits.params ++ blockParams(blockMilliseconds), Nil))
      .using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      blockMilliseconds: NonNegLong,
      count: PosLong,
      limits: ReadLimits
  ): Single[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      claimParams(group, consumer, minIdleMilliseconds, keys, countParams(count) ++ limits.params ++ blockParams(blockMilliseconds), Nil)
    ).using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      blockMilliseconds: NonNegLong,
      limits: ReadLimits,
      noAck: NoAck
  ): Single[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      claimParams(
        group,
        consumer,
        minIdleMilliseconds,
        keys,
        limits.params ++ blockParams(blockMilliseconds),
        List(Show[NoAck].show(noAck))
      )
    ).using(maybeClaimed[A])
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      blockMilliseconds: NonNegLong,
      count: PosLong,
      limits: ReadLimits,
      noAck: NoAck
  ): Single[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Protocol(
      "XREADGROUP",
      claimParams(
        group,
        consumer,
        minIdleMilliseconds,
        keys,
        countParams(count) ++ limits.params ++ blockParams(blockMilliseconds),
        List(Show[NoAck].show(noAck))
      )
    ).using(maybeClaimed[A])
}

trait BStreamP extends BStreamBaseP with BStreamReadGroupClaimP with BStreamReadLimitsP
