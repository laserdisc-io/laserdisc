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

// The blocking commands that wait for as long as it takes, under blocking.forever. Each is the command with a wait, built the same way and
// read the same way, whose wait is then set to 0: the wait given here only stands in for that

trait BListForeverBaseP {
  import ListP.Side

  private final val timed = new BListBaseP {}
  private final val any   = BlockingSeconds.unsafeFrom(1.0d)

  final def blmove[A: Read[Bulk, _]](source: Key, destination: Key, from: Side, to: Side): Forever[Option[A]] =
    Forever(timed.blmove[A](source, destination, from, to, any))
  final def blpop[A: Read[Bulk, _]](keys: OneOrMoreKeys): Forever[Option[KV[A]]]            = Forever(timed.blpop[A](keys, any))
  final def brpop[A: Read[Bulk, _]](keys: OneOrMoreKeys): Forever[Option[KV[A]]]            = Forever(timed.brpop[A](keys, any))
  final def brpoplpush[A: Read[Bulk, _]](source: Key, destination: Key): Forever[Option[A]] =
    Forever(timed.brpoplpush[A](source, destination, any))
}

/** BLMPOP: Redis 7.0 */
trait BListMPopForeverP {
  import ListP.Side

  private final val timed = new BListMPopP {}
  private final val any   = BlockingSeconds.unsafeFrom(1.0d)

  final def blmpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, side: Side): Forever[Option[KV[Seq[A]]]] =
    Forever(timed.blmpop[A](keys, side, any))
  final def blmpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, side: Side, count: PosInt): Forever[Option[KV[Seq[A]]]] =
    Forever(timed.blmpop[A](keys, side, count, any))
}

/** BLMOVEM: Redis 8.10 */
trait BListMoveManyForeverP {
  import ListP.{Amount, Order, Side}

  private final val timed = new BListMoveManyP {}
  private final val any   = BlockingSeconds.unsafeFrom(1.0d)

  /** Moves one element like `blmove`, replying with a list */
  final def blmovem[A: Read[Bulk, _]](source: Key, destination: Key, from: Side, to: Side): Forever[Option[Seq[A]]] =
    Forever(timed.blmovem[A](source, destination, from, to, any))
  final def blmovem[A: Read[Bulk, _]](
      source: Key,
      destination: Key,
      from: Side,
      to: Side,
      amount: Amount,
      order: Order
  ): Forever[Option[Seq[A]]] =
    Forever(timed.blmovem[A](source, destination, from, to, amount, order, any))
}

trait BSortedSetForeverBaseP {
  private final val timed = new BSortedSetBaseP {}
  private final val any   = BlockingSeconds.unsafeFrom(1.0d)

  final def bzpopmax[A: Read[Bulk, _]](keys: OneOrMoreKeys): Forever[Option[KV[(A, Double)]]] = Forever(timed.bzpopmax[A](keys, any))
  final def bzpopmin[A: Read[Bulk, _]](keys: OneOrMoreKeys): Forever[Option[KV[(A, Double)]]] = Forever(timed.bzpopmin[A](keys, any))
}

/** BZMPOP: Redis 7.0 */
trait BSortedSetMPopForeverP {
  import SortedSetP.Pop

  private final val timed = new BSortedSetMPopP {}
  private final val any   = BlockingSeconds.unsafeFrom(1.0d)

  final def bzmpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, pop: Pop): Forever[Option[KV[Seq[(A, Double)]]]] =
    Forever(timed.bzmpop[A](keys, pop, any))
  final def bzmpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, pop: Pop, count: PosInt): Forever[Option[KV[Seq[(A, Double)]]]] =
    Forever(timed.bzmpop[A](keys, pop, count, any))
}

trait BStreamForeverBaseP {
  import StreamP.{Entry, From, GroupFrom, NoAck}

  private final val timed = new BStreamBaseP {}
  private final val any   = PosLong(1L)

  /** The streams' entries after the given ids, by stream */
  final def xread[A: Read[Bulk, _]](streams: OneOrMore[(Key, From)]): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xread[A](streams, any))
  final def xread[A: Read[Bulk, _]](streams: OneOrMore[(Key, From)], count: PosLong): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xread[A](streams, any, count))

  /** Reads the streams for a consumer of the group, by stream */
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)]
  ): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xreadgroup[A](group, consumer, streams, any))
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      count: PosLong
  ): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xreadgroup[A](group, consumer, streams, any, count))
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      noAck: NoAck
  ): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xreadgroup[A](group, consumer, streams, any, noAck))
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      count: PosLong,
      noAck: NoAck
  ): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xreadgroup[A](group, consumer, streams, any, count, noAck))
}

/** XREADGROUP with CLAIM: Redis 8.4 */
trait BStreamReadGroupClaimForeverP {
  import StreamP.{ClaimedEntry, NoAck}

  private final val timed = new BStreamReadGroupClaimP {}
  private final val any   = PosLong(1L)

  /** Reads new entries for a consumer of the group, by stream, first taking over other consumers' entries that have been pending for at
    * least `minIdleMilliseconds`
    */
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong
  ): Forever[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Forever(timed.xreadgroupclaim[A](group, consumer, keys, minIdleMilliseconds, any))
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      count: PosLong
  ): Forever[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Forever(timed.xreadgroupclaim[A](group, consumer, keys, minIdleMilliseconds, any, count))
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      noAck: NoAck
  ): Forever[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Forever(timed.xreadgroupclaim[A](group, consumer, keys, minIdleMilliseconds, any, noAck))
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      count: PosLong,
      noAck: NoAck
  ): Forever[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Forever(timed.xreadgroupclaim[A](group, consumer, keys, minIdleMilliseconds, any, count, noAck))
}

/** XREAD and XREADGROUP with MAXCOUNT and MAXSIZE: Redis 8.10 */
trait BStreamReadLimitsForeverP {
  import StreamP.{ClaimedEntry, Entry, From, GroupFrom, NoAck, ReadLimits}

  private final val timed = new BStreamReadLimitsP {}
  private final val any   = PosLong(1L)

  /** The streams' entries after the given ids, by stream, up to the limits for the whole read */
  final def xread[A: Read[Bulk, _]](streams: OneOrMore[(Key, From)], limits: ReadLimits): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xread[A](streams, any, limits))
  final def xread[A: Read[Bulk, _]](
      streams: OneOrMore[(Key, From)],
      count: PosLong,
      limits: ReadLimits
  ): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xread[A](streams, any, count, limits))

  /** Reads the streams for a consumer of the group, by stream, up to the limits for the whole read */
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      limits: ReadLimits
  ): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xreadgroup[A](group, consumer, streams, any, limits))
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      count: PosLong,
      limits: ReadLimits
  ): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xreadgroup[A](group, consumer, streams, any, count, limits))
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      limits: ReadLimits,
      noAck: NoAck
  ): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xreadgroup[A](group, consumer, streams, any, limits, noAck))
  final def xreadgroup[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      streams: OneOrMore[(Key, GroupFrom)],
      count: PosLong,
      limits: ReadLimits,
      noAck: NoAck
  ): Forever[Option[Seq[KV[Seq[Entry[A]]]]]] =
    Forever(timed.xreadgroup[A](group, consumer, streams, any, count, limits, noAck))

  /** XREADGROUP with CLAIM, up to the limits for the whole read */
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      limits: ReadLimits
  ): Forever[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Forever(timed.xreadgroupclaim[A](group, consumer, keys, minIdleMilliseconds, any, limits))
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      count: PosLong,
      limits: ReadLimits
  ): Forever[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Forever(timed.xreadgroupclaim[A](group, consumer, keys, minIdleMilliseconds, any, count, limits))
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      limits: ReadLimits,
      noAck: NoAck
  ): Forever[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Forever(timed.xreadgroupclaim[A](group, consumer, keys, minIdleMilliseconds, any, limits, noAck))
  final def xreadgroupclaim[A: Read[Bulk, _]](
      group: Key,
      consumer: Key,
      keys: OneOrMoreKeys,
      minIdleMilliseconds: NonNegLong,
      count: PosLong,
      limits: ReadLimits,
      noAck: NoAck
  ): Forever[Option[Seq[KV[Seq[ClaimedEntry[A]]]]]] =
    Forever(timed.xreadgroupclaim[A](group, consumer, keys, minIdleMilliseconds, any, count, limits, noAck))
}

trait KeyWaitForeverP {
  private final val timed = new KeyBaseP {}

  /** WAIT: waits until the writes before it are on `replicas` replicas. Replies with how many it reached. Not named `wait`, as `wait(1)`
    * would call Java's `Object.wait`
    */
  final def waitreplicas(replicas: PosInt): Forever[NonNegInt] = Forever(timed.wait(replicas, PosLong(1L)))
}

/** WAITAOF: Redis 7.2 */
trait KeyWaitAofForeverP {
  private final val timed = new KeyWaitAofP {}

  /** Waits until the writes before it are on disk locally (when `local` is 1) and on `replicas` replicas. Replies with how many of each it
    * reached
    */
  final def waitaof(local: NonNegInt, replicas: NonNegInt): Forever[(NonNegInt, NonNegInt)] =
    Forever(timed.waitaof(local, replicas, PosLong(1L)))
}

/** CLUSTER SETSLOT with TIMEOUT: Valkey 8.0 */
trait ClusterSetSlotForeverP {
  import ClusterP.SetSlotMode

  private final val timed = new ClusterSetSlotTimeoutP {}
  private final val any   = PosLong(1L)

  /** Like `cluster.setslot`, waiting for as long as it takes for the replicas to take the change */
  final def setslot(slot: Slot): Forever[OK]                                  = Forever(timed.setslot(slot, any))
  final def setslot(slot: Slot, mode: SetSlotMode, node: NodeId): Forever[OK] = Forever(timed.setslot(slot, mode, node, any))
}
