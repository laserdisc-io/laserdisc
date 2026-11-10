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

trait BListBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "BLMOVE: LEFT RIGHT",
      "BLPOP",
      "BRPOP",
      "BRPOPLPUSH"
    )

  final def blmove[A: Read[Bulk, _]](
      source: Key,
      destination: Key,
      from: ListP.Side,
      to: ListP.Side,
      seconds: BlockingSeconds
  ): Single[Option[A]] =
    Protocol("BLMOVE", source *: destination *: from *: to *: seconds *: EmptyTuple).as[OrNullBulk[Bulk], Option[A]]

  final def blpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, seconds: BlockingSeconds): Single[Option[KV[A]]] =
    Protocol("BLPOP", keys.value *: seconds *: EmptyTuple).as[OrNilArr[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple]], Option[KV[A]]]

  final def brpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, seconds: BlockingSeconds): Single[Option[KV[A]]] =
    Protocol("BRPOP", keys.value *: seconds *: EmptyTuple).as[OrNilArr[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple]], Option[KV[A]]]

  final def brpoplpush[A: Read[Bulk, _]](source: Key, destination: Key): Single[Option[A]] =
    Protocol("BRPOPLPUSH", source *: destination *: 0 *: EmptyTuple).as[OrNullBulk[Bulk], Option[A]]
  final def brpoplpush[A: Read[Bulk, _]](source: Key, destination: Key, seconds: BlockingSeconds): Single[Option[A]] =
    Protocol("BRPOPLPUSH", source *: destination *: seconds *: EmptyTuple).as[OrNullBulk[Bulk], Option[A]]
}

/** BLMPOP: Redis 7.0 */
trait BListMPopP extends Features {
  import ListP.{Side, keyedSeq}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("BLMPOP: COUNT LEFT RIGHT")

  final def blmpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, side: Side, seconds: BlockingSeconds): Single[Option[KV[Seq[A]]]] =
    Protocol("BLMPOP", seconds *: keys.value.length *: keys.value *: side *: EmptyTuple).using(keyedSeq[A])
  final def blmpop[A: Read[Bulk, _]](
      keys: OneOrMoreKeys,
      side: Side,
      count: PosInt,
      seconds: BlockingSeconds
  ): Single[Option[KV[Seq[A]]]] =
    Protocol("BLMPOP", seconds *: keys.value.length *: keys.value *: side *: "COUNT" *: count *: EmptyTuple).using(keyedSeq[A])
}

/** BLMOVEM: Redis 8.10 */
trait BListMoveManyP extends Features {
  import ListP.{Amount, Order, Side}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("BLMOVEM: BULK COUNT EXACTLY LEFT OBO RIGHT")

  /** Moves one element like `blmove`, replying with a list */
  final def blmovem[A: Read[Bulk, _]](
      source: Key,
      destination: Key,
      from: Side,
      to: Side,
      seconds: BlockingSeconds
  ): Single[Option[Seq[A]]] =
    Protocol("BLMOVEM", source *: destination *: from *: to *: seconds *: EmptyTuple).as[OrNilArr[Arr.Of[Bulk]], Option[Seq[A]]]
  final def blmovem[A: Read[Bulk, _]](
      source: Key,
      destination: Key,
      from: Side,
      to: Side,
      amount: Amount,
      order: Order,
      seconds: BlockingSeconds
  ): Single[Option[Seq[A]]] =
    Protocol("BLMOVEM", source *: destination *: from *: to *: seconds *: amount.params *: order *: EmptyTuple)
      .as[OrNilArr[Arr.Of[Bulk]], Option[Seq[A]]]
}

trait BListP extends BListBaseP with BListExtP with BListMPopP with BListMoveManyP
