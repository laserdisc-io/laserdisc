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

trait BSortedSetBaseP extends Features {
  import SortedSetP.keyedScored

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "BZPOPMAX",
      "BZPOPMIN"
    )

  final def bzpopmax[A: Read[Bulk, _]](keys: OneOrMoreKeys, seconds: BlockingSeconds): Single[Option[KV[(A, Double)]]] =
    Protocol("BZPOPMAX", keys.value *: seconds *: EmptyTuple).using(keyedScored[A])

  final def bzpopmin[A: Read[Bulk, _]](keys: OneOrMoreKeys, seconds: BlockingSeconds): Single[Option[KV[(A, Double)]]] =
    Protocol("BZPOPMIN", keys.value *: seconds *: EmptyTuple).using(keyedScored[A])
}

/** BZMPOP: Redis 7.0 */
trait BSortedSetMPopP extends Features {
  import SortedSetP.{Pop, keyedScoredSeq}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("BZMPOP: COUNT MAX MIN")

  final def bzmpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, pop: Pop, seconds: BlockingSeconds): Single[Option[KV[Seq[(A, Double)]]]] =
    Protocol("BZMPOP", seconds *: keys.value.length *: keys.value *: pop *: EmptyTuple).using(keyedScoredSeq[A])
  final def bzmpop[A: Read[Bulk, _]](
      keys: OneOrMoreKeys,
      pop: Pop,
      count: PosInt,
      seconds: BlockingSeconds
  ): Single[Option[KV[Seq[(A, Double)]]]] =
    Protocol("BZMPOP", seconds *: keys.value.length *: keys.value *: pop *: "COUNT" *: count *: EmptyTuple).using(keyedScoredSeq[A])
}

trait BSortedSetP extends BSortedSetBaseP with BSortedSetMPopP
