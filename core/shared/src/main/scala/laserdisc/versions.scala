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

import protocol.*

/** What Redis 6.2 has, which every version below builds on */
trait Base6_2
    extends ClusterP
    with ConnectionP
    with GeoBaseP
    with GeoExtP
    with HashP
    with HyperLogLogP
    with KeyBaseP
    with KeyExtP
    with ListBaseP
    with ListExtP
    with PublishP
    with ServerP
    with SetBaseP
    with SetExtP
    with SortedSetBaseP
    with SortedSetExtP
    with StringBaseP
    with StringExtP
    with TransactionP {
  lazy val blocking: Blocking6_2 = new Blocking6_2 {}

  override private[laserdisc] def supported: List[String] = super.supported ++ blocking.supported
}

/** The blocking commands, under `blocking` */
trait Blocking6_2  extends BListBaseP with BListExtP with BSortedSetBaseP
trait Blocking7_0  extends Blocking6_2 with BListMPopP with BSortedSetMPopP
trait Blocking8_10 extends Blocking7_0 with BListMoveManyP

trait Redis6_2 extends Base6_2
trait Redis7_0
    extends Redis6_2
    with StringSetGetFlagP
    with KeyExpireConditionP
    with KeySortReadOnlyP
    with ListMPopP
    with SetInterCardP
    with SortedSetInterCardP
    with SortedSetMPopP {
  override lazy val blocking: Blocking7_0 = new Blocking7_0 {}
}
trait Redis7_2  extends Redis7_0 with KeyWaitAofP with SortedSetRankWithScoreP
trait Redis7_4  extends Redis7_2
trait Redis8_0  extends Redis7_4
trait Redis8_2  extends Redis8_0
trait Redis8_4  extends Redis8_2 with StringSetIfEqP with StringSetCompareP
trait Redis8_6  extends Redis8_4
trait Redis8_8  extends Redis8_6 with SortedSetAggregateCountP
trait Redis8_10 extends Redis8_8 with ListMoveManyP with SetDiffCardP with SetUnionCardP {
  override lazy val blocking: Blocking8_10 = new Blocking8_10 {}
}

/** Valkey started from Redis 7.2 */
trait Valkey7_2 extends Redis7_2
trait Valkey8_0 extends Valkey7_2 with SortedSetScanNoScoresP
trait Valkey8_1 extends Valkey8_0 with StringSetIfEqP
trait Valkey9_0 extends Valkey8_1 with GeoSearchPolygonP
trait Valkey9_1 extends Valkey9_0

object redis_6_2  extends Redis6_2
object redis_7_0  extends Redis7_0
object redis_7_2  extends Redis7_2
object redis_7_4  extends Redis7_4
object redis_8_0  extends Redis8_0
object redis_8_2  extends Redis8_2
object redis_8_4  extends Redis8_4
object redis_8_6  extends Redis8_6
object redis_8_8  extends Redis8_8
object redis_8_10 extends Redis8_10

object valkey_7_2 extends Valkey7_2
object valkey_8_0 extends Valkey8_0
object valkey_8_1 extends Valkey8_1
object valkey_9_0 extends Valkey9_0
object valkey_9_1 extends Valkey9_1
