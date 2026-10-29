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

object cluster     extends ClusterP
object connection  extends ConnectionP
object geo         extends GeoP
object hashmaps    extends HashP
object hyperloglog extends HyperLogLogP
object keys        extends KeyP
object lists       extends ListP { object blocking extends BListP }
object publish     extends PublishP
object server      extends ServerP
object sets        extends SetP
object sortedsets  extends SortedSetP
object strings     extends StringP
object transaction extends TransactionP

object all
    extends ClusterP
    with ConnectionP
    with GeoP
    with HashP
    with HyperLogLogP
    with KeyP
    with ListP
    with PublishP
    with ServerP
    with SetP
    with SortedSetP
    with StringP
    with TransactionP {
  object blocking extends BListP
}
