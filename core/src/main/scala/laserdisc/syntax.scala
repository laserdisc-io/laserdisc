/*
 * Copyright (c) 2018-2025 LaserDisc
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

object cluster     extends protocol.ClusterP
object connection  extends protocol.ConnectionP
object geo         extends protocol.GeoP
object hashmaps    extends protocol.HashP
object hyperloglog extends protocol.HyperLogLogP
object keys        extends protocol.KeyP
object lists       extends protocol.ListP { object blocking extends protocol.BListP }
object publish     extends protocol.PublishP
object server      extends protocol.ServerP
object sets        extends protocol.SetP
object sortedsets  extends protocol.SortedSetP
object strings     extends protocol.StringP
object transaction extends protocol.TransactionP

object all
    extends protocol.ClusterP
    with protocol.ConnectionP
    with protocol.GeoP
    with protocol.HashP
    with protocol.HyperLogLogP
    with protocol.KeyP
    with protocol.ListP
    with protocol.PublishP
    with protocol.ServerP
    with protocol.SetP
    with protocol.SortedSetP
    with protocol.StringP
    with protocol.TransactionP {
  final object blocking extends protocol.BListP
}
