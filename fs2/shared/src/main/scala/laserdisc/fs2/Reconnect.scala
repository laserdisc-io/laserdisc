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
package fs2

import scala.concurrent.duration.*

/** How long the client waits before trying the servers again when none could be reached: `first` the first time, then twice as long each
  * time in a row, up to `max`, and `first` again once a server has answered. Each wait is cut by up to half, at random, so that clients
  * started together don't all connect again at the same moment. Commands sent while it waits fail at once with [[ServerUnavailable]]
  */
final case class Reconnect(first: FiniteDuration, max: FiniteDuration) {
  require(first > Duration.Zero, s"first must be more than 0, but is $first")
  require(max >= first, s"max must be at least first, $first, but is $max")

  // the wait after no server could be reached this many times in a row, from 0, before it's cut at random
  private[fs2] def after(times: Int): FiniteDuration = {
    val doubled = first.toNanos.toDouble * math.pow(2d, times.toDouble)
    if (doubled >= max.toNanos.toDouble) max else doubled.toLong.nanos
  }
}

object Reconnect {

  /** 100 milliseconds, doubling up to 5 seconds */
  final val default: Reconnect = Reconnect(100.millis, 5.seconds)
}
