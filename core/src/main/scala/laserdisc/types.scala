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

final case class ControlChar()
object ControlChar {
  import eu.timepit.refined.api.Validate

  implicit final val controlCharValidate: Validate.Plain[Char, ControlChar] =
    Validate.fromPredicate(_.isControl, t => s"isControl('$t')", ControlChar())
}

final case class KV[A](key: Key, value: A)
final case class ScanKV(cursor: NonNegLong, maybeValues: Option[Seq[KV[String]]])
final case class Scan[A](cursor: NonNegLong, values: Option[Seq[A]])
final case class Time(timestamp: NonNegLong, elapsedMicroseconds: NonNegLong)

sealed trait Direction
object Direction {
  final object asc  extends Direction
  final object desc extends Direction

  implicit val directionShow: Show[Direction] = Show.instance {
    case `asc`  => "ASC"
    case `desc` => "DESC"
  }
}
