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

import java.lang as j

/** Numbers as servers write them, and nothing else: Java's parsers also take a +, spaces around a double, hex doubles and the d and f
  * suffixes, which no server sends, so that a garbled reply would be read as a number instead of failing
  */
private[laserdisc] object Numbers {
  private final val DoublePattern = "-?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?".r.pattern

  // an optional -, then digits
  final def isInteger(s: String): Boolean = {
    val start = if (s.startsWith("-")) 1 else 0
    var i     = start
    while (i < s.length && s.charAt(i) >= '0' && s.charAt(i) <= '9') i += 1
    i == s.length && i > start
  }

  final def toInt(s: String): Option[Int] =
    if (!isInteger(s)) None
    else
      try Some(j.Integer.parseInt(s))
      catch { case _: NumberFormatException => None }

  final def toLong(s: String): Option[Long] =
    if (!isInteger(s)) None
    else
      try Some(j.Long.parseLong(s))
      catch { case _: NumberFormatException => None }

  // as Redis writes doubles, which includes infinite ones as inf and -inf
  final def toDouble(s: String): Option[Double] = s match {
    case "inf" | "+inf"                          => Some(Double.PositiveInfinity)
    case "-inf"                                  => Some(Double.NegativeInfinity)
    case "nan" | "-nan"                          => Some(Double.NaN)
    case _ if DoublePattern.matcher(s).matches() => Some(j.Double.parseDouble(s))
    case _                                       => None
  }
}
