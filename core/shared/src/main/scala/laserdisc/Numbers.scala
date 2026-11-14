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

  /** A double as text, the same on every platform: the fewest significant digits that read back as it, written as an integer if it's whole,
    * in plain decimals if its point is within 21 places, and with an exponent otherwise. Infinities are `+inf` and `-inf`, which every
    * server reads, and NaN is `nan`. Each platform's `toString` doesn't do: they don't all give the fewest digits, nor lay them out the
    * same way
    */
  final def show(d: Double): String =
    if (d.isNaN) "nan"
    else if (d == Double.PositiveInfinity) "+inf"
    else if (d == Double.NegativeInfinity) "-inf"
    else if (d == 0d) "0"
    else if (d == j.Math.rint(d) && j.Math.abs(d) < 1e15) d.toLong.toString
    else {
      val (digits, point) = shortest(j.Math.abs(d))
      val body            =
        if (point >= digits.length && point <= 21) digits + "0" * (point - digits.length)
        else if (point > 0 && point <= 21) digits.take(point) + "." + digits.drop(point)
        else if (point <= 0 && point > -6) "0." + "0" * -point + digits
        else digits.take(1) + (if (digits.length > 1) "." + digits.drop(1) else "") + "e" + (point - 1).toString
      (if (d < 0) "-" else "") + body
    }

  // the fewest significant digits that read back as d, the nearest to it of those, and where the point goes among them. toString's are
  // read back as d, and are the fewest but on Scala Native at times and for the JVM's smallest doubles. A decimal with a digit less reads
  // back as d only if one of the two around toString's does, as those that do are all the ones close enough to d: only then are they
  // looked for one by one
  private def shortest(d: Double): (String, Int) = {
    val (digits, point)                = fromToString(d)
    def readsBack(ds: String, at: Int) = j.Double.parseDouble(s"0.${ds}e$at") == d
    if (digits.length == 1) digits -> point
    else {
      // at most 16 digits, so a Long holds them
      val below   = digits.take(digits.length - 1)
      val above   = (j.Long.parseLong(below) + 1L).toString
      val aboveAt = if (above.length > below.length) point + 1 else point
      if (!readsBack(below, point) && !readsBack(above, aboveAt)) digits -> point else exactly(d)
    }
  }

  // the digits of toString, without the zeros around them, and where the point goes among them
  private def fromToString(d: Double): (String, Int) = {
    val text              = d.toString
    val (mantissa, power) = text.indexWhere(c => c == 'e' || c == 'E') match {
      case -1 => text          -> 0
      case at => text.take(at) -> j.Integer.parseInt(text.drop(at + 1).dropWhile(_ == '+'))
    }
    val (whole, fraction) = mantissa.indexOf('.') match {
      case -1 => mantissa          -> ""
      case at => mantissa.take(at) -> mantissa.drop(at + 1)
    }
    val all  = whole + fraction
    val lead = all.takeWhile(_ == '0').length
    all.drop(lead).reverse.dropWhile(_ == '0').reverse -> (whole.length - lead + power)
  }

  // the fewest digits that read back as d, rounding its exact value to 1 digit, then 2, and so on: 17 always do
  private def exactly(d: Double): (String, Int) = {
    val exact                   = new java.math.BigDecimal(d)
    def rounded(precision: Int) = exact.round(new java.math.MathContext(precision, java.math.RoundingMode.HALF_EVEN))
    val found                   = (1 to 17).iterator.map(rounded).find(r => j.Double.parseDouble(r.toString) == d).getOrElse(rounded(17))
    val bare                    = found.stripTrailingZeros
    val digits                  = bare.unscaledValue.toString
    digits -> (digits.length - bare.scale)
  }
}
