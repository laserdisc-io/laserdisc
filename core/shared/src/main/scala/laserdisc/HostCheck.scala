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

/** What a [[Host]] can be: an IPv4 address, an IPv6 address with or without a zone id, or a hostname (RFC 1123) with or without a dot at
  * the end. The same check on Scala 2 and Scala 3
  */
private[laserdisc] object HostCheck {
  private final val HostnameMaxLength = 253
  private final val LabelMaxLength    = 63

  final def isValid(s: String): Boolean = isIPv4(s) || isIPv6(s) || isHostname(s)

  private def isDigit(c: Char): Boolean        = c >= '0' && c <= '9'
  private def isHexDigit(c: Char): Boolean     = isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  private def isAlphaNumeric(c: Char): Boolean = isDigit(c) || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

  // four numbers from 0 to 255, without leading zeros, which some read as octal
  private def isIPv4(s: String): Boolean = {
    val parts = s.split("\\.", -1)
    parts.length == 4 && parts.forall { p =>
      p.nonEmpty && p.length <= 3 && p.forall(isDigit) && (p.length == 1 || p.charAt(0) != '0') && p.toInt <= 255
    }
  }

  // eight groups, or fewer with one ::, the last two of which can be an IPv4 address, then maybe % and a zone id
  private def isIPv6(s: String): Boolean = {
    val zone = s.indexOf('%')
    if (zone < 0) isIPv6Address(s) else isIPv6Address(s.substring(0, zone)) && isZoneId(s.substring(zone + 1))
  }

  private def isIPv6Address(s: String): Boolean =
    s.split("::", -1) match {
      case Array(all)           => groups(all, ipv4AtTheEnd = true).contains(8)
      case Array(before, after) =>
        val b = if (before.isEmpty) Some(0) else groups(before, ipv4AtTheEnd = false)
        val a = if (after.isEmpty) Some(0) else groups(after, ipv4AtTheEnd = true)
        b.exists(b => a.exists(b + _ <= 7))
      case _ => false
    }

  // how many groups there are, an IPv4 address at the end counting as two
  private def groups(s: String, ipv4AtTheEnd: Boolean): Option[Int] = {
    val parts = s.split(":", -1)
    if (!parts.init.forall(isGroup)) None
    else if (isGroup(parts.last)) Some(parts.length)
    else if (ipv4AtTheEnd && isIPv4(parts.last)) Some(parts.length + 1)
    else None
  }

  private def isGroup(s: String): Boolean = s.nonEmpty && s.length <= 4 && s.forall(isHexDigit)

  // the characters RFC 6874 allows in a zone id without escaping them
  private def isZoneId(s: String): Boolean =
    s.nonEmpty && s.forall(c => isAlphaNumeric(c) || c == '-' || c == '.' || c == '_' || c == '~')

  // a last label of only digits is left out, as it's read as an address, as with 01234 or 256.1.1.1
  private def isHostname(s: String): Boolean = {
    val name   = if (s.endsWith(".")) s.substring(0, s.length - 1) else s
    val labels = name.split("\\.", -1)
    name.length <= HostnameMaxLength && labels.forall(isLabel) && !labels.last.forall(isDigit)
  }

  private def isLabel(s: String): Boolean =
    s.nonEmpty && s.length <= LabelMaxLength && s.forall(c => isAlphaNumeric(c) || c == '-') && s.charAt(0) != '-' &&
      s.charAt(s.length - 1) != '-'
}
