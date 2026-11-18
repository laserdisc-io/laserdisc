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

/** Whether text can be written to the server as it is: as UTF-8, where a lone surrogate, half of a character such as an emoji cut in two,
  * is written as `?`, and so as other text. A [[Key]] is that text, and isn't empty
  */
private[laserdisc] object TextCheck {
  final def isKey(s: String): Boolean = s.nonEmpty && isWellFormed(s)

  final def isWellFormed(s: String): Boolean = {
    var i = 0
    while (i >= 0 && i < s.length) {
      val c = s.charAt(i)
      i =
        if (Character.isHighSurrogate(c)) { if (i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1))) i + 2 else -1 }
        else if (Character.isLowSurrogate(c)) -1
        else i + 1
    }
    i == s.length
  }
}
