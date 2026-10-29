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

import io.github.iltotore.iron.Constraint

/* Both predicates below are spelled out as character ranges, rather than delegating to `Char.isControl` / `Char.isWhitespace`, so that
 * iron can evaluate them at compile time when refining literals. They match the JDK methods for every Char. */

final case class ControlChar()
object ControlChar {
  given Constraint[Char, ControlChar] with {
    override inline def test(inline c: Char): Boolean = c <= '\u001f' || (c >= '\u007f' && c <= '\u009f')
    override inline def message: String               = "isControl"
  }
}

private[laserdisc] final case class WhitespaceChar()
private[laserdisc] object WhitespaceChar {
  given Constraint[Char, WhitespaceChar] with {
    override inline def test(inline c: Char): Boolean =
      (c >= '\t' && c <= '\r') || (c >= '\u001c' && c <= ' ') || c == '\u1680' || (c >= '\u2000' && c <= '\u2006') ||
        (c >= '\u2008' && c <= '\u200a') || c == '\u2028' || c == '\u2029' || c == '\u205f' || c == '\u3000'
    override inline def message: String = "isWhitespace"
  }
}
