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

/** What a [[GlobPattern]] and an [[AclPattern]] can be, checked in one pass over the text, however long, the same on Scala 2 and Scala 3:
  * `*`, `?`, `[abc]`, `[a-z]`, `[^abc]`, `\x` for the character x itself, or any other character but a control character. A class can't be
  * empty, can't start with `^` after the one that negates it, and must be closed; a `]` outside of a class is written `\]`. It can't hold
  * half of a character either (see [[TextCheck]]). An [[AclPattern]] has no spaces
  */
private[laserdisc] object GlobCheck {
  final def isGlobPattern(s: String): Boolean = isValid(s, spaces = true)
  final def isAclPattern(s: String): Boolean  = isValid(s, spaces = false)

  private def isValid(s: String, spaces: Boolean): Boolean = {
    def allowed(c: Char): Boolean = !(c <= '\u001f' || (c >= '\u007f' && c <= '\u009f')) && (spaces || c != ' ')
    // the position after an escape starting at i, or -1 when nothing allowed follows the \
    def escape(i: Int): Int = if (i + 1 < s.length && allowed(s.charAt(i + 1))) i + 2 else -1
    // the position after a class whose [ is at i, or -1 when it's empty, starts with ^ after the negating one, or isn't closed
    def charClass(at: Int): Int = {
      var i = at + 1
      if (i < s.length && s.charAt(i) == '^') i += 1
      if (i >= s.length) -1
      else {
        val first = s.charAt(i)
        i =
          if (first == '\\') escape(i)
          else if (!allowed(first) || first == ']' || first == '^') -1
          else i + 1
        while (i >= 0 && i < s.length && s.charAt(i) != ']') {
          val c = s.charAt(i)
          i = if (!allowed(c)) -1 else if (c == '\\') escape(i) else i + 1
        }
        if (i < 0 || i >= s.length) -1 else i + 1
      }
    }
    var i = 0
    while (i >= 0 && i < s.length) {
      val c = s.charAt(i)
      i =
        if (!allowed(c) || c == ']') -1
        else if (c == '\\') escape(i)
        else if (c == '[') charClass(i)
        else i + 1
    }
    s.nonEmpty && i == s.length && TextCheck.isWellFormed(s)
  }
}
