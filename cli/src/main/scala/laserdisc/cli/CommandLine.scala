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
package cli

import scala.annotation.tailrec

/** Splits a line typed at the prompt into a command and its arguments, following redis-cli's rules: arguments are separated by whitespace,
  * "double quoted" ones support the escapes \\ \" \n \r \t \b \a and \xHH, 'single quoted' ones only \'.
  */
private[cli] object CommandLine {
  final def parse(line: String): Either[String, List[String]] = {
    val error = Left("Invalid argument(s)")

    @tailrec def unquoted(i: Int, acc: StringBuilder, args: List[String]): Either[String, List[String]] =
      if (i == line.length) Right((if (acc.isEmpty) args else acc.result() :: args).reverse)
      else
        line.charAt(i) match {
          case c if c.isWhitespace => unquoted(i + 1, new StringBuilder, if (acc.isEmpty) args else acc.result() :: args)
          case '"' if acc.isEmpty  => quoted(i + 1, '"', acc, args)
          case '\'' if acc.isEmpty => quoted(i + 1, '\'', acc, args)
          case c                   => unquoted(i + 1, acc += c, args)
        }

    @tailrec def quoted(i: Int, quote: Char, acc: StringBuilder, args: List[String]): Either[String, List[String]] =
      if (i == line.length) error
      else
        line.charAt(i) match {
          case `quote` if i + 1 == line.length || line.charAt(i + 1).isWhitespace =>
            unquoted(i + 1, new StringBuilder, acc.result() :: args)
          case `quote`                                      => error
          case '\\' if i + 1 < line.length && quote == '\'' =>
            line.charAt(i + 1) match {
              case '\'' => quoted(i + 2, quote, acc += '\'', args)
              case _    => quoted(i + 1, quote, acc += '\\', args)
            }
          case '\\' if i + 1 < line.length =>
            line.charAt(i + 1) match {
              case 'n' => quoted(i + 2, quote, acc += '\n', args)
              case 'r' => quoted(i + 2, quote, acc += '\r', args)
              case 't' => quoted(i + 2, quote, acc += '\t', args)
              case 'b' => quoted(i + 2, quote, acc += '\b', args)
              case 'a' => quoted(i + 2, quote, acc += '\u0007', args)
              case 'x' if i + 3 < line.length && isHex(line.charAt(i + 2)) && isHex(line.charAt(i + 3)) =>
                quoted(i + 4, quote, acc += Integer.parseInt(line.substring(i + 2, i + 4), 16).toChar, args)
              case c => quoted(i + 2, quote, acc += c, args)
            }
          case c => quoted(i + 1, quote, acc += c, args)
        }

    unquoted(0, new StringBuilder, Nil)
  }

  private[this] final def isHex(c: Char): Boolean = Character.digit(c, 16) >= 0
}
