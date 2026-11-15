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

import scodec.bits.ByteVector

import java.nio.charset.StandardCharsets.UTF_8
import scala.annotation.tailrec

/** Splits a line typed at the prompt into a command and its arguments, following redis-cli's rules: arguments are separated by whitespace,
  * and a quote starts quoting anywhere in one, so `a"b c"` is `ab c`. "Double quoted" text supports the escapes \\ \" \n \r \t \b \a and
  * \xHH, where \xHH is that byte, 'single quoted' text only \'. Each argument is the bytes of its text, in UTF-8, and of its \xHH escapes
  */
private[cli] object CommandLine {
  // an argument's text and the bytes of its escapes, in order: each run of text is encoded once, so characters made of two chars stay whole
  private final class Argument {
    private val bytes = Array.newBuilder[Byte]
    private val text  = new StringBuilder
    private var empty = true

    def isEmpty: Boolean = empty

    def +=(c: Char): Argument = {
      text += c
      empty = false
      this
    }

    def byte(b: Byte): Argument = {
      flush()
      bytes += b
      empty = false
      this
    }

    def result(): ByteVector = {
      flush()
      ByteVector.view(bytes.result())
    }

    private def flush(): Unit =
      if (text.nonEmpty) {
        bytes ++= text.toString.getBytes(UTF_8)
        text.clear()
      }
  }

  final def parse(line: String): Either[String, List[ByteVector]] = {
    val error = Left("Invalid argument(s)")

    @tailrec def unquoted(i: Int, acc: Argument, args: List[ByteVector]): Either[String, List[ByteVector]] =
      if (i == line.length) Right((if (acc.isEmpty) args else acc.result() :: args).reverse)
      else
        line.charAt(i) match {
          case c if ends(c) || (acc.isEmpty && isSpace(c)) => unquoted(i + 1, new Argument, if (acc.isEmpty) args else acc.result() :: args)
          case '"'                                         => quoted(i + 1, '"', acc, args)
          case '\''                                        => quoted(i + 1, '\'', acc, args)
          case c                                           => unquoted(i + 1, acc += c, args)
        }

    @tailrec def quoted(i: Int, quote: Char, acc: Argument, args: List[ByteVector]): Either[String, List[ByteVector]] =
      if (i == line.length) error
      else
        line.charAt(i) match {
          case `quote` if i + 1 == line.length || isSpace(line.charAt(i + 1)) =>
            unquoted(i + 1, new Argument, acc.result() :: args)
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
                quoted(i + 4, quote, acc.byte(Integer.parseInt(line.substring(i + 2, i + 4), 16).toByte), args)
              case c => quoted(i + 2, quote, acc += c, args)
            }
          case c => quoted(i + 1, quote, acc += c, args)
        }

    unquoted(0, new Argument, Nil)
  }

  // ASCII only, as in redis-cli, where a fullwidth digit like U+FF26 isn't one
  private final def isHex(c: Char): Boolean = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  // redis-cli's spaces are ASCII only: any of them separates arguments, but only these end an unquoted one, so a vertical tab or a form
  // feed inside it stays, as does a space that isn't ASCII, like U+2003
  private final def isSpace(c: Char): Boolean = c == ' ' || (c >= '\t' && c <= '\r')
  private final def ends(c: Char): Boolean    = c == ' ' || c == '\t' || c == '\n' || c == '\r'
}
