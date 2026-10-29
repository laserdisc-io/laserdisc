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

/** Renders a Redis reply the way redis-cli does */
private[cli] object RESPFormat {
  final def format(resp: RESP): String = lines(resp).mkString(LF)

  private final def lines(resp: RESP): List[String] = resp match {
    case Str(value)        => value :: Nil
    case Bulk(value)       => quote(value) :: Nil
    case Num(value)        => s"(integer) $value" :: Nil
    case Err(message)      => s"(error) $message" :: Nil
    case NullBulk | NilArr => "(nil)" :: Nil
    case Arr(Nil)          => "(empty array)" :: Nil
    case Arr(elements)     =>
      val width = elements.size.toString.length
      elements.zipWithIndex.flatMap { case (element, i) =>
        val prefix = s"${(i + 1).toString.reverse.padTo(width, ' ').reverse}) "
        lines(element) match {
          case first :: rest => (prefix + first) :: rest.map(" " * prefix.length + _)
          case Nil           => prefix :: Nil
        }
      }
  }

  private final def quote(s: String): String =
    s.flatMap {
      case '"'                     => "\\\""
      case '\\'                    => "\\\\"
      case '\n'                    => "\\n"
      case '\r'                    => "\\r"
      case '\t'                    => "\\t"
      case c if c < ' ' || c > '~' => f"\\x${c.toInt}%02x"
      case c                       => c.toString
    }.mkString("\"", "", "\"")
}
