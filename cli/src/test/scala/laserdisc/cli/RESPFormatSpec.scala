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

import munit.FunSuite

final class RESPFormatSpec extends FunSuite {
  test("formats simple replies like redis-cli") {
    assertEquals(RESPFormat.format(Str("OK")), "OK")
    assertEquals(RESPFormat.format(Bulk("a \"b\"\n")), "\"a \\\"b\\\"\\n\"")
    assertEquals(RESPFormat.format(Num(42L)), "(integer) 42")
    assertEquals(RESPFormat.format(Err("ERR boom")), "(error) ERR boom")
    assertEquals(RESPFormat.format(NullBulk), "(nil)")
    assertEquals(RESPFormat.format(NilArr), "(nil)")
    assertEquals(RESPFormat.format(Arr(Nil)), "(empty array)")
  }

  test("formats (nested) arrays as numbered, aligned lines") {
    val nested = Arr(Bulk("a"), Arr(Num(1L), NullBulk))
    assertEquals(RESPFormat.format(nested), List("1) \"a\"", "2) 1) (integer) 1", "   2) (nil)").mkString("\n"))
    assertEquals(
      RESPFormat.format(Arr((1 to 10).map(i => Num(i.toLong)).toList)).split("\n").toList.take(2),
      List(" 1) (integer) 1", " 2) (integer) 2")
    )
  }
}
