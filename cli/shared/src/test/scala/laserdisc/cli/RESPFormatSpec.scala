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

  test("formats a value byte by byte, with \\xHH for each byte that isn't printable ASCII, as redis-cli does") {
    assertEquals(RESPFormat.format(Bulk("€")), "\"\\xe2\\x82\\xac\"")
    assertEquals(RESPFormat.format(Bulk(scodec.bits.ByteVector(0x80, 0xff))), "\"\\x80\\xff\"")
    assertEquals(RESPFormat.format(Bulk("a\u0007\bz")), "\"a\\a\\bz\"")
  }

  test("shows the control characters of a status or an error as \\xHH, so that a server can't drive the terminal with them") {
    assertEquals(RESPFormat.format(Str("a\u001b[2Jb\u007f")), "a\\x1b[2Jb\\x7f")
    assertEquals(RESPFormat.format(Err("ERR \u001b]0;title\u0007\u009b")), "(error) ERR \\x1b]0;title\\x07\\x9b")
    assertEquals(RESPFormat.format(Str("café €")), "café €")
    assertEquals(RESPFormat.printable("refused: \u001b[31m"), "refused: \\x1b[31m")
  }

  test("formats (nested) arrays as numbered, aligned lines") {
    val nested = Arr(Bulk("a"), Arr(Num(1L), NullBulk))
    assertEquals(RESPFormat.format(nested), List("1) \"a\"", "2) 1) (integer) 1", "   2) (nil)").mkString("\n"))
    assertEquals(
      RESPFormat.format(Arr((1 to 10).map(i => Num(i.toLong)).toList)).split("\n").toList.take(2),
      List(" 1) (integer) 1", " 2) (integer) 2")
    )
  }

  test(
    "shows the text of INFO, CLIENT LIST and the other commands redis-cli shows raw, a line at a time, with control characters as \\xHH"
  ) {
    assert(RESPFormat.isRaw("info", Nil))
    assert(RESPFormat.isRaw("CLIENT", List("list", "TYPE", "normal")))
    assert(RESPFormat.isRaw("cluster", List("nodes")))
    assert(RESPFormat.isRaw("LATENCY", List("graph", "command")))
    assert(!RESPFormat.isRaw("cluster", List("nodes", "x")))
    assert(!RESPFormat.isRaw("GET", List("info")))
    assertEquals(
      RESPFormat.formatRaw(Bulk("# Server\r\nredis_version:8.10.3\r\n\u001b[2J\r\n")),
      "# Server\nredis_version:8.10.3\n\\x1b[2J"
    )
    assertEquals(RESPFormat.formatRaw(Num(1L)), "(integer) 1")
  }
}
