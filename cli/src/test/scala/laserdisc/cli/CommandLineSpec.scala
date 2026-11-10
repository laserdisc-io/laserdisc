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
import scodec.bits.ByteVector

import java.nio.charset.StandardCharsets.UTF_8

final class CommandLineSpec extends FunSuite {
  private def parse(line: String): Either[String, List[String]] = CommandLine.parse(line).map(_.map(b => new String(b.toArray, UTF_8)))

  test("splits a command and its arguments on whitespace") {
    assertEquals(parse("  SET  key\tvalue "), Right(List("SET", "key", "value")))
  }

  test("only ASCII spaces separate arguments, as in redis-cli: a space that isn't ASCII, a vertical tab or a form feed stays in one") {
    assertEquals(parse("SET k a\u2003b"), Right(List("SET", "k", "a\u2003b")))
    assertEquals(parse("SET k a\u000bb\fc"), Right(List("SET", "k", "a\u000bb\fc")))
    assertEquals(parse("\u000b\fSET k \u000b\f\t\r\nv"), Right(List("SET", "k", "v")))
    assertEquals(parse("SET k \"v\"\u000b"), Right(List("SET", "k", "v")))
  }

  test("an empty or blank line has no arguments") {
    assertEquals(parse(""), Right(Nil))
    assertEquals(parse("   "), Right(Nil))
  }

  test("double quotes keep whitespace and support escapes") {
    assertEquals(parse("""SET "my key" "a\"b\\c\n\x41""""), Right(List("SET", "my key", "a\"b\\c\nA")))
  }

  test("single quotes are literal, except for an escaped single quote") {
    assertEquals(parse("""SET 'my key' 'it\'s \n'"""), Right(List("SET", "my key", "it's \\n")))
  }

  test("an empty quoted argument is kept") {
    assertEquals(parse("""SET k """""), Right(List("SET", "k", "")))
  }

  test("unbalanced quotes or a closing quote not followed by a space are invalid") {
    assertEquals(parse("""SET "k v"""), Left("Invalid argument(s)"))
    assertEquals(parse("""SET "k"v"""), Left("Invalid argument(s)"))
  }

  test("double quotes support every escape, and take any other escaped character as it is") {
    assertEquals(parse("""GET "\r\t\b\a\q""""), Right(List("GET", "\r\t\b\u0007q")))
  }

  test("an \\x not followed by two ASCII hex digits is an x") {
    assertEquals(parse("""GET "\xZZ" "\x4""""), Right(List("GET", "xZZ", "x4")))
    assertEquals(parse("GET \"\\x\uFF26\uFF26\""), Right(List("GET", "x\uFF26\uFF26")))
  }

  test("a quote starts quoting anywhere in an argument, as in redis-cli, and must still be closed") {
    assertEquals(parse("""SET k prefix"value""""), Right(List("SET", "k", "prefixvalue")))
    assertEquals(parse("""SET k prefix" suffix""""), Right(List("SET", "k", "prefix suffix")))
    assertEquals(parse("""SET k it'\'s'"""), Right(List("SET", "k", "it's")))
    assertEquals(parse("""SET k prefix"unclosed"""), Left("Invalid argument(s)"))
  }

  test("an \\xHH escape is that byte, and the text around it is UTF-8, as in redis-cli") {
    def value(line: String): Either[String, List[ByteVector]] = CommandLine.parse(line).map(_.drop(2))
    def utf8(text: String): ByteVector                        = ByteVector(text.getBytes(UTF_8))

    assertEquals(value("""SET k "\xc3\xa9""""), Right(List(ByteVector(0xc3, 0xa9))))
    assertEquals(value("""SET k "\xff""""), Right(List(ByteVector(0xff))))
    assertEquals(value("""SET k "é\x80€""""), Right(List(utf8("é") ++ ByteVector(0x80) ++ utf8("€"))))
    assertEquals(value("SET k \uD83D\uDE00"), Right(List(utf8("\uD83D\uDE00"))))
  }
}
