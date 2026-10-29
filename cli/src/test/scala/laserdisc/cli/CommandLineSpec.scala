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

final class CommandLineSpec extends FunSuite {
  test("splits a command and its arguments on whitespace") {
    assertEquals(CommandLine.parse("  SET  key\tvalue "), Right(List("SET", "key", "value")))
  }

  test("an empty or blank line has no arguments") {
    assertEquals(CommandLine.parse(""), Right(Nil))
    assertEquals(CommandLine.parse("   "), Right(Nil))
  }

  test("double quotes keep whitespace and support escapes") {
    assertEquals(CommandLine.parse("""SET "my key" "a\"b\\c\n\x41""""), Right(List("SET", "my key", "a\"b\\c\nA")))
  }

  test("single quotes are literal, except for an escaped single quote") {
    assertEquals(CommandLine.parse("""SET 'my key' 'it\'s \n'"""), Right(List("SET", "my key", "it's \\n")))
  }

  test("an empty quoted argument is kept") {
    assertEquals(CommandLine.parse("""SET k """""), Right(List("SET", "k", "")))
  }

  test("unbalanced quotes or a closing quote not followed by a space are invalid") {
    assertEquals(CommandLine.parse("""SET "k v"""), Left("Invalid argument(s)"))
    assertEquals(CommandLine.parse("""SET "k"v"""), Left("Invalid argument(s)"))
  }
}
