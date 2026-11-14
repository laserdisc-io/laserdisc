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
package protocol

final class ReadSpec extends BaseSpec {
  test("A Read can only be built with Read's own functions, not written by hand") {
    assertCompileErrorsContain(
      compileErrors("new Read[Bulk, String] { override def read(a: Bulk): Either[RESPDecErr, String] = Right(a.value) }"),
      "illegal inheritance from sealed trait Read",
      "Cannot extend sealed trait Read in a different source file"
    )
  }

  test("An error message is cut after 1000 characters, saying how much was left out") {
    val long = RESPDecErr("x" * 1500)
    assertEquals(long.message, ("x" * 1000) + "… (500 more characters)")
    assertEquals(long.getMessage, long.message)
    assertEquals(RESPDecErr("short").message, "short")
  }

  test("Failing to read a long reply says why before the reply, which is cut") {
    val failure = Protocol("GET", "k" *: EmptyTuple).as[Bulk, Int].decode(Bulk("a" * 5000))
    failure match {
      case Left(RESPDecErr(message)) =>
        assert(
          message.startsWith("RESP type(s) matched but failed to deserialize correctly with error Read Error: expected Bulk(Int)"),
          message
        )
        assert(message.endsWith(" more characters)"), message)
        assert(message.length < 1100, message.length.toString)
      case other => fail(s"expected a read error, got $other")
    }
  }
}
