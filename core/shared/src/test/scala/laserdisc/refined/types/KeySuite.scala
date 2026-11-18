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
package refined.types

final class KeySuite extends BaseSpec {
  test("Key fails to compile given an empty String") {
    assertCompileErrorsContain(
      compileErrors("""Key("")"""),
      "Predicate failed: isKey().",
      "Should not be empty, nor hold half of a character"
    )
  }

  test("Key refuses half of a character, a lone surrogate, which would be sent as ? and so name another key") {
    assert(Key.from("a\uD800").isLeft)
    assert(Key.from("\uDC00b").isLeft)
    assert(Key.from("a\uD800\uD800").isLeft)
    assert(compileErrors("""Key("a\uD800")""").nonEmpty)
    assertEquals(Key.from("a😀b").map(_.value), "a😀b")
  }

  test("Key takes control characters, as the server does in a name") {
    assertEquals(Key.from("a\u0001\nb").map(_.value), "a\u0001\nb")
  }

  test("Key compiles given a non empty String") {
    Key("a")
  }
}
