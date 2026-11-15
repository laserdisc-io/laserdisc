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

import org.scalacheck.Prop.forAll

final class ConnectionNameSuite extends BaseSpec {
  test("ConnectionName fails to compile given an empty string, a space or a character that isn't printable ASCII") {
    assertCompileErrorsContain(compileErrors("""ConnectionName("")"""), """Predicate failed: "".matches("[!-~]+")""", "Should match [!-~]+")
    assertCompileErrorsContain(
      compileErrors("""ConnectionName("a b")"""),
      """Predicate failed: "a b".matches("[!-~]+")""",
      "Should match [!-~]+"
    )
    assertCompileErrorsContain(
      compileErrors("""ConnectionName("café")"""),
      """Predicate failed: "café".matches("[!-~]+")""",
      "Should match [!-~]+"
    )
  }

  test("ConnectionName refuses control characters at runtime, and checks a long name without overflowing the stack") {
    assert(ConnectionName.from("a\u0000b").isLeft)
    assert(ConnectionName.from("a\u007fb").isLeft)
    assert(ConnectionName.from("x" * 1000000).isRight)
  }

  property("ConnectionName fails at runtime provided non literal cases of strings that contain spaces") {
    forAll(stringsWithSpacesGen.filterNot(connectionNameIsValid)) { s =>
      intercept[IllegalArgumentException](ConnectionName.unsafeFrom(s))
    }
  }

  test("ConnectionName compiles given printable ASCII with no spaces") {
    ConnectionName("a")
    ConnectionName("my-app_1.0~!")
  }

  property("ConnectionName refines correctly provided non literal cases of printable ASCII with no spaces") {
    forAll(connectionNameGen) { s =>
      ConnectionName.from(s).onRight(_.value == s)
    }
  }
}
