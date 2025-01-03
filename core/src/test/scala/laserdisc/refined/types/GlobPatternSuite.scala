/*
 * Copyright (c) 2018-2025 LaserDisc
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

final class GlobPatternSuite extends BaseSpec {
  test("GlobPattern fails to compile given an empty String") {
    assertNoDiff(
      compileErrors("""GlobPattern("")"""),
      """|error: Predicate failed: "".matches("(\[?[\w\*\?]+\]?)+").
         |GlobPattern("")
         |           ^
         |""".stripMargin
    )
  }

  test("GlobPattern fails to compile given a non conformant String") {
    assertNoDiff(
      compileErrors("""GlobPattern("!")"""),
      """|error: Predicate failed: "!".matches("(\[?[\w\*\?]+\]?)+").
         |GlobPattern("!")
         |           ^
         |""".stripMargin
    )
  }

  property("GlobPattern fails at runtime provided non literal cases of non conformant Strings") {
    forAll(strings.filterNot(globPatternIsValid)) { s =>
      intercept[IllegalArgumentException](GlobPattern.unsafeFrom(s))
    }
  }

  test("GlobPattern compiles given conformant String") {
    GlobPattern("abc*fg?1jk")
    GlobPattern("a[bc*]fg?1jk")
  }

  property("GlobPattern refines correctly provided non literal cases of conformant Strings") {
    forAll(globPatternGen) { s =>
      GlobPattern.from(s) onRight (_.value == s)
    }
  }
}
