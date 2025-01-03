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

final class NonNegIntSuite extends BaseSpec {
  test("NonNegInt fails to compile given out of range Ints (< 0)") {
    assertNoDiff(
      compileErrors("NonNegInt(-1)"),
      """|error: Predicate (-1 < 0) did not fail.
         |NonNegInt(-1)
         |         ^
         |""".stripMargin
    )
  }

  property("NonNegInt fails at runtime provided non literal cases of out of range Ints (i < 0)") {
    forAll(ints.filterNot(nonNegIntIsValid)) { i =>
      intercept[IllegalArgumentException](NonNegInt.unsafeFrom(i))
    }
  }

  test("NonNegInt compiles given edge cases (0)") {
    NonNegInt(0)
  }

  property("NonNegInt refines correctly provided non literal cases of in range Ints (i > 0)") {
    forAll(nonNegIntGen) { i =>
      NonNegInt.from(i) onRight (_.value == i)
    }
  }
}
