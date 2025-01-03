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

final class NonNegDoubleSuite extends BaseSpec {
  test("NonNegDouble fails to compile given out of range Double (< 0.0D)") {
    assertNoDiff(
      compileErrors("NonNegDouble(-0.00000001D)"),
      """|error: Right predicate of ((-1.0E-8 != NaN) && !(-1.0E-8 < 0.0)) failed: Predicate (-1.0E-8 < 0.0) did not fail.
         |NonNegDouble(-0.00000001D)
         |            ^
         |""".stripMargin
    )
  }

  test("NonNegDouble fails to compile given NaN Double") {
    assertNoDiff(
      compileErrors("NonNegDouble(Double.NaN)"),
      """|error: Left predicate of ((NaN != NaN) && !(NaN < 0.0)) failed: Predicate failed: (NaN != NaN).
         |NonNegDouble(Double.NaN)
         |            ^
         |""".stripMargin
    )
  }

  property("NonNegDouble fails at runtime provided non literal cases of out of range Doubles (d < 0.0D)") {
    forAll(doubles.filterNot(nonNegDoubleIsValid)) { d =>
      intercept[IllegalArgumentException](NonNegDouble.unsafeFrom(d))
    }
  }

  test("NonNegDouble compiles given edge cases (0.0D)") {
    NonNegDouble(0.0d)
  }

  property("NonNegDouble refines correctly provided non literal cases of in range Doubles (d >= 0.0D)") {
    forAll(nonNegDoubleGen) { d =>
      NonNegDouble.from(d) onRight (_.value == d)
    }
  }
}
