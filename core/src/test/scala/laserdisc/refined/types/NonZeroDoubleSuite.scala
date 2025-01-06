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

final class NonZeroDoubleSuite extends BaseSpec {
  test("NonZeroDouble fails to compile given 0.0D") {
    assertNoDiff(
      compileErrors("NonZeroDouble(0.0D)"),
      """|error: Right predicate of ((0.0 != NaN) && !(0.0 == 0.0)) failed: Predicate (0.0 == 0.0) did not fail.
         |NonZeroDouble(0.0D)
         |             ^
         |""".stripMargin
    )
  }

  test("NonZeroDouble fails to compile given NaN") {
    assertNoDiff(
      compileErrors("NonZeroDouble(Double.NaN)"),
      """|error: Left predicate of ((NaN != NaN) && !(NaN == 0.0)) failed: Predicate failed: (NaN != NaN).
         |NonZeroDouble(Double.NaN)
         |             ^
         |""".stripMargin
    )
  }

  property("NonZeroDouble refines correctly provided non literal cases of valid Doubles (d != 0.0D)") {
    forAll(nonZeroDoubleGen) { d =>
      NonZeroDouble.from(d) onRight (_.value == d)
    }
  }
}
