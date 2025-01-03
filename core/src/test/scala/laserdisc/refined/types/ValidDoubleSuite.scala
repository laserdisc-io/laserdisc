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

final class ValidDoubleSuite extends BaseSpec {
  test("ValidDouble fails to compile given Double.NaN") {
    assertNoDiff(
      compileErrors("ValidDouble(Double.NaN)"),
      """|error: Predicate failed: (NaN != NaN).
         |ValidDouble(Double.NaN)
         |           ^
         |""".stripMargin
    )
  }

  test("ValidDouble compiles given edge cases (-1.7976931348623157E308) -> can't use Double.MinValue as not a literal") {
    ValidDouble(-1.7976931348623157e308)
  }

  test("ValidDouble compiles given edge cases (Double.MaxValue)") {
    ValidDouble(Double.MaxValue)
  }

  property("ValidDouble refines correctly provided non literal cases of valid Doubles (d != Double.NaN)") {
    forAll(doubles.filter(validDoubleIsValid)) { d =>
      ValidDouble.from(d) onRight (_.value == d)
    }
  }
}
