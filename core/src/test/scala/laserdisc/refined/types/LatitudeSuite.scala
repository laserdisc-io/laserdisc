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

final class LatitudeSuite extends BaseSpec {
  test("Latitude fails to compile given out of range Double (< -85.05112878D)") {
    assertNoDiff(
      compileErrors("Latitude(-85.05112879D)"),
      """|error: Left predicate of (!(-85.05112879 < -85.05112878) && !(-85.05112879 > 85.05112878)) failed: Predicate (-85.05112879 < -85.05112878) did not fail.
         |Latitude(-85.05112879D)
         |        ^
         |""".stripMargin
    )
  }

  test("Latitude fails to compile given out of range Double (> 85.05112878D)") {
    assertNoDiff(
      compileErrors("Latitude(85.05112879D)"),
      """|error: Right predicate of (!(85.05112879 < -85.05112878) && !(85.05112879 > 85.05112878)) failed: Predicate (85.05112879 > 85.05112878) did not fail.
         |Latitude(85.05112879D)
         |        ^
         |""".stripMargin
    )
  }

  property("Latitude fails at runtime provided non literal cases of out of range Doubles (d < -85.05112878D | d > 85.05112878D)") {
    forAll(doubles.filterNot(latitudeIsValid)) { d =>
      intercept[IllegalArgumentException](Latitude.unsafeFrom(d))
    }
  }

  test("Key compiles given edge cases (-85.05112878D)") {
    Latitude(-85.05112878d)
  }

  test("Key compiles given edge cases (85.05112878D)") {
    Latitude(85.05112878d)
  }

  property("Latitude refines correctly provided non literal cases of in range Doubles (-85.05112878D <= d <= 85.05112878D)") {
    forAll(latitudeGen) { l =>
      Latitude.from(l) onRight (_.value == l)
    }
  }
}
