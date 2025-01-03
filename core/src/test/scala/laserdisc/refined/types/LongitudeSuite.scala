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

final class LongitudeSuite extends BaseSpec {
  test("Longitude fails to compile given out of range Double (< -180.0D)") {
    assertNoDiff(
      compileErrors("Longitude(-180.00000001D)"),
      """|error: Left predicate of (!(-180.00000001 < -180.0) && !(-180.00000001 > 180.0)) failed: Predicate (-180.00000001 < -180.0) did not fail.
         |Longitude(-180.00000001D)
         |         ^
         |""".stripMargin
    )
  }

  test("Longitude fails to compile given out of range Double (> 180.0D)") {
    assertNoDiff(
      compileErrors("Longitude(180.00000001D)"),
      """|error: Right predicate of (!(180.00000001 < -180.0) && !(180.00000001 > 180.0)) failed: Predicate (180.00000001 > 180.0) did not fail.
         |Longitude(180.00000001D)
         |         ^
         |""".stripMargin
    )
  }

  property("Longitude fails at runtime provided non literal cases of out of range Doubles (d < -180.0D | d > 180.0D)") {
    forAll(doubles.filterNot(longitudeIsValid)) { d =>
      intercept[IllegalArgumentException](Longitude.unsafeFrom(d))
    }
  }

  test("Longitude compiles given edge cases (-180.0D)") {
    Longitude(-180.0d)
  }

  test("Longitude compiles given edge cases (180.0D)") {
    Longitude(180.0d)
  }

  property("Longitude refines correctly provided non literal cases of in range Doubles (-180.0D <= d <= 180.0D)") {
    forAll(longitudeGen) { l =>
      Longitude.from(l) onRight (_.value == l)
    }
  }
}
