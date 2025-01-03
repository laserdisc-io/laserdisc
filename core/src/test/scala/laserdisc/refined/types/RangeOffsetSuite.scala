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

final class RangeOffsetSuite extends BaseSpec {
  test("RangeOffset fails to compile given out of range Int (< 0)") {
    assertNoDiff(
      compileErrors("RangeOffset(-1)"),
      """|error: Left predicate of (!(-1 < 0) && !(-1 > 536870911)) failed: Predicate (-1 < 0) did not fail.
         |RangeOffset(-1)
         |           ^
         |""".stripMargin
    )
  }

  test("RangeOffset fails to compile given out of range Int (> 536870911)") {
    assertNoDiff(
      compileErrors("RangeOffset(536870912)"),
      """|error: Right predicate of (!(536870912 < 0) && !(536870912 > 536870911)) failed: Predicate (536870912 > 536870911) did not fail.
         |RangeOffset(536870912)
         |           ^
         |""".stripMargin
    )
  }

  property("RangeOffset fails at runtime provided non literal cases of out of range Ints (i < 0 | i > 536870911)") {
    forAll(ints.filterNot(rangeOffsetIsValid)) { i =>
      intercept[IllegalArgumentException](RangeOffset.unsafeFrom(i))
    }
  }

  test("RangeOffset compiles given edge cases (0)") {
    RangeOffset(0)
  }

  test("RangeOffset compiles given edge cases (536870911)") {
    RangeOffset(536870911)
  }

  property("RangeOffset refines correctly provided non literal cases of in range Ints (0 <= i <= 536870911)") {
    forAll(rangeOffsetGen) { i =>
      RangeOffset.from(i) onRight (_.value == i)
    }
  }
}
