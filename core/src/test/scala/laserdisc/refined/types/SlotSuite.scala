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

final class SlotSuite extends BaseSpec {
  test("Slot fails to compile given out of range Int (< 0)") {
    assertNoDiff(
      compileErrors("Slot(-1)"),
      """|error: Left predicate of (!(-1 < 0) && !(-1 > 16383)) failed: Predicate (-1 < 0) did not fail.
         |Slot(-1)
         |    ^
         |""".stripMargin
    )
  }

  test("Slot fails to compile given out of range Int (> 16383)") {
    assertNoDiff(
      compileErrors("Slot(16384)"),
      """|error: Right predicate of (!(16384 < 0) && !(16384 > 16383)) failed: Predicate (16384 > 16383) did not fail.
         |Slot(16384)
         |    ^
         |""".stripMargin
    )
  }

  property("Slot fails at runtime provided non literal cases of out of range Ints (i < 0 | i > 16383)") {
    forAll(ints.filterNot(slotIsValid)) { i =>
      intercept[IllegalArgumentException](Slot.unsafeFrom(i))
    }
  }

  test("NonNegLong compiles given edge cases (0)") {
    Slot(0)
  }

  test("NonNegLong compiles given edge cases (16383)") {
    Slot(16383)
  }
}
