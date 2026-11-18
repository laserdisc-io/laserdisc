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

final class AccessFrequencySuite extends BaseSpec {
  test("AccessFrequency fails to compile given out of range Int (< 0)") {
    assertCompileErrors(
      compileErrors("AccessFrequency(-1)"),
      """|error: Left predicate of (!(-1 < 0) && !(-1 > 255)) failed: Predicate (-1 < 0) did not fail.
         |AccessFrequency(-1)
         |               ^
         |""".stripMargin,
      "Message: Should be included in [0, 255]"
    )
  }

  test("AccessFrequency fails to compile given out of range Int (> 255)") {
    assertCompileErrors(
      compileErrors("AccessFrequency(256)"),
      """|error: Right predicate of (!(256 < 0) && !(256 > 255)) failed: Predicate (256 > 255) did not fail.
         |AccessFrequency(256)
         |               ^
         |""".stripMargin,
      "Message: Should be included in [0, 255]"
    )
  }

  property("AccessFrequency fails at runtime provided non literal cases of out of range Ints (i < 0 | i > 255)") {
    forAll(ints.filterNot(accessFrequencyIsValid)) { i =>
      intercept[IllegalArgumentException](AccessFrequency.unsafeFrom(i))
    }
  }

  test("AccessFrequency compiles given edge cases (0)") {
    AccessFrequency(0)
  }

  test("AccessFrequency compiles given edge cases (255)") {
    AccessFrequency(255)
  }

  property("AccessFrequency refines correctly provided non literal cases of in range Ints (0 <= i <= 255)") {
    forAll(accessFrequencyGen) { i =>
      AccessFrequency.from(i).onRight(_.value == i)
    }
  }
}
