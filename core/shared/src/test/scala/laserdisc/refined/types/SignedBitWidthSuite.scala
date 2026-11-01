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

import org.scalacheck.Prop.*

final class SignedBitWidthSuite extends BaseSpec {
  test("SignedBitWidth fails to compile given out of range Int (< 1)") {
    assertCompileErrors(
      compileErrors("SignedBitWidth(0)"),
      """|error: Left predicate of (!(0 < 1) && !(0 > 64)) failed: Predicate (0 < 1) did not fail.
         |SignedBitWidth(0)
         |              ^
         |""".stripMargin,
      "Message: Should be included in [1, 64]"
    )
  }

  test("SignedBitWidth fails to compile given out of range Int (> 64)") {
    assertCompileErrors(
      compileErrors("SignedBitWidth(65)"),
      """|error: Right predicate of (!(65 < 1) && !(65 > 64)) failed: Predicate (65 > 64) did not fail.
         |SignedBitWidth(65)
         |              ^
         |""".stripMargin,
      "Message: Should be included in [1, 64]"
    )
  }

  property("SignedBitWidth fails at runtime provided non literal cases of out of range Ints") {
    forAll(ints.filterNot(signedBitWidthIsValid)) { i =>
      intercept[IllegalArgumentException](SignedBitWidth.unsafeFrom(i))
    }
  }

  test("SignedBitWidth compiles given in range Int") {
    SignedBitWidth(1)
    SignedBitWidth(64)
  }

  property("SignedBitWidth refines correctly provided non literal cases of in range Ints") {
    forAll(signedBitWidthGen) { i =>
      SignedBitWidth.from(i).onRight(_.value == i)
    }
  }
}
