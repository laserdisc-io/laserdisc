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

final class UnsignedBitWidthSuite extends BaseSpec {
  test("UnsignedBitWidth fails to compile given out of range Int (< 1)") {
    assertCompileErrors(
      compileErrors("UnsignedBitWidth(0)"),
      """|error: Left predicate of (!(0 < 1) && !(0 > 63)) failed: Predicate (0 < 1) did not fail.
         |UnsignedBitWidth(0)
         |                ^
         |""".stripMargin,
      "Message: Should be included in [1, 63]"
    )
  }

  test("UnsignedBitWidth fails to compile given out of range Int (> 63)") {
    assertCompileErrors(
      compileErrors("UnsignedBitWidth(64)"),
      """|error: Right predicate of (!(64 < 1) && !(64 > 63)) failed: Predicate (64 > 63) did not fail.
         |UnsignedBitWidth(64)
         |                ^
         |""".stripMargin,
      "Message: Should be included in [1, 63]"
    )
  }

  property("UnsignedBitWidth fails at runtime provided non literal cases of out of range Ints") {
    forAll(ints.filterNot(unsignedBitWidthIsValid)) { i =>
      intercept[IllegalArgumentException](UnsignedBitWidth.unsafeFrom(i))
    }
  }

  test("UnsignedBitWidth compiles given in range Int") {
    UnsignedBitWidth(1)
    UnsignedBitWidth(63)
  }

  property("UnsignedBitWidth refines correctly provided non literal cases of in range Ints") {
    forAll(unsignedBitWidthGen) { i =>
      UnsignedBitWidth.from(i).onRight(_.value == i)
    }
  }
}
