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

final class TwoOrMoreWeightedKeysSuite extends BaseSpec {
  test("TwoOrMoreWeightedKeys fails to compile given non literal empty List") {
    assertNoDiff(
      compileErrors("TwoOrMoreWeightedKeys(List.empty)"),
      """|error: compile-time refinement only works with literals
         |TwoOrMoreWeightedKeys(List.empty)
         |                     ^
         |""".stripMargin
    )
  }

  test("TwoOrMoreWeightedKeys fails to compile given non literal single element List") {
    assertNoDiff(
      compileErrors("""TwoOrMoreWeightedKeys(List(Key("a") -> ValidDouble(42.0D)))"""),
      """|error: compile-time refinement only works with literals
         |TwoOrMoreWeightedKeys(List(Key("a") -> ValidDouble(42.0D)))
         |                     ^
         |""".stripMargin
    )
  }

  test("TwoOrMoreWeightedKeys fails to compile given non literal List of two elements") {
    assertNoDiff(
      compileErrors("""TwoOrMoreWeightedKeys(List(Key("a") -> ValidDouble(42.0D), Key("b") -> ValidDouble(23.0D)))"""),
      """|error: compile-time refinement only works with literals
         |TwoOrMoreWeightedKeys(List(Key("a") -> ValidDouble(42.0D), Key("b") -> ValidDouble(23.0D)))
         |                     ^
         |""".stripMargin
    )
  }

  test("TwoOrMoreWeightedKeys fails at runtime provided empty List") {
    intercept[IllegalArgumentException](TwoOrMoreWeightedKeys.unsafeFrom(List.empty))
  }

  test("TwoOrMoreWeightedKeys fails at runtime provided single element List") {
    intercept[IllegalArgumentException](TwoOrMoreWeightedKeys.unsafeFrom(List(Key("a") -> ValidDouble(42.0d))))
  }

  property("TwoOrMoreWeightedKeys refines correctly provided non literal cases of Lists of length > 1") {
    forAll(wightedKeyLists.filter(_.size > 1)) { ks =>
      TwoOrMoreWeightedKeys.from(ks) onRight (_.value == ks)
    }
  }
}
