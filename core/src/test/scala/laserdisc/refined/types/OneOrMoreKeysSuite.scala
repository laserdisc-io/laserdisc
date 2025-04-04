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

final class OneOrMoreKeysSuite extends BaseSpec {
  test("OneOrMoreKeys fails to compile given non literal empty List") {
    assertNoDiff(
      compileErrors("OneOrMoreKeys(List.empty)"),
      """|error: compile-time refinement only works with literals
         |OneOrMoreKeys(List.empty)
         |             ^
         |""".stripMargin
    )
  }

  test("OneOrMoreKeys fails to compile given non literal non empty List") {
    assertNoDiff(
      compileErrors("""OneOrMoreKeys(List(Key("a")))"""),
      """|error: compile-time refinement only works with literals
         |OneOrMoreKeys(List(Key("a")))
         |             ^
         |""".stripMargin
    )
  }

  test("OneOrMoreKeys fails at runtime provided empty List") {
    intercept[IllegalArgumentException](OneOrMoreKeys.unsafeFrom(List.empty))
  }

  property("OneOrMoreKeys refines correctly provided non literal cases of non empty Lists (length > 0)") {
    forAll(keyLists.filter(_.nonEmpty)) { ks =>
      OneOrMoreKeys.from(ks) onRight (_.value == ks)
    }
  }
}
