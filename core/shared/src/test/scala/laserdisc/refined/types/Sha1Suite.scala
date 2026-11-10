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

final class Sha1Suite extends BaseSpec {
  test("Sha1 fails to compile given a non conformant String (length < 40)") {
    assertCompileErrors(
      compileErrors("""Sha1("0123456789abcdef0123456789abcdef0123456")"""),
      """|error: Predicate failed: "0123456789abcdef0123456789abcdef0123456".matches("[a-f0-9]{40}").
         |Sha1("0123456789abcdef0123456789abcdef0123456")
         |    ^
         |""".stripMargin,
      "Message: Should match [a-f0-9]{40}"
    )
  }

  test("Sha1 fails to compile given a non conformant String (length > 40)") {
    assertCompileErrors(
      compileErrors("""Sha1("0123456789abcdef0123456789abcdef012345678")"""),
      """|error: Predicate failed: "0123456789abcdef0123456789abcdef012345678".matches("[a-f0-9]{40}").
         |Sha1("0123456789abcdef0123456789abcdef012345678")
         |    ^
         |""".stripMargin,
      "Message: Should match [a-f0-9]{40}"
    )
  }

  test("Sha1 fails to compile given a non conformant String (uppercase)") {
    assertCompileErrors(
      compileErrors("""Sha1("0123456789abcdEf0123456789abcdef01234567")"""),
      """|error: Predicate failed: "0123456789abcdEf0123456789abcdef01234567".matches("[a-f0-9]{40}").
         |Sha1("0123456789abcdEf0123456789abcdef01234567")
         |    ^
         |""".stripMargin,
      "Message: Should match [a-f0-9]{40}"
    )
  }

  test("Sha1 fails to compile given a non conformant String (invalid chars)") {
    assertCompileErrors(
      compileErrors("""Sha1("0123456789abcd&f0123456789abcdef01234567&fghijk")"""),
      """|error: Predicate failed: "0123456789abcd&f0123456789abcdef01234567&fghijk".matches("[a-f0-9]{40}").
         |Sha1("0123456789abcd&f0123456789abcdef01234567&fghijk")
         |    ^
         |""".stripMargin,
      "Message: Should match [a-f0-9]{40}"
    )
  }

  property("Sha1 fails at runtime provided non literal cases of non conformant Strings") {
    forAll(strings.filterNot(sha1IsValid)) { d =>
      intercept[IllegalArgumentException](Sha1.unsafeFrom(d))
    }
  }

  test("Sha1 compiles given conformant String") {
    Sha1("0123456789abcdef0123456789abcdef01234567")
  }

  property("Sha1 refines correctly provided non literal cases of conformant Strings") {
    forAll(sha1Gen) { i =>
      Sha1.from(i).onRight(_.value == i)
    }
  }
}
