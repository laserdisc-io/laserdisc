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

final class GeoHashSuite extends BaseSpec {
  test("GeoHash fails to compile given a non conformant String (length < 11)") {
    assertCompileErrors(
      compileErrors("""GeoHash("abcdefghij")"""),
      """|error: Predicate failed: "abcdefghij".matches("[a-z0-9]{11}").
         |GeoHash("abcdefghij")
         |       ^
         |""".stripMargin,
      "Message: Should match [a-z0-9]{11}"
    )
  }

  test("GeoHash fails to compile given a non conformant String (length > 11)") {
    assertCompileErrors(
      compileErrors("""GeoHash("abcdefghijkl")"""),
      """|error: Predicate failed: "abcdefghijkl".matches("[a-z0-9]{11}").
         |GeoHash("abcdefghijkl")
         |       ^
         |""".stripMargin,
      "Message: Should match [a-z0-9]{11}"
    )
  }

  test("GeoHash fails to compile given a non conformant String (uppercase)") {
    assertCompileErrors(
      compileErrors("""GeoHash("abCdefghijk")"""),
      """|error: Predicate failed: "abCdefghijk".matches("[a-z0-9]{11}").
         |GeoHash("abCdefghijk")
         |       ^
         |""".stripMargin,
      "Message: Should match [a-z0-9]{11}"
    )
  }

  test("GeoHash fails to compile given a non conformant String (invalid chars)") {
    assertCompileErrors(
      compileErrors("""GeoHash("abcd&fghijk")"""),
      """|error: Predicate failed: "abcd&fghijk".matches("[a-z0-9]{11}").
         |GeoHash("abcd&fghijk")
         |       ^
         |""".stripMargin,
      "Message: Should match [a-z0-9]{11}"
    )
  }

  property("GeoHash fails at runtime provided non literal cases of non conformant Strings") {
    forAll(strings.filterNot(geoHashIsValid)) { s =>
      intercept[IllegalArgumentException](GeoHash.unsafeFrom(s))
    }
  }

  test("GeoHash compiles given conformant String") {
    GeoHash("abcd3fgh1jk")
  }

  property("GeoHash refines correctly provided non literal cases of conformant Strings") {
    forAll(geoHashGen) { s =>
      GeoHash.from(s).onRight(_.value == s)
    }
  }
}
