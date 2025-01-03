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

final class NodeIdSuite extends BaseSpec {
  test("NodeId fails to compile given a non conformant String (length < 40)") {
    assertNoDiff(
      compileErrors("""NodeId("0123456789abcdef0123456789abcdef0123456")"""),
      """|error: Predicate failed: "0123456789abcdef0123456789abcdef0123456".matches("[0-9a-f]{40}").
         |NodeId("0123456789abcdef0123456789abcdef0123456")
         |      ^
         |""".stripMargin
    )
  }

  test("NodeId fails to compile given a non conformant String (length > 40)") {
    assertNoDiff(
      compileErrors("""NodeId("0123456789abcdef0123456789abcdef012345678")"""),
      """|error: Predicate failed: "0123456789abcdef0123456789abcdef012345678".matches("[0-9a-f]{40}").
         |NodeId("0123456789abcdef0123456789abcdef012345678")
         |      ^
         |""".stripMargin
    )
  }

  test("NodeId fails to compile given a non conformant String (uppercase)") {
    assertNoDiff(
      compileErrors("""NodeId("0123456789abcdEf0123456789abcdef01234567")"""),
      """|error: Predicate failed: "0123456789abcdEf0123456789abcdef01234567".matches("[0-9a-f]{40}").
         |NodeId("0123456789abcdEf0123456789abcdef01234567")
         |      ^
         |""".stripMargin
    )
  }

  test("NodeId fails to compile given a non conformant String (invalid chars)") {
    assertNoDiff(
      compileErrors("""NodeId("0123456789abcd&f0123456789abcdef01234567&fghijk")"""),
      """|error: Predicate failed: "0123456789abcd&f0123456789abcdef01234567&fghijk".matches("[0-9a-f]{40}").
         |NodeId("0123456789abcd&f0123456789abcdef01234567&fghijk")
         |      ^
         |""".stripMargin
    )
  }

  property("NodeId fails at runtime provided non literal cases of non conformant Strings") {
    forAll(strings.filterNot(nodeIdIsValid)) { d =>
      intercept[IllegalArgumentException](NodeId.unsafeFrom(d))
    }
  }

  test("NodeId compiles given conformant String") {
    NodeId("0123456789abcdef0123456789abcdef01234567")
  }

  property("NodeId refines correctly provided non literal cases of conformant Strings") {
    forAll(nodeIdGen) { i =>
      NodeId.from(i) onRight (_.value == i)
    }
  }
}
