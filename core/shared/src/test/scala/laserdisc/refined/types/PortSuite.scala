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

final class PortSuite extends BaseSpec {
  test("Port fail to compile given out of range Int (< 1)") {
    assertCompileErrors(
      compileErrors("Port(0)"),
      """|error: Left predicate of (!(0 < 1) && !(0 > 65535)) failed: Predicate (0 < 1) did not fail.
         |Port(0)
         |    ^
         |""".stripMargin,
      "Message: Should be included in [1, 65535]"
    )
  }

  test("Port fail to compile given out of range Int (> 65535)") {
    assertCompileErrors(
      compileErrors("Port(65536)"),
      """|error: Right predicate of (!(65536 < 1) && !(65536 > 65535)) failed: Predicate (65536 > 65535) did not fail.
         |Port(65536)
         |    ^
         |""".stripMargin,
      "Message: Should be included in [1, 65535]"
    )
  }

  property("Port fails at runtime provided non literal cases of out of range Ints") {
    forAll(ints.filterNot(i => i >= 1 && i <= 65535)) { i =>
      intercept[IllegalArgumentException](Port.unsafeFrom(i))
    }
  }

  test("Port compiles given in range Int") {
    Port(80)
    Port(6379)
    Port(65535)
  }

  property("Port refines correctly provided non literal cases of in range Ints") {
    forAll(portGen) { p =>
      Port.from(p.value).onRight(_.value == p.value)
    }
  }
}
