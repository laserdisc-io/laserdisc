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

final class AclCategorySuite extends BaseSpec {
  test("AclCategory fails to compile given an empty name, or one with a space or an @") {
    assert(compileErrors("""AclCategory("")""").nonEmpty)
    assert(compileErrors("""AclCategory("no such")""").nonEmpty)
    assert(compileErrors("""AclCategory("@read")""").nonEmpty)
  }

  test("AclCategory compiles given the names Redis, Valkey and Dragonfly give their categories") {
    assertEquals(AclCategory("read").value, "read")
    assertEquals(AclCategory("FT_SEARCH").value, "FT_SEARCH")
    assertEquals(AclCategory("my-module2").value, "my-module2")
  }

  property("AclCategory refines letters, digits, _ and -, and nothing else") {
    forAll(strings) { s =>
      assertEquals(AclCategory.from(s).isRight, s.matches("[A-Za-z0-9_-]+"))
    }
  }
}
