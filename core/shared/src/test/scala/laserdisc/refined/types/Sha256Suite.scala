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

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

final class Sha256Suite extends BaseSpec {
  private val valid = "0123456789abcdef" * 4

  test("Sha256 fails to compile given a String that isn't 64 lowercase hexadecimal characters") {
    assert(compileErrors("""Sha256("0123456789abcdef")""").nonEmpty)
    assert(compileErrors("""Sha256("0123456789ABCDEF0123456789abcdef0123456789abcdef0123456789abcdef")""").nonEmpty)
    assert(compileErrors("""Sha256("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0")""").nonEmpty)
  }

  test("Sha256 compiles given 64 lowercase hexadecimal characters") {
    assertEquals(Sha256("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef").value, valid)
  }

  property("Sha256 refines 64 lowercase hexadecimal characters, and nothing else") {
    forAll(Gen.listOfN(64, Gen.oneOf("0123456789abcdef")).map(_.mkString)) { s =>
      Sha256.from(s).onRight(_.value == s)
    }
    forAll(strings.filterNot(_.matches("[a-f0-9]{64}"))) { s =>
      assert(Sha256.from(s).isLeft)
    }
  }
}
