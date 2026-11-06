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

final class BlockingSecondsSuite extends BaseSpec {
  test("BlockingSeconds fails to compile below its smallest wait, which servers can read as waiting forever") {
    assert(compileErrors("BlockingSeconds(0.001d)").nonEmpty)
  }

  test("BlockingSeconds fails to compile above what every server takes") {
    assert(compileErrors("BlockingSeconds(4294967.5d)").nonEmpty)
  }

  test("BlockingSeconds fails to compile given a negative wait") {
    assert(compileErrors("BlockingSeconds(-1.0d)").nonEmpty)
  }

  test("BlockingSeconds compiles given its edge cases") {
    BlockingSeconds(0.0d)
    BlockingSeconds(0.002d)
    BlockingSeconds(4294967.0d)
  }

  property("BlockingSeconds fails at runtime provided non literal cases of what it doesn't take") {
    forAll(doubles.filterNot(blockingSecondsIsValid)) { d =>
      intercept[IllegalArgumentException](BlockingSeconds.unsafeFrom(d))
    }
  }

  test("BlockingSeconds fails at runtime given infinity or NaN") {
    List(Double.PositiveInfinity, Double.NaN, 0.0019d).foreach(d => intercept[IllegalArgumentException](BlockingSeconds.unsafeFrom(d)))
  }

  property("BlockingSeconds refines correctly provided non literal cases of what it takes") {
    forAll(blockingSecondsGen) { d =>
      BlockingSeconds.from(d).onRight(_.value == d)
    }
  }
}
