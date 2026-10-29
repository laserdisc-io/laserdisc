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
package protocol

final class RESPReadSpec extends BaseSpec {
  private[this] val num: RESPRead.Aux[Num, Long]             = RESPRead.derive
  private[this] val optBulk: RESPRead.Aux[Bulk, Option[Int]] = RESPRead.nullable(Read[Bulk, Int])
  private[this] val optArr: RESPRead.Aux[Arr, Option[Arr]]   = RESPRead.nullable(Read[Arr, Arr])

  test("RESPRead reads a matching RESP type") {
    assertEquals(num.read(Num(42L)), 42L)
  }

  test("RESPRead passes Redis errors through untouched") {
    assertLeftEquals(num.read(Err("boom")), Err("boom"))
    assertLeftEquals(optBulk.read(Err("boom")), Err("boom"))
  }

  test("RESPRead fails when the RESP type does not match") {
    assertLeftEquals(num.read(Str("42")), RESPDecErr("RESP type(s) did not match: Str(42)"))
  }

  test("RESPRead fails when the RESP type matches but cannot be read") {
    assertLeftEquals(
      optBulk.read(Bulk("nope")),
      RESPDecErr(
        "RESP type(s) of Bulk(nope) matched but failed to deserialize correctly with error Read Error: expected Bulk(Int) but was Bulk(nope)"
      )
    )
  }

  test("nullable RESPRead reads the null counterpart of the RESP type as None") {
    assertEquals(optBulk.read(Bulk("42")), Some(42))
    assertEquals(optBulk.read(NullBulk), None)
    assertEquals(optArr.read(Arr(Bulk("a"))), Some(Arr(Bulk("a"))))
    assertEquals(optArr.read(NilArr), None)
  }

  test("nullable RESPRead does not accept the wrong null") {
    assertLeftEquals(optBulk.read(NilArr), RESPDecErr("RESP type(s) did not match: NilArr"))
    assertLeftEquals(optArr.read(NullBulk), RESPDecErr("RESP type(s) did not match: NullBulk"))
  }
}
