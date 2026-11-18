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

import scodec.bits.BitVector

final class NumbersSpec extends BaseSpec {
  test("a number reads as servers write it") {
    assertEquals(Read[Bulk, Long].read(Bulk("-42")), -42L)
    assertEquals(Read[Bulk, Int].read(Bulk("7")), 7)
    List("1.5" -> 1.5d, "-0" -> -0.0d, "1e+21" -> 1e21d, "3.0000000000000001e-05" -> 3.0000000000000001e-05d, "2." -> 2.0d, ".5" -> 0.5d)
      .foreach { case (text, value) => assertEquals(Read[Bulk, Double].read(Bulk(text)), value) }
    assertEquals(Read[Bulk, Double].read(Bulk("inf")), Double.PositiveInfinity)
    assertEquals(Read[Bulk, Double].read(Bulk("-inf")), Double.NegativeInfinity)
  }

  test("a number written in a way no server writes it fails to read, instead of being read anyway") {
    List("+5", " 5", "5 ", "", "-", "0x10", "1_000").foreach(text => assert(Read[Bulk, Long].read(Bulk(text)).isLeft, text))
    List("+5", " 5", "", "-").foreach(text => assert(Read[Bulk, Int].read(Bulk(text)).isLeft, text))
    List(" 1.5 ", "0x10p0", "1d", "1f", "Infinity", "NaN", "1e", "e5", ".", "-.", "1.5.2", "")
      .foreach(text => assert(Read[Bulk, Double].read(Bulk(text)).isLeft, text))
  }

  test("a RESP number or length with a + fails to decode") {
    assert(RESP.respCodec.decodeValue(BitVector(":+5\r\n".getBytes)).isFailure)
    assert(RESP.respCodec.decodeValue(BitVector("$+1\r\na\r\n".getBytes)).isFailure)
    assertEquals(RESP.respCodec.decodeValue(BitVector(":-5\r\n".getBytes)).toOption, Some(Num(-5L)))
  }
}
