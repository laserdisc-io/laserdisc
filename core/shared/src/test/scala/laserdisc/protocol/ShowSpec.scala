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

final class ShowSpec extends BaseSpec {
  test("A Show can only be built with Show's own functions, not written by hand") {
    assertCompileErrorsContain(
      compileErrors("new Show[Int] { override def show(a: Int): String = a.toString }"),
      "illegal inheritance from sealed trait Show",
      "Cannot extend sealed trait Show in a different source file"
    )
  }

  test("A Double is written the same on every platform") {
    val written = List(
      1.0d                    -> "1",
      -3.0d                   -> "-3",
      0.0d                    -> "0",
      -0.0d                   -> "0",
      1.5d                    -> "1.5",
      0.1d                    -> "0.1",
      -0.25d                  -> "-0.25",
      123456789.125d          -> "123456789.125",
      1e15d                   -> "1000000000000000",
      1e20d                   -> "100000000000000000000",
      1e21d                   -> "1e21",
      1.5e22d                 -> "1.5e22",
      0.00001d                -> "0.00001",
      0.000001d               -> "0.000001",
      1e-7d                   -> "1e-7",
      -1.25e-9d               -> "-1.25e-9",
      Double.MaxValue         -> "1.7976931348623157e308",
      Double.MinPositiveValue -> "5e-324",
      1e23d                   -> "1e23",
      0.1d + 0.2d             -> "0.30000000000000004",
      Double.PositiveInfinity -> "+inf",
      Double.NegativeInfinity -> "-inf",
      Double.NaN              -> "nan"
    )
    written.foreach { case (d, text) => assertEquals(Show[Double].show(d), text, d.toString) }
    assertEquals(Show[Latitude].show(Latitude(38.0d)), "38")
    assertEquals(Show[ValidDouble].show(ValidDouble(2.5d)), "2.5")
  }

  property("A Double written reads back as itself") {
    org.scalacheck.Prop.forAll { (d: Double) =>
      assertEquals(Numbers.toDouble(Show[Double].show(d)).map(r => r.equals(d) || (r == 0d && d == 0d)), Some(true), d.toString)
    }
  }

  test("A Show built from Show[Bulk] sends a value's bytes as they are, not as text") {
    final case class Blob(bytes: scodec.bits.ByteVector)
    implicit val blobShow: Show[Blob] = Show[Bulk].contramap(blob => Bulk(blob.bytes))

    val bytes = scodec.bits.ByteVector(0x00, 0xff, 0xc3, 0x28, 0x80)
    assertEquals(Value.fromShow(Blob(bytes)).bulk.bytes, bytes)
    assertEquals(Bulk(Blob(bytes)).bytes, bytes)
    assertEquals(Bulk(Bulk(bytes)).bytes, bytes)
    assertEquals(RESPParamWrite[Blob].write(Blob(bytes)).collect { case b: Bulk => b.bytes }, Seq(bytes))
  }
}
