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
package protocol

import shapeless._

final class ArrSpec extends BaseSpec with EitherSyntax {
  test("decoding the correct type Arr(Bulk) ==> Seq[A] with the wrong encoding gives details about the decoding error") {
    def protocol = Protocol("CUSTOM", _: String :: HNil).as[Arr, Seq[Long]]
    val request  = "id" :: HNil
    val response = Arr(Num(1L), Num(2L), Num(3L), Num(4L))

    assertLeftEquals(
      protocol(request).decode(response) leftMap (_.getMessage),
      "RESP type(s) of Arr(Num(1),Num(2),Num(3),Num(4)) matched but failed to deserialize correctly with error Arr(Bulk) ==> Seq[A] error at element 1: Unexpected for Bulk. Was Num(4)"
    )
  }

  test("decoding the correct type Arr(Bulk) ==> Seq[Option[A]] with the wrong encoding gives details about the decoding error") {
    def protocol = Protocol("CUSTOM", _: String :: HNil).as[Arr, Seq[Option[Long]]]
    val request  = "id" :: HNil
    val response = Arr(Num(1L), NullBulk, Num(3L), NullBulk)

    assertLeftEquals(
      protocol(request).decode(response) leftMap (_.getMessage),
      "RESP type(s) of Arr(Num(1),NullBulk,Num(3),NullBulk) matched but failed to deserialize correctly with error Arr(Bulk) ==> Seq[Option[A]] error at element 2: Unexpected for Bulk. Was Num(3)"
    )
  }
}
