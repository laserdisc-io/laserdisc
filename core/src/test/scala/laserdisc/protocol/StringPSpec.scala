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

final class StringPSpec extends BaseSpec with EitherSyntax {
  import laserdisc.auto._

  test("The String protocol decoding the wrong type gives details about the decoding error") {
    val correct = strings.set("a", 23)
    assertLeftEquals(
      correct.decode(Arr(Bulk("wrong type"))) leftMap (_.getMessage),
      "RESP type(s) did not match: Arr(Bulk(wrong type))"
    )
  }

  test("The String protocol decoding the correct type with the wrong encoding gives details about the decoding error") {
    val correct = strings.set("a", 23)
    assertLeftEquals(
      correct.decode(Str("wrong")) leftMap (_.getMessage),
      "RESP type(s) of Str(wrong) matched but failed to deserialize correctly with error Read Error: expected Str(OK) but was Str(wrong)"
    )
  }
}
