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

import scodec.bits.ByteVector

final class RESPFrameLargeReplySpec extends BaseSpec {
  private val reply = {
    val element = s"$$200\r\n${"x" * 200}\r\n"
    ByteVector(s"*100000\r\n${element * 100000}".getBytes("UTF-8"))
  }

  // looking at all of it again for each chunk took minutes for a fraction of this size
  test("a reply of 20 MB arriving in chunks of 8 KB is found in much less time than the default timeout of a command") {
    val start = System.nanoTime()
    val frame = reply.grouped(8192L).foldLeft[Either[Exception, RESPFrame]](Right(EmptyFrame))((f, c) => f.flatMap(_.append(c.bits)))
    val taken = (System.nanoTime() - start) / 1000000L

    assertEquals(frame, CompleteFrame(reply.bits))
    assert(taken < 10000L, s"took $taken ms")
  }
}
