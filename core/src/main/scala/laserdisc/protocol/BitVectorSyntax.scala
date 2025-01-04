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

import scodec.bits.BitVector

private[protocol] trait BitVectorSyntax {
  implicit def bitVectorSyntax(bv: BitVector): BitVectorSyntaxOps = new BitVectorSyntaxOps(bv)
}

private[protocol] final class BitVectorSyntaxOps(private val bv: BitVector) extends AnyVal {

  /** Tries to decode the last 48 bytes of the bit vector as UTF-8 text
    */
  def tailToUtf8: String = bv.takeRight(48 * 8).decodeUtf8.getOrElse("content is not UTF-8 encoded")

  /** Tries to decode the whole bit vector to UTF-8 text
    */
  def toUtf8: String = bv.decodeUtf8.getOrElse("content is not UTF-8 encoded")
}
