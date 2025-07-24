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

package laserdisc.protocol

import java.nio.charset.Charset

import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.bits.{BitVector, ByteVector}

final class LenientStringCodec(private[this] val charset: Charset) extends Codec[String] {
  override def sizeBound: SizeBound                                   = SizeBound.unknown
  override def encode(str: String): Attempt[BitVector]                = Attempt.successful(ByteVector.view(str.getBytes(charset)).bits)
  override def decode(bits: BitVector): Attempt[DecodeResult[String]] =
    Attempt.successful(DecodeResult(new String(bits.toByteArray, charset), BitVector.empty))
  override def toString: String = s"lenient-${charset.displayName}"
}
