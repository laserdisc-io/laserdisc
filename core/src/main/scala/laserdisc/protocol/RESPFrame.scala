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

import laserdisc.protocol.BitVectorDecoding._
import laserdisc.protocol.RESP.stateOf
import scodec.bits.BitVector

import scala.annotation.tailrec

private[laserdisc] sealed trait RESPFrame extends Product with Serializable with EitherSyntax with BitVectorSyntax {
  def append(bits: BitVector): Exception | NonEmptyRESPFrame = nextFrame(bits)
  protected final def nextFrame(bits: BitVector): Exception | NonEmptyRESPFrame =
    stateOf(bits) match {
      case Right(MissingBits(n))              => Right(IncompleteFrame(bits, n))
      case Right(Incomplete)                  => Right(IncompleteFrame(bits, 0L))
      case Right(Complete)                    => Right(CompleteFrame(bits))
      case Right(CompleteWithRemainder(c, r)) => consumeRemainder(MoreThanOneFrame(Vector(CompleteFrame(c)), r))
      case Left(e) => Left(UnknownBufferState(s"Err building the frame from buffer: $e. Content: ${bits.tailToUtf8}"))
    }

  @tailrec private[this] final def consumeRemainder(current: MoreThanOneFrame): Exception | MoreThanOneFrame =
    stateOf(current.remainder) match {
      case Right(CompleteWithRemainder(c, r)) => consumeRemainder(MoreThanOneFrame(current.complete :+ CompleteFrame(c), r))
      case Right(Complete) => Right(MoreThanOneFrame(current.complete :+ CompleteFrame(current.remainder), BitVector.empty))
      case Left(ee) => Left(UnknownBufferState(s"Err building the frame from a remainder: $ee. Content: ${current.remainder.tailToUtf8}"))
      case _        => Right(current)
    }
}

private[laserdisc] case object EmptyFrame        extends RESPFrame
private[protocol] sealed trait NonEmptyRESPFrame extends RESPFrame

private[laserdisc] final case class CompleteFrame(bits: BitVector) extends NonEmptyRESPFrame
private[laserdisc] final case class MoreThanOneFrame(private[laserdisc] val complete: Vector[CompleteFrame], remainder: BitVector)
    extends NonEmptyRESPFrame
private[laserdisc] final case class IncompleteFrame(partial: BitVector, bitsToComplete: Long) extends NonEmptyRESPFrame {
  override def append(bits: BitVector): Exception | NonEmptyRESPFrame =
    //  Saves some size checks
    if (bitsToComplete > 0 && bitsToComplete == bits.size) Right(CompleteFrame(partial ++ bits))
    else nextFrame(partial ++ bits)
}

private[laserdisc] final case class UnknownBufferState(message: String) extends laserdisc.Platform.LaserDiscRespFrameError(message)
