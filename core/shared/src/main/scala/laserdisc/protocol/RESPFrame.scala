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

import laserdisc.protocol.BitVectorDecoding.*
import laserdisc.protocol.RESP.stateOf
import scodec.bits.BitVector

import scala.annotation.tailrec

private[laserdisc] sealed trait RESPFrame extends Product with Serializable with BitVectorSyntax {
  def append(bits: BitVector): Either[Exception, NonEmptyRESPFrame]                    = nextFrame(bits)
  protected final def nextFrame(bits: BitVector): Either[Exception, NonEmptyRESPFrame] = nextFrame(bits, new FrameProgress(bits, Nil))

  // all the bits, and where in them to carry on looking for the end of the frame
  protected final def nextFrame(bits: BitVector, progress: FrameProgress): Either[Exception, NonEmptyRESPFrame] = {
    val rest = progress.rest.compact.bytes
    RESP.scan(rest, 0L, progress.open) match {
      // stopped with nothing missing and something to look at: an element whose first line hasn't ended yet
      case Right(Stopped(at, open, missing)) =>
        val inLine = missing == 0L && at < rest.size
        if (inLine && rest.size - at > progress.maxLine) Left(progress.lineTooLong)
        else Right(IncompleteFrame(bits, missing)(new FrameProgress(rest.drop(at).bits, open, inLine, progress.maxLine)))
      case Right(Scanned(end)) if end == rest.size => Right(CompleteFrame(bits.compact))
      case Right(Scanned(end))                     =>
        val complete = bits.compact
        val size     = bits.size - rest.drop(end).bits.size
        consumeRemainder(MoreThanOneFrame(Vector(CompleteFrame(complete.take(size))), complete.drop(size)))
      case Left(e) => Left(UnknownBufferState(s"Err building the frame from buffer: $e. Content: ${bits.tailToUtf8}"))
    }
  }

  // a frame that isn't complete, or that isn't valid, stays in the remainder, so that the complete ones before it are still read: what
  // reads the frames finds the error when it goes on with the remainder
  @tailrec private final def consumeRemainder(current: MoreThanOneFrame): Either[Exception, MoreThanOneFrame] =
    stateOf(current.remainder) match {
      case Right(CompleteWithRemainder(c, r)) => consumeRemainder(MoreThanOneFrame(current.complete :+ CompleteFrame(c), r))
      case Right(Complete) => Right(MoreThanOneFrame(current.complete :+ CompleteFrame(current.remainder), BitVector.empty))
      case _               => Right(current)
    }
}

private[laserdisc] case object EmptyFrame        extends RESPFrame
private[protocol] sealed trait NonEmptyRESPFrame extends RESPFrame

private[laserdisc] final case class CompleteFrame(bits: BitVector) extends NonEmptyRESPFrame
private[laserdisc] final case class MoreThanOneFrame(private[laserdisc] val complete: Vector[CompleteFrame], remainder: BitVector)
    extends NonEmptyRESPFrame
private[laserdisc] final case class IncompleteFrame(partial: BitVector, bitsToComplete: Long)(
    progress: FrameProgress = new FrameProgress(partial, Nil)
) extends NonEmptyRESPFrame {
  override def append(bits: BitVector): Either[Exception, NonEmptyRESPFrame] = {
    val all  = partial ++ bits
    val rest = progress.rest ++ bits
    // the element it stopped at can't be complete yet, so there's nothing to look at. One it stopped at for want of the end of its line,
    // as a long status, error or number can be, isn't complete either until the line ends in what came
    if (bits.size < bitsToComplete)
      Right(IncompleteFrame(all, bitsToComplete - bits.size)(new FrameProgress(rest, progress.open, false, progress.maxLine)))
    else if (progress.inLine && !endsALine(progress.rest, bits))
      if (rest.size / 8L > progress.maxLine) Left(progress.lineTooLong)
      else Right(IncompleteFrame(all, 0L)(new FrameProgress(rest, progress.open, true, progress.maxLine)))
    else nextFrame(all, new FrameProgress(rest, progress.open, false, progress.maxLine))
  }

  // whether the bits that came have a CRLF, or the LF of a CR that came before them
  private def endsALine(before: BitVector, bits: BitVector): Boolean = {
    val came     = bits.bytes
    val crBefore = before.takeRight(8L).bytes.lastOption.contains(CR_CH.toByte)
    came.indexOfSlice(RESP.crlfBytes) >= 0L || (crBefore && came.headOption.contains(LF_CH.toByte))
  }
}

// what's left to look at, from the start of the element it stopped at, how many more elements each array it's in is waiting for,
// whether it stopped for want of the end of that element's first line, and how many bytes that line may have before it's given up on, as a
// server could send one that never ends
private[protocol] final class FrameProgress(
    val rest: BitVector,
    val open: List[Long],
    val inLine: Boolean = false,
    val maxLine: Long = RESP.MaxSize
) {
  def lineTooLong: UnknownBufferState = UnknownBufferState(s"Expected a line of at most $maxLine bytes, but it hasn't ended")
}

private[laserdisc] final case class UnknownBufferState(message: String) extends RuntimeException(message, null, true, false)
