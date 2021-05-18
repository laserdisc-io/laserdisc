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
      case Left(e)                            => Left(UnknownBufferState(s"Err building the frame from buffer: $e. Content: ${bits.tailToUtf8}"))
    }

  @tailrec private[this] final def consumeRemainder(current: MoreThanOneFrame): Exception | MoreThanOneFrame =
    stateOf(current.remainder) match {
      case Right(CompleteWithRemainder(c, r)) => consumeRemainder(MoreThanOneFrame(current.complete :+ CompleteFrame(c), r))
      case Right(Complete)                    => Right(MoreThanOneFrame(current.complete :+ CompleteFrame(current.remainder), BitVector.empty))
      case Left(ee)                           => Left(UnknownBufferState(s"Err building the frame from a remainder: $ee. Content: ${current.remainder.tailToUtf8}"))
      case _                                  => Right(current)
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
