package laserdisc
package protocol

import java.nio.ByteBuffer

import laserdisc.protocol.BitVectorDecoding._
import laserdisc.protocol.RESP.stateOf
import scodec.bits.BitVector

import scala.annotation.tailrec

private[laserdisc] sealed trait RESPFrame extends Product with Serializable with EitherSyntax with BitVectorSyntax {
  def append(bytes: ByteBuffer): Exception | NonEmptyRESPFrame = nextFrame(BitVector.view(bytes))
  protected final def nextFrame(bits: BitVector): Exception | NonEmptyRESPFrame =
    stateOf(bits)
      .flatMap {
        case MissingBits(n)              => Right(IncompleteFrame(bits, n))
        case Incomplete                  => Right(IncompleteFrame(bits, 0L))
        case Complete                    => Right(CompleteFrame(bits))
        case CompleteWithRemainder(c, r) => consumeRemainder(Right(MoreThanOneFrame(Vector(CompleteFrame(c)), r)))
      }
      .leftMap(e => UnknownBufferState(s"Err building the frame from buffer: $e. Content: ${bits.tailToUtf8}"))

  @tailrec private[this] final def consumeRemainder(current: String | MoreThanOneFrame): String | MoreThanOneFrame = current match {
    case Right(s) =>
      stateOf(s.remainder) match {
        case Left(ee)                           => Left(ee)
        case Right(CompleteWithRemainder(c, r)) => consumeRemainder(Right(MoreThanOneFrame(s.complete.:+(CompleteFrame(c)), r)))
        case Right(Complete)                    => Right(MoreThanOneFrame(s.complete.:+(CompleteFrame(s.remainder)), BitVector.empty))
        case _                                  => Right(s)
      }
    case left => left
  }
}

private[laserdisc] case object EmptyFrame        extends RESPFrame
private[protocol] sealed trait NonEmptyRESPFrame extends RESPFrame

private[laserdisc] final case class CompleteFrame(bits: BitVector) extends NonEmptyRESPFrame
private[laserdisc] final case class MoreThanOneFrame(private[laserdisc] val complete: Vector[CompleteFrame], remainder: BitVector)
    extends NonEmptyRESPFrame
private[laserdisc] final case class IncompleteFrame(partial: BitVector, bitsToComplete: Long) extends NonEmptyRESPFrame {
  override def append(bytes: ByteBuffer): Exception | NonEmptyRESPFrame = {
    val newBits = BitVector.view(bytes)
    //  Saves some size checks
    if (bitsToComplete > 0 && bitsToComplete == newBits.size) Right(CompleteFrame(partial ++ newBits))
    else nextFrame(partial ++ newBits)
  }
}

private[laserdisc] final case class UnknownBufferState(message: String) extends laserdisc.Platform.LaserDiscRespFrameError(message)
