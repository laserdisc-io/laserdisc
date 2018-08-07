package laserdisc
package protocol

import java.nio.ByteBuffer

import laserdisc.protocol.BitVectorDecoding.{CompleteVector, _}
import scodec.bits.BitVector

import scala.annotation.tailrec

sealed trait RESPFrame extends Product with Serializable with EitherSyntax {

  def append(bytes: ByteBuffer): Exception | NonEmptyFrame =
    nextFrame(BitVector.view(bytes))

  protected final def nextFrame(bits: BitVector): Exception | NonEmptyFrame =
    RESP.stateOf(bits) flatMap {
      case MissingBits(n)              => Incomplete(bits, n).asRight
      case CompleteAndDecoded(r)       => Decoded(r.value).asRight
      case IncompleteVector            => Incomplete(bits, 0L).asRight
      case CompleteVector              => Complete(bits).asRight
      case CompleteWithRemainder(c, r) => consumeRemainder(MoreThanOne(Complete(c) :: Nil, r).asRight)
    } leftMap (e => new Exception(s"Error: $e - Vector content: ${bits.toByteArray map (_.toChar) mkString ""}"))

  @tailrec
  private final def consumeRemainder(current: String | MoreThanOne): String | MoreThanOne =
    current match {
      case Left(e)  => Left(e)
      case Right(s) => RESP.stateOf(s.remainder) match {
        case Left(ee)  => Left(ee)
        case Right(rs) => rs match {
          case CompleteWithRemainder(c, r) =>
            consumeRemainder(Right(MoreThanOne(Complete(c) :: s.invertedComplete, r)))

          case CompleteVector | CompleteAndDecoded(_) =>
            Right(MoreThanOne(Complete(s.remainder) :: s.invertedComplete, BitVector.empty))

          case MissingBits(_) | IncompleteVector =>
            Right(MoreThanOne(s.invertedComplete, s.remainder))

        }
      }
    }
}

sealed trait NonEmptyFrame extends Product with Serializable
sealed trait CompleteFrame extends Product with Serializable

case object EmptyFrame extends RESPFrame
final case class Complete(full: BitVector) extends RESPFrame with NonEmptyFrame with CompleteFrame
final case class Decoded(resp: RESP) extends RESPFrame with NonEmptyFrame with CompleteFrame
final case class MoreThanOne(invertedComplete: List[Complete], remainder: BitVector) extends RESPFrame with NonEmptyFrame { self =>
  def complete: Vector[Complete] =
    invertedComplete.foldRight(Vector.empty[Complete])((c, v) => v :+ c)
}
final case class Incomplete(partial: BitVector, bitsToComplete: Long) extends RESPFrame with NonEmptyFrame {

  override def append(bytes: ByteBuffer): Exception | NonEmptyFrame = {

    val newBits = BitVector.view(bytes)

    //  Saves some size checks
    if (bitsToComplete > 0 && bitsToComplete == newBits.size) Right(Complete(partial ++ newBits))
    else nextFrame(partial ++ newBits)
  }
}
