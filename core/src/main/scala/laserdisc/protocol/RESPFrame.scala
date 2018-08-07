package laserdisc
package protocol

import java.nio.ByteBuffer

import laserdisc.protocol.BitVectorDecoding._
import scodec.bits.BitVector

import scala.annotation.tailrec

sealed trait RESPFrame extends Product with Serializable with EitherSyntax with BitVectorSyntax {

  def append(bytes: ByteBuffer): Exception | NonEmptyRESPFrame =
    nextFrame(BitVector.view(bytes))

  protected final def nextFrame(bits: BitVector): Exception | NonEmptyRESPFrame =
    RESP.stateOf(bits) flatMap {
      case MissingBits(n)              => Incomplete(bits, n).asRight
      case CompleteAndDecoded(r)       => Decoded(r.value).asRight
      case IncompleteVector            => Incomplete(bits, 0L).asRight
      case CompleteVector              => Complete(bits).asRight
      case CompleteWithRemainder(c, r) => consumeRemainder(MoreThanOne(Complete(c) :: Nil, r).asRight)
    } leftMap (e => new Exception(s"Error: $e. Content: ${bits.print}"))

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

sealed trait NonEmptyRESPFrame extends Product with Serializable
sealed trait CompleteRESPFrame extends Product with Serializable

case object EmptyFrame extends RESPFrame
final case class Complete(full: BitVector) extends RESPFrame with NonEmptyRESPFrame with CompleteRESPFrame
final case class Decoded(resp: RESP) extends RESPFrame with NonEmptyRESPFrame with CompleteRESPFrame
final case class MoreThanOne(invertedComplete: List[Complete], remainder: BitVector) extends RESPFrame with NonEmptyRESPFrame { self =>
  def complete: Vector[Complete] =
    invertedComplete.foldRight(Vector.empty[Complete])((c, v) => v :+ c)
}
final case class Incomplete(partial: BitVector, bitsToComplete: Long) extends RESPFrame with NonEmptyRESPFrame {

  override def append(bytes: ByteBuffer): Exception | NonEmptyRESPFrame = {

    val newBits = BitVector.view(bytes)

    //  Saves some size checks
    if (bitsToComplete > 0 && bitsToComplete == newBits.size) Right(Complete(partial ++ newBits))
    else nextFrame(partial ++ newBits)
  }
}
