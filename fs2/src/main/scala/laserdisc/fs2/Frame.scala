package laserdisc
package fs2

import java.nio.ByteBuffer

import cats.syntax.either._
import laserdisc.protocol.BitVectorDecoding.{CompleteVector, _}
import laserdisc.protocol.RESP
import scodec.bits.BitVector

import scala.annotation.tailrec

sealed trait Frame extends Product with Serializable {

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
      case Left(e)  => e.asLeft
      case Right(s) => RESP.stateOf(s.remainder) match {
        case Left(ee)  => ee.asLeft
        case Right(rs) => rs match {
          case CompleteWithRemainder(c, r) =>
            consumeRemainder(MoreThanOne(Complete(c) :: s.complete, r).asRight)

          case CompleteVector | CompleteAndDecoded(_) =>
            MoreThanOne(Complete(s.remainder) :: s.complete, BitVector.empty).asRight

          case MissingBits(_) | IncompleteVector =>
            MoreThanOne(s.complete, s.remainder).asRight

        }
      }
    }
}

sealed trait NonEmptyFrame extends Product with Serializable
sealed trait CompleteFrame extends Product with Serializable

case object EmptyFrame extends Frame
final case class Complete(full: BitVector) extends Frame with NonEmptyFrame with CompleteFrame
final case class Decoded(resp: RESP) extends Frame with NonEmptyFrame with CompleteFrame
final case class MoreThanOne(complete: List[Complete], remainder: BitVector) extends Frame with NonEmptyFrame
final case class Incomplete(partial: BitVector, bitsToComplete: Long) extends Frame with NonEmptyFrame {

  override def append(bytes: ByteBuffer): Exception | NonEmptyFrame = {

    val newBits = BitVector.view(bytes)

    //  Saves some size inspections
    if (bitsToComplete > 0 && bitsToComplete == newBits.size) Right(Complete(partial ++ newBits))
    else nextFrame(partial ++ newBits)
  }
}
