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
      case IncompleteVector            => Incomplete(bits, 0L).asRight
      case CompleteVector              => Complete(bits).asRight
      case CompleteWithRemainder(c, r) => consumeRemainder(MoreThanOne(Complete(c) :: Nil, r).asRight)
    } leftMap (e => new Exception(s"Error building the frame: $e. Content: ${bits.print}"))

  @tailrec
  private final def consumeRemainder(current: String | MoreThanOne): String | MoreThanOne =
    current match {
      case Left(e)  => e.asLeft
      case Right(s) => RESP.stateOf(s.remainder) match {
        case Left(ee)  => ee.asLeft
        case Right(rs) => rs match {

          case CompleteWithRemainder(c, r) =>
            consumeRemainder(MoreThanOne(Complete(c) :: s.invertedComplete, r).asRight)

          case CompleteVector =>
            MoreThanOne(Complete(s.remainder) :: s.invertedComplete, BitVector.empty).asRight

          case MissingBits(_) | IncompleteVector =>
            s.asRight
        }
      }
    }
}

sealed trait NonEmptyRESPFrame extends Product with Serializable

case object EmptyFrame extends RESPFrame

final case class Complete(full: BitVector) extends RESPFrame with NonEmptyRESPFrame
final case class MoreThanOne(private[protocol] val invertedComplete: List[Complete], remainder: BitVector) extends RESPFrame with NonEmptyRESPFrame {
  def complete: Vector[Complete] =
    invertedComplete.foldRight(Vector.empty[Complete])((c, v) => v :+ c)
}
final case class Incomplete(partial: BitVector, bitsToComplete: Long) extends RESPFrame with NonEmptyRESPFrame {

  override def append(bytes: ByteBuffer): Exception | NonEmptyRESPFrame = {

    val newBits = BitVector.view(bytes)

    //  Saves some size checks
    if (bitsToComplete > 0 && bitsToComplete == newBits.size) Complete(partial ++ newBits).asRight
    else nextFrame(partial ++ newBits)
  }
}