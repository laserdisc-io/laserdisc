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
      case MissingBits(n)              => IncompleteFrame(bits, n).asRight
      case Incomplete                  => IncompleteFrame(bits, 0L).asRight
      case Complete                    => CompleteFrame(bits).asRight
      case CompleteWithRemainder(c, r) => consumeRemainder(MoreThanOneFrame(CompleteFrame(c) :: Nil, r).asRight)
    } leftMap (e => new Exception(s"Error building the frame: $e. Content: ${bits.tailToUtf8()}"))

  @tailrec
  private[this] final def consumeRemainder(current: String | MoreThanOneFrame): String | MoreThanOneFrame =
    current match {
      case Left(e) => e.asLeft
      case Right(s) =>
        RESP.stateOf(s.remainder) match {
          case Left(ee) => ee.asLeft
          case Right(rs) =>
            rs match {

              case CompleteWithRemainder(c, r) =>
                consumeRemainder(
                  MoreThanOneFrame(CompleteFrame(c) :: s.invertedComplete, r).asRight
                )

              case Complete =>
                MoreThanOneFrame(CompleteFrame(s.remainder) :: s.invertedComplete, BitVector.empty).asRight

              case MissingBits(_) | Incomplete =>
                s.asRight
            }
        }
    }
}

private[protocol] sealed trait NonEmptyRESPFrame extends Product with Serializable

case object EmptyFrame extends RESPFrame

final case class CompleteFrame(bits: BitVector) extends RESPFrame with NonEmptyRESPFrame
final case class MoreThanOneFrame(private[protocol] val invertedComplete: List[CompleteFrame], remainder: BitVector)
    extends RESPFrame
    with NonEmptyRESPFrame {
  def complete: Vector[CompleteFrame] =
    invertedComplete.foldRight(Vector.empty[CompleteFrame])((c, v) => v :+ c)
}
final case class IncompleteFrame(partial: BitVector, bitsToComplete: Long) extends RESPFrame with NonEmptyRESPFrame {

  override def append(bytes: ByteBuffer): Exception | NonEmptyRESPFrame = {

    val newBits = BitVector.view(bytes)

    //  Saves some size checks
    if (bitsToComplete > 0 && bitsToComplete == newBits.size) CompleteFrame(partial ++ newBits).asRight
    else nextFrame(partial ++ newBits)
  }
}
