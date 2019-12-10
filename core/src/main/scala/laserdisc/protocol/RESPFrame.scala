package laserdisc
package protocol

import java.nio.ByteBuffer

import laserdisc.protocol.BitVectorDecoding._
import laserdisc.protocol.RESP.stateOf
import scodec.bits.BitVector

import scala.annotation.tailrec

sealed trait RESPFrame extends Product with Serializable with EitherSyntax with BitVectorSyntax {
  def append(bytes: ByteBuffer): Exception | NonEmptyRESPFrame = nextFrame(BitVector.view(bytes))
  protected final def nextFrame(bits: BitVector): Exception | NonEmptyRESPFrame =
    stateOf(bits)
      .flatMap {
        case MissingBits(n)              => IncompleteFrame(bits, n).asRight
        case Incomplete                  => IncompleteFrame(bits, 0L).asRight
        case Complete                    => CompleteFrame(bits).asRight
        case CompleteWithRemainder(c, r) => consumeRemainder(MoreThanOneFrame(CompleteFrame(c) :: Nil, r).asRight)
      }
      .leftMap(e => new Exception(s"Err building the frame: $e. Content: ${bits.tailToUtf8}"))

  @tailrec private[this] final def consumeRemainder(current: String | MoreThanOneFrame): String | MoreThanOneFrame = current match {
    case Right(s) =>
      stateOf(s.remainder) match {
        case Left(ee)                           => ee.asLeft
        case Right(CompleteWithRemainder(c, r)) => consumeRemainder(MoreThanOneFrame(CompleteFrame(c) :: s.invertedComplete, r).asRight)
        case Right(Complete)                    => MoreThanOneFrame(CompleteFrame(s.remainder) :: s.invertedComplete, BitVector.empty).asRight
        case _                                  => s.asRight
      }
    case left => left
  }
}

case object EmptyFrame                           extends RESPFrame
private[protocol] sealed trait NonEmptyRESPFrame extends RESPFrame

final case class CompleteFrame(bits: BitVector) extends NonEmptyRESPFrame
final case class MoreThanOneFrame(private[protocol] val invertedComplete: List[CompleteFrame], remainder: BitVector)
    extends NonEmptyRESPFrame {
  def complete: Vector[CompleteFrame] = invertedComplete.foldRight(Vector.empty[CompleteFrame])((c, v) => v :+ c)
}
final case class IncompleteFrame(partial: BitVector, bitsToComplete: Long) extends NonEmptyRESPFrame {
  override def append(bytes: ByteBuffer): Exception | NonEmptyRESPFrame = {
    val newBits = BitVector.view(bytes)
    //  Saves some size checks
    if (bitsToComplete > 0 && bitsToComplete == newBits.size) CompleteFrame(partial ++ newBits).asRight
    else nextFrame(partial ++ newBits)
  }
}
