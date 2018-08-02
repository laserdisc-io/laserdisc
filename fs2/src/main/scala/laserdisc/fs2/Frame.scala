package laserdisc
package fs2

import _root_.fs2.Chunk
import cats.syntax.either._
import laserdisc.protocol.BitVectorDecoding._
import laserdisc.protocol.RESP
import scodec.bits.BitVector

sealed trait Frame extends Product with Serializable {

  def append(chunk: Chunk[Byte]): Exception | NonEmptyFrame =
    nextFrame(BitVector.view(chunk.toByteBuffer))

  protected final def nextFrame(bits: BitVector): Exception | NonEmptyFrame =
    RESP.stateOf(bits) map {
      case MissingBits(n)        => Incomplete(bits, n)
      case CompleteAndDecoded(r) => Decoded(r.value)
      case IncompleteVector      => Incomplete(bits, 0L)
      case CompleteVector        => Complete(bits)
    } leftMap (new Exception(_))
}

sealed trait NonEmptyFrame extends Product with Serializable

case object EmptyFrame extends Frame
final case class Complete(full: BitVector) extends Frame with NonEmptyFrame
final case class Decoded(resp: RESP) extends Frame with NonEmptyFrame
final case class Incomplete(partial: BitVector, bitsToComplete: Long) extends Frame with NonEmptyFrame {

  override def append(chunk: Chunk[Byte]): Exception | NonEmptyFrame = {

    val newBits = BitVector.view(chunk.toByteBuffer)

    //  Saves some size inspections
    if (bitsToComplete > 0 && bitsToComplete == newBits.size) Right(Complete(partial ++ newBits))
    else nextFrame(partial ++ newBits)
  }
}
