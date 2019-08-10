package laserdisc.protocol

import java.nio.charset.Charset

import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.bits.{BitVector, ByteVector}

final class LenientStringCodec(private[this] val charset: Charset) extends Codec[String] {
  override def sizeBound: SizeBound                    = SizeBound.unknown
  override def encode(str: String): Attempt[BitVector] = Attempt.successful(ByteVector.view(str.getBytes(charset)).bits)
  override def decode(bits: BitVector): Attempt[DecodeResult[String]] =
    Attempt.successful(DecodeResult(new String(bits.toByteArray, charset), BitVector.empty))
  override def toString: String = s"lenient-${charset.displayName}"
}
