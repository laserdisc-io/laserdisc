package laserdisc
package protocol

import scodec.bits.BitVector

private[protocol] trait BitVectorSyntax {
  implicit def bitVectorSyntax(bv: BitVector): BitVectorSyntaxOps = new BitVectorSyntaxOps(bv)
}

final private[protocol] class BitVectorSyntaxOps(private val bv: BitVector) extends AnyVal {

  /** Tries to decode the last 48 bytes of the bit vector as UTF-8 text
    */
  def tailToUtf8: String = bv.takeRight(48 * 8).decodeUtf8.getOrElse("content is not UTF-8 encoded")

  /** Tries to decode the whole bit vector to UTF-8 text
    */
  def toUtf8: String = bv.decodeUtf8.getOrElse("content is not UTF-8 encoded")
}
