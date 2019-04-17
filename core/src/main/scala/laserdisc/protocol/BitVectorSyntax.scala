package laserdisc
package protocol

import scodec.bits.BitVector

private[protocol] trait BitVectorSyntax {
  implicit def bitVectorSyntax(bv: BitVector) = new BitVectorSyntaxOps(bv)
}

final private[protocol] class BitVectorSyntaxOps(private val bv: BitVector) extends AnyVal {

  /**
    * Tries to decode the last `takeRight` bytes of the bit vector as UTF8 text
    * If not passed it defaults to 48 bytes
    */
  def tailToUtf8(takeRight: Long = 48L): String = {
    bv.takeRight(takeRight * 8).decodeUtf8 getOrElse "!! unable to represent the content as UTF8 string !!"
  }

  /**
    * Tries to decode the whole bit vector to UTF8 text
    */
  def toUtf8: String = {
    bv.decodeUtf8 getOrElse "!! unable to represent the content as UTF8 string !!"
  }
}
