package laserdisc
package protocol

import scodec.bits.BitVector

private[protocol] trait BitVectorSyntax {
  implicit def bitVectorSyntax(bv: BitVector) = new BitVectorSyntaxOps(bv)
}

final private[protocol] class BitVectorSyntaxOps(private val bv: BitVector) extends AnyVal {

  def print: String = {
    val printedSize = 16L * 8
    bv.takeRight(printedSize).decodeUtf8 getOrElse "!! unable to represent the content as UTF8 string !!"
  }
}