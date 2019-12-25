package laserdisc
package protocol

import scodec.bits.BitVector

final class RESPFrameNumSpec extends BaseSpec {
  "An empty Bulk Frame" when {

    "appending a bit vector with a number split in two chunks" should {
      "produce MoreThanOneFrame with a follow up partial bulk string" in {
        val incompleteFirstInput = BitVector(":2".getBytes)
        val secondInput          = BitVector(s"1${CRLF}$$18${CRLF}Test bulk".getBytes)

        EmptyFrame.append(incompleteFirstInput.toByteBuffer).flatMap(_.append(secondInput.toByteBuffer)) should be(
          Right(
            MoreThanOneFrame(
              Vector(CompleteFrame(BitVector(s":21${CRLF}".getBytes))),
              BitVector(s"$$18${CRLF}Test bulk".getBytes)
            )
          )
        )
      }
    }
  }
}
