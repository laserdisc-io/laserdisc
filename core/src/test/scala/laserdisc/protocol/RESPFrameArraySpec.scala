package laserdisc.protocol

import org.scalatest.{Matchers, WordSpecLike}
import scodec.bits.BitVector

final class RESPFrameArraySpec extends WordSpecLike with Matchers {

  "A non empty Array Frame" when {

    "appending a bit vector that completes it" should {
      "produce Complete with all the bits" in {
        val nonEmptyFrame = Incomplete(BitVector("*1\r\n$16\r\nTest bulk str".getBytes), 0)
        val inputVector = BitVector("ing\r\n".getBytes)
        val expected = BitVector("*1\r\n$16\r\nTest bulk string\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(Right(Complete(expected)))
      }
    }
  }
}
