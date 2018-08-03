package laserdisc.protocol

import laserdisc.protocol.BitVectorDecoding.IncompleteVector
import org.scalatest.{Matchers, WordSpecLike}
import scodec.bits.BitVector

final class RESPFunctionsSpec extends WordSpecLike with Matchers {

  "A RESP codec" when {

    "checking the next state of a bit vector" should {

      "handle the case when the expected size of a bulk is not yet complete" in {
        RESP.stateOf(BitVector.apply("$2362".getBytes)) should be (Right(IncompleteVector))
      }
    }
  }
}
