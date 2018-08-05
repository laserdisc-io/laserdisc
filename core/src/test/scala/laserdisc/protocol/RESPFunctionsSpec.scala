package laserdisc.protocol

import laserdisc.protocol.BitVectorDecoding.{CompleteVector, IncompleteVector, MissingBits}
import org.scalatest.{Matchers, WordSpecLike}
import scodec.bits.BitVector

final class RESPFunctionsSpec extends WordSpecLike with Matchers {

  "A RESP codec" when {

    "checking the state of a bit vector with the size prefix not complete" should {
      "produce IncompleteVector" in {
        RESP.stateOf(BitVector.apply("$2362".getBytes)) should be (Right(IncompleteVector))
      }
    }

    "checking the state of a bit vector with only the data type selector" should {
      "produce IncompleteVector" in {
        RESP.stateOf(BitVector.apply("$".getBytes)) should be (Right(IncompleteVector))
      }
    }

    "checking the state of a bit vector that's complete" should {
      "produce CompleteVector" in {
        RESP.stateOf(BitVector.apply("$16\r\nTest bulk string\r\n".getBytes)) should be (Right(CompleteVector))
      }
    }

    "checking the state of a bit vector whit the size prefix complete and an incomplete payload" should {
      "produce MissingBits with the correct number of bits missing" in {
        RESP.stateOf(BitVector.apply("$40\r\nIncomplete test bulk string".getBytes)) should be (Right(MissingBits(120)))
      }
    }

    "checking the state of a bit vector that represents an empty bulk" should {
      "produce CompleteVector" in {
        RESP.stateOf(BitVector.apply("$-1\r\n".getBytes)) should be (Right(CompleteVector))
      }
    }

    "checking the state of an incomplete bit vector that represents an empty bulk" should {
      "produce MissingBits" in {
        RESP.stateOf(BitVector.apply("$-".getBytes)) should be (Right(IncompleteVector))
      }
    }
  }
}
