package laserdisc
package protocol

import BitVectorDecoding.{Complete, CompleteWithRemainder, Incomplete, MissingBits}
import scodec.bits.BitVector

final class RESPFunctionsSpec extends BaseSpec {
  "A RESP codec" when {
    "checking the state of a bit vector with the size prefix not complete" should {
      "produce IncompleteVector" in {
        RESP.stateOf(BitVector("$2362".getBytes)) should be(Right(Incomplete))
      }
    }

    "checking the state of a bit vector with only the data type selector" should {
      "produce IncompleteVector" in {
        RESP.stateOf(BitVector("$".getBytes)) should be(Right(Incomplete))
      }
    }

    "checking the state of a bit vector that's complete" should {
      "produce CompleteVector" in {
        RESP.stateOf(BitVector("$16\r\nTest bulk string\r\n".getBytes)) should be(Right(Complete))
      }
    }

    "checking the state of a bit vector whit the size prefix complete and an incomplete payload" should {
      "produce MissingBits with the correct number of bits missing" in {
        RESP.stateOf(BitVector("$40\r\nIncomplete test bulk string".getBytes)) should be(Right(MissingBits(120)))
      }
    }

    "checking the state of a bit vector that represents an empty bulk" should {
      "produce CompleteVector" in {
        RESP.stateOf(BitVector("$-1\r\n".getBytes)) should be(Right(Complete))
      }
    }

    "checking the state of an incomplete bit vector that represents an empty bulk" should {
      "produce MissingBits" in {
        RESP.stateOf(BitVector("$-".getBytes)) should be(Right(Incomplete))
      }
    }

    "checking the state of a bulk bit vector that contains one message complete and one not complete" should {
      "produce CompleteWithRemainder" in {
        RESP.stateOf(BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk".getBytes)) should be(
          Right(
            CompleteWithRemainder(
              BitVector("$16\r\nTest bulk string\r\n".toCharArray.map(_.toByte)),
              BitVector("$16\r\nTest bulk".toCharArray.map(_.toByte))
            )
          )
        )
      }
    }

    "checking the state of a bulk bit vector that contains more than one complete messages" should {
      "produce CompleteWithRemainder" in {
        RESP.stateOf(BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".getBytes)) should be(
          Right(
            CompleteWithRemainder(
              BitVector("$16\r\nTest bulk string\r\n".toCharArray.map(_.toByte)),
              BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".toCharArray.map(_.toByte))
            )
          )
        )
      }
    }

    "checking the state of a bulk bit vector that contains more than one null message" should {
      "produce CompleteWithRemainder" in {
        RESP.stateOf(BitVector("$-1\r\n$-1\r\n$-1\r\n".getBytes)) should be(
          Right(
            CompleteWithRemainder(
              BitVector("$-1\r\n".toCharArray.map(_.toByte)),
              BitVector("$-1\r\n$-1\r\n".toCharArray.map(_.toByte))
            )
          )
        )
      }
    }
  }
}
