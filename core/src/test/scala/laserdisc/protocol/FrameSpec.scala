package laserdisc.protocol

import org.scalatest.{Matchers, WordSpecLike}
import scodec.bits.BitVector

final class FrameSpec extends WordSpecLike with Matchers {

  "An empty Frame" when {

    "appending a bit vector that's complete" should {
      "produce Complete with all the bits" in {
        val inputVector = BitVector("$16\r\nTest bulk string\r\n".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(Right(Complete(inputVector)))
      }
    }

    "appending a bit vector that's not complete" should {
      "produce Incomplete with the correct partial and the correct missing count" in {
        val inputVector = BitVector("$16\r\nTest bulk str".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(Right(Incomplete(inputVector, 40)))
      }
    }

    "appending a bit vector with multiple messages all complete" should {
      "produce MoreThanOne with a list of the complete ones and an empty remainder" in {
        val inputVector = BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(
          Right( MoreThanOne(
            List.fill(3)(Complete(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
            BitVector.empty
          ) )
        )
      }
    }

    "appending a bit vector with multiple messages with the last not complete" should {
      "produce MoreThanOne with a list of the complete ones and a remainder with the incomplete bits" in {
        val inputVector = BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(
          Right( MoreThanOne(
            List.fill(2)(Complete(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
            BitVector("$16\r\nTest bulk".getBytes())
          ) )
        )
      }
    }

    "appending a bit vector with multiple null bulk all complete" should {
      "produce MoreThanOne with a list of the complete ones and an empty remainder" in {
        val inputVector = BitVector("$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(
          Right( MoreThanOne(
            List.fill(6)(Complete(BitVector("$-1\r\n".getBytes()))),
            BitVector.empty
          ) )
        )
      }
    }

    "appending a bit vector with multiple null bulk with the last not complete" should {
      "produce MoreThanOne with a list of the complete ones and a remainder with the incomplete bits" in {
        val inputVector = BitVector("$-1\r\n$-1\r\n$-1\r\n$-1\r\n$".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(
          Right( MoreThanOne(
            List.fill(4)(Complete(BitVector("$-1\r\n".getBytes()))),
            BitVector("$".getBytes())
          ) )
        )
      }
    }

    "appending a bit vector with multiple different messages with the last not complete" should {

      "produce MoreThanOne with a list of the complete ones in the inverted order and a remainder with the incomplete bits" in {
        val inputVector = BitVector("$18\r\nTest bulk string 1\r\n$18\r\nTest bulk string 2\r\n$18\r\nTest bulk string 3\r\n$18\r\nTest bulk string 4\r\n$18\r\nTest bulk".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(
          Right( MoreThanOne(
            Complete(BitVector("$18\r\nTest bulk string 4\r\n".getBytes())) ::
              Complete(BitVector("$18\r\nTest bulk string 3\r\n".getBytes())) ::
              Complete(BitVector("$18\r\nTest bulk string 2\r\n".getBytes())) ::
              Complete(BitVector("$18\r\nTest bulk string 1\r\n".getBytes())) :: Nil,
            BitVector("$18\r\nTest bulk".getBytes())
          ) )
        )
      }

      "produce MoreThanOne where the call to invertedComplete should give complete ones in the original order" in {
        val inputVector = BitVector("$18\r\nTest bulk string 1\r\n$18\r\nTest bulk string 2\r\n$18\r\nTest bulk string 3\r\n$18\r\nTest bulk string 4\r\n$18\r\nTest bulk".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer).fold(
          err => fail(s"expected a result but failed with $err"),
          {
            case r@MoreThanOne(_, _) => r.complete shouldBe Vector(
                Complete(BitVector("$18\r\nTest bulk string 1\r\n".getBytes())),
                Complete(BitVector("$18\r\nTest bulk string 2\r\n".getBytes())),
                Complete(BitVector("$18\r\nTest bulk string 3\r\n".getBytes())),
                Complete(BitVector("$18\r\nTest bulk string 4\r\n".getBytes()))
              )
            case _ => fail(s"expected a MoreThanOne type")
          }
        )
      }
    }

  }
}
