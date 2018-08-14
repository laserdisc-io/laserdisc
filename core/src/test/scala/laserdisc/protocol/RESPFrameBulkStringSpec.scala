package laserdisc.protocol

import org.scalatest.{Matchers, WordSpecLike}
import scodec.bits.BitVector

final class RESPFrameBulkStringSpec extends WordSpecLike with Matchers {

  "An empty BulkString Frame" when {

    "appending a bit vector that's complete" should {
      "produce Complete with all the bits" in {
        val inputVector = BitVector("$16\r\nTest bulk string\r\n".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(Right(Complete(inputVector)))
      }
    }

    "appending a bit vector that represent an empty bulk" should {
      "produce Complete with a empty content" in {
        val inputVector = BitVector("$0\r\n\r\n".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(Right(Complete(inputVector)))
      }
    }

    "appending a bit vector that's not complete" should {
      "produce Incomplete with the correct partial and the correct missing count" in {
        val inputVector = BitVector("$16\r\nTest bulk string".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(Right(Incomplete(inputVector, 16)))
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

      "produce MoreThanOne where the call to complete should give a vector with the complete ones in the original order" in {
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

  "A non empty BulkString Frame" when {

    "appending a bit vector that completes it" should {
      "produce Complete with all the bits" in {
        val nonEmptyFrame = Incomplete(BitVector("$16\r\nTest bulk str".getBytes), 0)
        val inputVector = BitVector("ing\r\n".getBytes)
        val expected = BitVector("$16\r\nTest bulk string\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(Right(Complete(expected)))
      }
    }

    "appending a bit vector that doesn't complete it" should {
      "produce Incomplete with the correct partial and the correct missing count" in {
        val nonEmptyFrame = Incomplete(BitVector("$16\r\nTest bul".getBytes), 0)
        val inputVector = BitVector("k str".getBytes)
        val expected = BitVector("$16\r\nTest bulk str".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(Right(Incomplete(expected, 40)))
      }
    }

    "appending a bit vector with multiple messages all complete" should {
      "produce MoreThanOne with a list of the complete ones and an empty remainder" in {
        val nonEmptyFrame = Incomplete(BitVector("$16\r\nTest bulk s".getBytes), 0)
        val inputVector = BitVector("tring\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right( MoreThanOne(
            List.fill(3)(Complete(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
            BitVector.empty
          ) )
        )
      }
    }

    "appending a bit vector with multiple messages with the last not complete" should {
      "produce MoreThanOne with a list of the complete ones and a remainder with the incomplete bits" in {
        val nonEmptyFrame = Incomplete(BitVector("$16\r\nTest bulk s".getBytes), 0)
        val inputVector = BitVector("tring\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right( MoreThanOne(
            List.fill(2)(Complete(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
            BitVector("$16\r\nTest bulk".getBytes())
          ) )
        )
      }
    }

    "appending a bit vector with multiple null bulk all complete" should {
      "produce MoreThanOne with a list of the complete ones and an empty remainder" in {
        val nonEmptyFrame = Incomplete(BitVector("$-".getBytes), 0)
        val inputVector = BitVector("1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right( MoreThanOne(
            List.fill(6)(Complete(BitVector("$-1\r\n".getBytes()))),
            BitVector.empty
          ) )
        )
      }
    }

    "appending a bit vector with multiple null bulk with the last not complete" should {
      "produce MoreThanOne with a list of the complete ones and a remainder with the incomplete bits" in {
        val nonEmptyFrame = Incomplete(BitVector("$-".getBytes), 0)
        val inputVector = BitVector("1\r\n$-1\r\n$-1\r\n$-1\r\n$".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right( MoreThanOne(
            List.fill(4)(Complete(BitVector("$-1\r\n".getBytes()))),
            BitVector("$".getBytes())
          ) )
        )
      }
    }

    "appending a bit vector with multiple different messages with the last not complete" should {

      "produce MoreThanOne with a list of the complete ones in the inverted order and a remainder with the incomplete bits" in {
        val nonEmptyFrame = Incomplete(BitVector("$21\r\nTest bulk s".getBytes), 0)
        val inputVector = BitVector("tring 1 11\r\n$17\r\nTest bulk string2\r\n$20\r\nTest bulk string 3 1\r\n$19\r\nTest bulk string 40\r\n$18\r\nTest bulk".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right( MoreThanOne(
            Complete(BitVector("$19\r\nTest bulk string 40\r\n".getBytes())) ::
              Complete(BitVector("$20\r\nTest bulk string 3 1\r\n".getBytes())) ::
              Complete(BitVector("$17\r\nTest bulk string2\r\n".getBytes())) ::
              Complete(BitVector("$21\r\nTest bulk string 1 11\r\n".getBytes())) :: Nil,
            BitVector("$18\r\nTest bulk".getBytes())
          ) )
        )
      }

      "produce MoreThanOne where the call to complete should give a vector with the complete ones in the original order" in {
        val nonEmptyFrame = Incomplete(BitVector("$21\r\nTest bulk s".getBytes), 0)
        val inputVector = BitVector("tring 1 11\r\n$17\r\nTest bulk string2\r\n$20\r\nTest bulk string 3 1\r\n$19\r\nTest bulk string 40\r\n$18\r\nTest bulk".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer).fold(
          err => fail(s"expected a result but failed with $err"),
          {
            case r@MoreThanOne(_, _) => r.complete shouldBe Vector(
              Complete(BitVector("$21\r\nTest bulk string 1 11\r\n".getBytes())),
              Complete(BitVector("$17\r\nTest bulk string2\r\n".getBytes())),
              Complete(BitVector("$20\r\nTest bulk string 3 1\r\n".getBytes())),
              Complete(BitVector("$19\r\nTest bulk string 40\r\n".getBytes()))
            )
            case _ => fail(s"expected a MoreThanOne type")
          }
        )
      }
    }
  }
}
