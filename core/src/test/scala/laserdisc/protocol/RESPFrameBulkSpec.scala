package laserdisc
package protocol

import scodec.bits.BitVector

final class RESPFrameBulkSpec extends BaseSpec {
  "An empty Bulk Frame" when {
    "appending a bit vector that's complete" should {
      "produce Complete with all the bits" in {
        val inputVector = BitVector("$16\r\nTest bulk string\r\n".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(Right(CompleteFrame(inputVector)))
      }
    }

    "appending a bit vector that's complete and includes unsafe characters (\\r\\n)" should {
      "produce Complete with all the bits" in {
        val inputVector = BitVector("$16\r\nTest \n\r ! string\r\n".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(Right(CompleteFrame(inputVector)))
      }
    }

    "appending a bit vector that represent an empty bulk" should {
      "produce Complete with a empty content" in {
        val inputVector = BitVector("$0\r\n\r\n".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(Right(CompleteFrame(inputVector)))
      }
    }

    "appending a bit vector that's not complete" should {
      "produce Incomplete with the correct partial and the correct missing count" in {
        val inputVector = BitVector("$16\r\nTest bulk string".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(Right(IncompleteFrame(inputVector, 16)))
      }
    }

    "appending a bit vector with multiple messages all complete" should {
      "produce MoreThanOne with a list of the complete ones and an empty remainder" in {
        val inputVector = BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector.fill(3)(CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
              BitVector.empty
            )
          )
        )
      }
    }

    "appending a bit vector with multiple messages with the last not complete" should {
      "produce MoreThanOne with a list of the complete ones and a remainder with the incomplete bits" in {
        val inputVector = BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector.fill(2)(CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
              BitVector("$16\r\nTest bulk".getBytes())
            )
          )
        )
      }
    }

    "appending a bit vector with multiple null bulk all complete" should {
      "produce MoreThanOne with a list of the complete ones and an empty remainder" in {
        val inputVector = BitVector("$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector.fill(6)(CompleteFrame(BitVector("$-1\r\n".getBytes()))),
              BitVector.empty
            )
          )
        )
      }
    }

    "appending a bit vector with multiple null bulk with the last not complete" should {
      "produce MoreThanOne with a list of the complete ones and a remainder with the incomplete bits" in {
        val inputVector = BitVector("$-1\r\n$-1\r\n$-1\r\n$-1\r\n$".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector.fill(4)(CompleteFrame(BitVector("$-1\r\n".getBytes()))),
              BitVector("$".getBytes())
            )
          )
        )
      }
    }

    "appending a bit vector with multiple different messages with the last not complete" should {
      "produce MoreThanOne with a list of the complete ones in the inverted order and a remainder with the incomplete bits" in {
        val inputVector = BitVector(
          "$18\r\nTest bulk string 1\r\n$18\r\nTest bulk string 2\r\n$18\r\nTest bulk string 3\r\n$18\r\nTest bulk string 4\r\n$18\r\nTest bulk".getBytes
        )
        EmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector(
                CompleteFrame(BitVector("$18\r\nTest bulk string 1\r\n".getBytes())),
                CompleteFrame(BitVector("$18\r\nTest bulk string 2\r\n".getBytes())),
                CompleteFrame(BitVector("$18\r\nTest bulk string 3\r\n".getBytes())),
                CompleteFrame(BitVector("$18\r\nTest bulk string 4\r\n".getBytes()))
              ),
              BitVector("$18\r\nTest bulk".getBytes())
            )
          )
        )
      }

      "produce MoreThanOne where the call to complete should give a vector with the complete ones in the original order" in {
        val inputVector = BitVector(
          "$18\r\nTest bulk string 1\r\n$18\r\nTest bulk string 2\r\n$18\r\nTest bulk string 3\r\n$18\r\nTest bulk string 4\r\n$18\r\nTest bulk".getBytes
        )
        EmptyFrame.append(inputVector.toByteBuffer) onRight {
          case r @ MoreThanOneFrame(_, _) =>
            r.complete shouldBe Vector(
              CompleteFrame(BitVector("$18\r\nTest bulk string 1\r\n".getBytes())),
              CompleteFrame(BitVector("$18\r\nTest bulk string 2\r\n".getBytes())),
              CompleteFrame(BitVector("$18\r\nTest bulk string 3\r\n".getBytes())),
              CompleteFrame(BitVector("$18\r\nTest bulk string 4\r\n".getBytes()))
            )
          case _ => fail(s"expected a MoreThanOne type")
        }
      }
    }
  }

  "A non empty Bulk Frame" when {
    "appending a bit vector that completes it" should {
      "produce Complete with all the bits" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bulk str".getBytes), 0)
        val inputVector   = BitVector("ing\r\n".getBytes)
        val expected      = BitVector("$16\r\nTest bulk string\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(Right(CompleteFrame(expected)))
      }
    }

    "appending a bit vector that doesn't complete it" should {
      "produce Incomplete with the correct partial and the correct missing count" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bul".getBytes), 0)
        val inputVector   = BitVector("k str".getBytes)
        val expected      = BitVector("$16\r\nTest bulk str".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(Right(IncompleteFrame(expected, 40)))
      }
    }

    "appending a bit vector with multiple messages all complete" should {
      "produce MoreThanOne with a list of the complete ones and an empty remainder" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bulk s".getBytes), 0)
        val inputVector   = BitVector("tring\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector.fill(3)(CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
              BitVector.empty
            )
          )
        )
      }
    }

    "appending a bit vector with multiple messages with the last not complete" should {
      "produce MoreThanOne with a list of the complete ones and a remainder with the incomplete bits" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bulk s".getBytes), 0)
        val inputVector   = BitVector("tring\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector.fill(2)(CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
              BitVector("$16\r\nTest bulk".getBytes())
            )
          )
        )
      }
    }

    "appending a bit vector with multiple null bulk all complete" should {
      "produce MoreThanOne with a list of the complete ones and an empty remainder" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("$-".getBytes), 0)
        val inputVector   = BitVector("1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector.fill(6)(CompleteFrame(BitVector("$-1\r\n".getBytes()))),
              BitVector.empty
            )
          )
        )
      }
    }

    "appending a bit vector with multiple null bulk with the last not complete" should {
      "produce MoreThanOne with a list of the complete ones and a remainder with the incomplete bits" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("$-".getBytes), 0)
        val inputVector   = BitVector("1\r\n$-1\r\n$-1\r\n$-1\r\n$".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector.fill(4)(CompleteFrame(BitVector("$-1\r\n".getBytes()))),
              BitVector("$".getBytes())
            )
          )
        )
      }
    }

    "appending a bit vector with multiple different messages with the last not complete" should {
      "produce MoreThanOne with a list of the complete ones in the inverted order and a remainder with the incomplete bits" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("$21\r\nTest bulk s".getBytes), 0)
        val inputVector = BitVector(
          "tring 1 11\r\n$17\r\nTest bulk string2\r\n$20\r\nTest bulk string 3 1\r\n$19\r\nTest bulk string 40\r\n$18\r\nTest bulk".getBytes
        )
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector(
                CompleteFrame(BitVector("$21\r\nTest bulk string 1 11\r\n".getBytes())),
                CompleteFrame(BitVector("$17\r\nTest bulk string2\r\n".getBytes())),
                CompleteFrame(BitVector("$20\r\nTest bulk string 3 1\r\n".getBytes())),
                CompleteFrame(BitVector("$19\r\nTest bulk string 40\r\n".getBytes()))
              ),
              BitVector("$18\r\nTest bulk".getBytes())
            )
          )
        )
      }

      "produce MoreThanOne where the call to complete should give a vector with the complete ones in the original order" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("$21\r\nTest bulk s".getBytes), 0)
        val inputVector = BitVector(
          "tring 1 11\r\n$17\r\nTest bulk string2\r\n$20\r\nTest bulk string 3 1\r\n$19\r\nTest bulk string 40\r\n$18\r\nTest bulk".getBytes
        )
        nonEmptyFrame.append(inputVector.toByteBuffer) onRight {
          case r @ MoreThanOneFrame(_, _) =>
            r.complete shouldBe Vector(
              CompleteFrame(BitVector("$21\r\nTest bulk string 1 11\r\n".getBytes())),
              CompleteFrame(BitVector("$17\r\nTest bulk string2\r\n".getBytes())),
              CompleteFrame(BitVector("$20\r\nTest bulk string 3 1\r\n".getBytes())),
              CompleteFrame(BitVector("$19\r\nTest bulk string 40\r\n".getBytes()))
            )
          case _ => fail(s"expected a MoreThanOne type")
        }
      }
    }
  }
}
