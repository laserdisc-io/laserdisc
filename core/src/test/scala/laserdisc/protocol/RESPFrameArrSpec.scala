package laserdisc
package protocol

import scodec.bits.BitVector

final class RESPFrameArrSpec extends BaseSpec {
  "An empty GenArr Frame" when {
    "appending a bit vector that represent an empty array" should {
      "produce a complete frame with the bits of an empty bulk" in {
        val inputVector = BitVector("*0\r\n".getBytes)
        EmptyFrame.append(inputVector.toByteBuffer) should be(Right(CompleteFrame(inputVector)))
      }
    }
  }

  "A non empty GenArr Frame" when {
    "appending a bit vector that completes it" should {
      "produce a complete frame with the correct bits" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("*1\r\n$16\r\nTest bulk str".getBytes), 0)
        val inputVector   = BitVector("ing\r\n".getBytes)
        val expected      = BitVector("*1\r\n$16\r\nTest bulk string\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(Right(CompleteFrame(expected)))
      }
    }

    "appending a bit vector that doesn't complete the array but has complete objects" should {
      "produce an incomplete frame with the correct partial and 0 as missing count" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
        val inputVector   = BitVector("ing\r\n:100\r\n".getBytes)
        val expected      = BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(Right(IncompleteFrame(expected, 0)))
      }
    }

    "appending a bit vector that completes the array" should {
      "produce a complete frame with the correct bits" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
        val inputVector   = BitVector("ing\r\n:100\r\n+A simple string\r\n".getBytes)
        val expected      = BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(Right(CompleteFrame(expected)))
      }
    }

    "appending a bit vector with more than one array" should {
      "produce more than one frame with a list of the complete ones and an empty remainder" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
        val inputVector   = BitVector("ing\r\n:100\r\n+A simple string\r\n*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector(
                CompleteFrame(BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)),
                CompleteFrame(BitVector("*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes))
              ),
              BitVector.empty
            )
          )
        )
      }
    }

    "appending a bit vector with more than one array plus a reminder" should {
      "produce more than one frame with a list of the complete ones and the correct remainder" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
        val inputVector   = BitVector("ing\r\n:100\r\n+A simple string\r\n*2\r\n$8\r\nAnother1\r\n-An error\r\n$17\r\nAnother bulk ".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector(
                CompleteFrame(BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)),
                CompleteFrame(BitVector("*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes))
              ),
              BitVector("$17\r\nAnother bulk ".getBytes())
            )
          )
        )
      }
    }

    "appending a bit vector with multiple null arrays" should {
      "produce more than one frame with a list of the complete ones and the correct remainder" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
        val inputVector   = BitVector("ing\r\n:100\r\n+A simple string\r\n*-1\r\n*-1\r\n*-1\r\n*-1\r\n*-1\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector(
                CompleteFrame(BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes))
              ),
              BitVector.empty
            )
          )
        )
      }
    }

    "appending a bit vector with multiple null arrays the last of which not complete" should {
      "produce more than one frame with a list of the complete ones and the correct remainder" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
        val inputVector   = BitVector("ing\r\n:100\r\n+A simple string\r\n*-1\r\n*-1\r\n*-1\r\n*-1\r\n*-1\r\n*".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer) should be(
          Right(
            MoreThanOneFrame(
              Vector(
                CompleteFrame(BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes))
              ),
              BitVector("*".getBytes())
            )
          )
        )
      }
    }

    "appending a bit vector with multiple arrays interleaved with null arrays" should {
      "produce as result of `complete` more than one frame with a list of the complete arrays in the correct order" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
        val inputVector =
          BitVector("ing\r\n:100\r\n+A simple string\r\n*-1\r\n*-1\r\n*2\r\n$8\r\nAnother1\r\n-An error\r\n*-1\r\n*-1\r\n*-1\r\n".getBytes)
        nonEmptyFrame
          .append(inputVector.toByteBuffer)
          .fold(
            err => fail(s"expected a result but failed with $err"), {
              case r @ MoreThanOneFrame(_, _) =>
                r.complete shouldBe Vector(
                  CompleteFrame(BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)),
                  CompleteFrame(BitVector("*-1\r\n".getBytes)),
                  CompleteFrame(BitVector("*-1\r\n".getBytes)),
                  CompleteFrame(BitVector("*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes)),
                  CompleteFrame(BitVector("*-1\r\n".getBytes)),
                  CompleteFrame(BitVector("*-1\r\n".getBytes)),
                  CompleteFrame(BitVector("*-1\r\n".getBytes))
                )
              case _ => fail(s"expected a MoreThanOne type")
            }
          )
      }
    }

    "appending a bit vector with multiple arrays containing nested arrays" should {
      "produce as result of `complete` more than one frame with a list of the complete arrays in the correct order" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
        val inputVector = BitVector(
          "ing\r\n:100\r\n+A simple string\r\n*-1\r\n*2\r\n$8\r\nAnother1\r\n-An error\r\n*3\r\n$8\r\nAnother1\r\n*3\r\n*2\r\n+Simple string\r\n*2\r\n$3\r\nfoo\r\n-an error\r\n:13\r\n:12\r\n-An error\r\n*-1\r\n".getBytes
        )
        nonEmptyFrame
          .append(inputVector.toByteBuffer)
          .fold(
            err => fail(s"expected a result but failed with $err"), {
              case r @ MoreThanOneFrame(_, _) =>
                r.complete shouldBe Vector(
                  CompleteFrame(BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)),
                  CompleteFrame(BitVector("*-1\r\n".getBytes)),
                  CompleteFrame(BitVector("*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes)),
                  CompleteFrame(
                    BitVector(
                      "*3\r\n$8\r\nAnother1\r\n*3\r\n*2\r\n+Simple string\r\n*2\r\n$3\r\nfoo\r\n-an error\r\n:13\r\n:12\r\n-An error\r\n".getBytes
                    )
                  ),
                  CompleteFrame(BitVector("*-1\r\n".getBytes))
                )
              case _ => fail(s"expected a MoreThanOne type")
            }
          )
      }
    }
  }
}
