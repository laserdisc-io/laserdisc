package laserdisc
package protocol

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import scodec.bits.BitVector

final class RESPFrameArrSpec extends BaseSpec with RESPFrameFixture {

  test("Appending an empty array bit vector to an empty GenArr frame gives a complete frame with the bits of an empty bulk") {
    val inputVector = BitVector("*0\r\n".getBytes)
    assertEquals(EmptyFrame.append(inputVector), CompleteFrame(inputVector))
  }

  test(
    "Appending a bit vector that contains only the size of a non empty array to an empty GenArr frame gives an incomplete frame with the bits of the size"
  ) {
    val inputVector = BitVector("*32\r\n".getBytes)
    assertEquals(EmptyFrame.append(inputVector), IncompleteFrame(inputVector, 0))
  }

  test("Appending to a non empty GenArr frame a bit vector that completes it gives a correct complete frame") {
    val nonEmptyFrame = IncompleteFrame(BitVector("*1\r\n$16\r\nTest bulk str".getBytes), 0)
    val inputVector   = BitVector("ing\r\n".getBytes)
    val expected      = BitVector("*1\r\n$16\r\nTest bulk string\r\n".getBytes)
    assertEquals(nonEmptyFrame.append(inputVector), CompleteFrame(expected))
  }

  test(
    "Appending to a non empty GenArr frame a bit vector that doesn't complete it but has complete results gives an incomplete frame with the correct partial and 0 as missing count"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
    val inputVector   = BitVector("ing\r\n:100\r\n".getBytes)
    val expected      = BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n".getBytes)
    assertEquals(nonEmptyFrame.append(inputVector), IncompleteFrame(expected, 0))
  }

  test("Appending to a non empty GenArr frame a bit vector that completes the array gives a correct complete frame") {
    val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
    val inputVector   = BitVector("ing\r\n:100\r\n+A simple string\r\n".getBytes)
    val expected      = BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)
    assertEquals(nonEmptyFrame.append(inputVector), CompleteFrame(expected))
  }

  test(
    "Appending to a non empty GenArr frame a bit vector with more than one array gives more than one frame with a list of the complete ones and an empty remainder"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
    val inputVector   = BitVector("ing\r\n:100\r\n+A simple string\r\n*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes)
    assertEquals(
      nonEmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector(
          CompleteFrame(BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)),
          CompleteFrame(BitVector("*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes))
        ),
        BitVector.empty
      )
    )
  }

  test(
    "Appending to a non empty GenArr frame a bit vector with more than one array plus a reminder gives more than one frame with a list of the complete ones and the correct remainder"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
    val inputVector   = BitVector("ing\r\n:100\r\n+A simple string\r\n*2\r\n$8\r\nAnother1\r\n-An error\r\n$17\r\nAnother bulk ".getBytes)
    assertEquals(
      nonEmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector(
          CompleteFrame(BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)),
          CompleteFrame(BitVector("*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes))
        ),
        BitVector("$17\r\nAnother bulk ".getBytes())
      )
    )
  }

  test(
    "Appending to a non empty GenArr frame a bit vector with multiple null arrays gives more than one frame with a list of the complete ones and the correct remainder"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
    val inputVector   = BitVector("ing\r\n:100\r\n+A simple string\r\n*-1\r\n*-1\r\n*-1\r\n*-1\r\n*-1\r\n".getBytes)
    assertEquals(
      nonEmptyFrame.append(inputVector),
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
  }

  test(
    "Appending to a non empty GenArr frame a bit vector with multiple null arrays the last of which not complete gives more than one frame with a list of the complete ones and the correct remainder"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
    val inputVector   = BitVector("ing\r\n:100\r\n+A simple string\r\n*-1\r\n*-1\r\n*-1\r\n*-1\r\n*-1\r\n*".getBytes)
    assertEquals(
      nonEmptyFrame.append(inputVector),
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
  }

  test(
    "Appending to a non empty GenArr frame a bit vector with multiple arrays interleaved with null arrays gives as `complete` more than one frame with a list of the complete ones in the right order"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
    val inputVector = BitVector(
      "ing\r\n:100\r\n+A simple string\r\n*-1\r\n*-1\r\n*2\r\n$8\r\nAnother1\r\n-An error\r\n*-1\r\n*-1\r\n*-1\r\n".getBytes
    )
    nonEmptyFrame
      .append(inputVector)
      .fold(
        err => fail(s"expected a result but failed with $err"),
        {
          case r @ MoreThanOneFrame(_, _) =>
            assertEquals(
              r.complete,
              Vector(
                CompleteFrame(BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes)),
                CompleteFrame(BitVector("*-1\r\n".getBytes))
              )
            )
          case _ => fail(s"expected a MoreThanOne type")
        }
      )
  }

  test(
    "Appending to a non empty GenArr frame a bit vector with multiple arrays containing nested arrays gives as `complete` more than one frame with a list of the complete ones in the correct order"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("*3\r\n$16\r\nTest bulk str".getBytes), 0)
    val inputVector = BitVector(
      "ing\r\n:100\r\n+A simple string\r\n*-1\r\n*2\r\n$8\r\nAnother1\r\n-An error\r\n*3\r\n$8\r\nAnother1\r\n*3\r\n*2\r\n+Simple string\r\n*2\r\n$3\r\nfoo\r\n-an error\r\n:13\r\n:12\r\n-An error\r\n*-1\r\n".getBytes
    )
    nonEmptyFrame
      .append(inputVector)
      .fold(
        err => fail(s"expected a result but failed with $err"),
        {
          case r @ MoreThanOneFrame(_, _) =>
            assertEquals(
              r.complete,
              Vector(
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
            )
          case _ => fail(s"expected a MoreThanOne type")
        }
      )
  }

  property("Appending multi-level arrays in chunks of different size to a empty GenArr frame gives the exact encoding") {
    val chunkSize: Gen[Int] =
      Gen.frequency(
        8 -> Gen.choose(128, 1024),
        2 -> Gen.choose(1025, 4096)
      )

    forAll(chunkSize) { chunkSize: Int =>
      val inputChunks = groupInChunks(bytesOf(arrFiveLevelsList), chunkSize)

      val frames = appendChunks(inputChunks).map(f => new String(f.bits.toByteArray))

      assertEquals(frames.head, NullArrEncoded().encoded)
      assertEquals(frames.drop(1).head, StrEncoded("OK").encoded)
      assertEquals(frames.drop(2).head, ArrEncoded(arrThreeLevelsList).encoded)
      assertEquals(frames.drop(3).head, NumEncoded(21).encoded)
      assertEquals(frames.drop(4).head, ArrEncoded(mixedNoArrList).encoded)
      assertEquals(frames.drop(5).head, StrEncoded("").encoded)
      assertEquals(frames.drop(6).head, ArrEncoded(arrOneLevelList).encoded)
      assertEquals(frames.drop(7).head, NumEncoded(Long.MaxValue).encoded)
      assertEquals(frames.drop(8).head, ArrEncoded(arrFourLevelsList).encoded)
      assertEquals(frames.drop(9).head, StrEncoded(shortStr).encoded)
      assertEquals(frames.drop(10).head, ArrEncoded(arrTwoLevelsList).encoded)
      assertEquals(frames.drop(11).head, StrEncoded("PONG").encoded)
    }
  }
}
