/*
 * Copyright (c) 2018-2025 LaserDisc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package laserdisc
package protocol

import scodec.bits.BitVector

final class RESPFrameBulkSpec extends BaseSpec {

  test("Appending to an empty Bulk frame a bit vector bit vector that's complete gives Complete with all the bits") {
    val inputVector = BitVector("$16\r\nTest bulk string\r\n".getBytes)
    assertEquals(EmptyFrame.append(inputVector), CompleteFrame(inputVector))
  }

  test(
    "Appending to an empty Bulk frame a bit vector that's complete and includes unsafe characters (\\r\\n) gives Complete with all the bits"
  ) {
    val inputVector = BitVector("$16\r\nTest \n\r ! string\r\n".getBytes)
    assertEquals(EmptyFrame.append(inputVector), CompleteFrame(inputVector))
  }

  test("Appending to an empty Bulk frame an empty bulk bit vector gives Complete with an empty content") {
    val inputVector = BitVector("$0\r\n\r\n".getBytes)
    assertEquals(EmptyFrame.append(inputVector), CompleteFrame(inputVector))
  }

  test(
    "Appending to an empty Bulk frame a bit vector that's not complete gives Incomplete with the correct partial and the correct missing count"
  ) {
    val inputVector = BitVector("$16\r\nTest bulk string".getBytes)
    assertEquals(EmptyFrame.append(inputVector), IncompleteFrame(inputVector, 16))
  }

  test(
    "Appending to an empty Bulk frame a bit vector with a number split in two chunks gives MoreThanOneFrame with a follow up partial bulk string"
  ) {
    val incompleteFirstInput = BitVector(":2".getBytes)
    val secondInput          = BitVector(s"1$CRLF$$18${CRLF}Test bulk".getBytes)
    assertEquals(
      EmptyFrame.append(incompleteFirstInput).flatMap(_.append(secondInput)),
      MoreThanOneFrame(
        Vector(CompleteFrame(BitVector(s":21$CRLF".getBytes))),
        BitVector(s"$$18${CRLF}Test bulk".getBytes)
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple messages all complete gives MoreThanOne with a list of the complete ones and an empty remainder"
  ) {
    val inputVector = BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".getBytes)
    assertEquals(
      EmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(3)(CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
        BitVector.empty
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple messages with the last not complete gives MoreThanOne with a list of the complete ones and a remainder with the incomplete bits"
  ) {
    val inputVector = BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk".getBytes)
    assertEquals(
      EmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(2)(CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
        BitVector("$16\r\nTest bulk".getBytes())
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple null bulk all complete gives MoreThanOne with a list of the complete ones and an empty remainder"
  ) {
    val inputVector = BitVector("$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n".getBytes)
    assertEquals(
      EmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(6)(CompleteFrame(BitVector("$-1\r\n".getBytes()))),
        BitVector.empty
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple null bulk all complete gives MoreThanOne with a list of the complete ones and an empty remainder"
  ) {
    val inputVector = BitVector("$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n".getBytes)
    assertEquals(
      EmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(6)(CompleteFrame(BitVector("$-1\r\n".getBytes()))),
        BitVector.empty
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple null bulk with the last not complete gives MoreThanOne with a list of the complete ones and a remainder with the incomplete bits"
  ) {
    val inputVector = BitVector("$-1\r\n$-1\r\n$-1\r\n$-1\r\n$".getBytes)
    assertEquals(
      EmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(4)(CompleteFrame(BitVector("$-1\r\n".getBytes()))),
        BitVector("$".getBytes())
      )
    )
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple different messages with the last not complete gives MoreThanOne with a list of the complete ones in the inverted order and a remainder with the incomplete bits"
  ) {
    val inputVector = BitVector(
      "$18\r\nTest bulk string 1\r\n$18\r\nTest bulk string 2\r\n$18\r\nTest bulk string 3\r\n$18\r\nTest bulk string 4\r\n$18\r\nTest bulk".getBytes
    )
    assertEquals(
      EmptyFrame.append(inputVector),
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
  }

  test(
    "Appending to an empty Bulk frame a bit vector with multiple different messages with the last not complete gives MoreThanOne where the call to complete should give a vector with the complete ones in the original order"
  ) {
    val inputVector = BitVector(
      "$18\r\nTest bulk string 1\r\n$18\r\nTest bulk string 2\r\n$18\r\nTest bulk string 3\r\n$18\r\nTest bulk string 4\r\n$18\r\nTest bulk".getBytes
    )
    EmptyFrame.append(inputVector) onRightAll {
      case r @ MoreThanOneFrame(_, _) =>
        assertEquals(
          r.complete,
          Vector(
            CompleteFrame(BitVector("$18\r\nTest bulk string 1\r\n".getBytes())),
            CompleteFrame(BitVector("$18\r\nTest bulk string 2\r\n".getBytes())),
            CompleteFrame(BitVector("$18\r\nTest bulk string 3\r\n".getBytes())),
            CompleteFrame(BitVector("$18\r\nTest bulk string 4\r\n".getBytes()))
          )
        )
      case _ => fail(s"expected a MoreThanOne type")
    }
  }

  test("Appending to a non empty Bulk frame a bit vector that completes it gives Complete with all the bits") {
    val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bulk str".getBytes), 0)
    val inputVector   = BitVector("ing\r\n".getBytes)
    val expected      = BitVector("$16\r\nTest bulk string\r\n".getBytes)
    assertEquals(nonEmptyFrame.append(inputVector), CompleteFrame(expected))
  }

  test(
    "Appending to a non empty Bulk frame a bit vector that doesn't complete it gives Incomplete with the correct partial and the correct missing count"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bul".getBytes), 0)
    val inputVector   = BitVector("k str".getBytes)
    val expected      = BitVector("$16\r\nTest bulk str".getBytes)
    assertEquals(nonEmptyFrame.append(inputVector), IncompleteFrame(expected, 40))
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple messages all complete gives MoreThanOne with a list of the complete ones and an empty remainder"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bulk s".getBytes), 0)
    val inputVector   = BitVector("tring\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".getBytes)
    assertEquals(
      nonEmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(3)(CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
        BitVector.empty
      )
    )
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple messages with the last not complete gives MoreThanOne with a list of the complete ones and a remainder with the incomplete bits"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bulk s".getBytes), 0)
    val inputVector   = BitVector("tring\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk".getBytes)
    assertEquals(
      nonEmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(2)(CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes()))),
        BitVector("$16\r\nTest bulk".getBytes())
      )
    )
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple null bulk all complete gives MoreThanOne with a list of the complete ones and an empty remainder"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("$-".getBytes), 0)
    val inputVector   = BitVector("1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n$-1\r\n".getBytes)
    assertEquals(
      nonEmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(6)(CompleteFrame(BitVector("$-1\r\n".getBytes()))),
        BitVector.empty
      )
    )
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple null bulk with the last not complete gives MoreThanOne with a list of the complete ones and a remainder with the incomplete bits"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("$-".getBytes), 0)
    val inputVector   = BitVector("1\r\n$-1\r\n$-1\r\n$-1\r\n$".getBytes)
    assertEquals(
      nonEmptyFrame.append(inputVector),
      MoreThanOneFrame(
        Vector.fill(4)(CompleteFrame(BitVector("$-1\r\n".getBytes()))),
        BitVector("$".getBytes())
      )
    )
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple different messages with the last not complete gives MoreThanOne with a list of the complete ones in the inverted order and a remainder with the incomplete bits"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("$21\r\nTest bulk s".getBytes), 0)
    val inputVector = BitVector(
      "tring 1 11\r\n$17\r\nTest bulk string2\r\n$20\r\nTest bulk string 3 1\r\n$19\r\nTest bulk string 40\r\n$18\r\nTest bulk".getBytes
    )
    assertEquals(
      nonEmptyFrame.append(inputVector),
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
  }

  test(
    "Appending to a non empty Bulk frame a bit vector with multiple different messages with the last not complete gives MoreThanOne where the call to complete should give a vector with the complete ones in the original order"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("$21\r\nTest bulk s".getBytes), 0)
    val inputVector = BitVector(
      "tring 1 11\r\n$17\r\nTest bulk string2\r\n$20\r\nTest bulk string 3 1\r\n$19\r\nTest bulk string 40\r\n$18\r\nTest bulk".getBytes
    )
    nonEmptyFrame.append(inputVector) onRightAll {
      case r @ MoreThanOneFrame(_, _) =>
        assertEquals(
          r.complete,
          Vector(
            CompleteFrame(BitVector("$21\r\nTest bulk string 1 11\r\n".getBytes())),
            CompleteFrame(BitVector("$17\r\nTest bulk string2\r\n".getBytes())),
            CompleteFrame(BitVector("$20\r\nTest bulk string 3 1\r\n".getBytes())),
            CompleteFrame(BitVector("$19\r\nTest bulk string 40\r\n".getBytes()))
          )
        )
      case _ => fail(s"expected a MoreThanOne type")
    }
  }
}
