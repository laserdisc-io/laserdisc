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

import org.scalacheck.Prop.forAll
import scodec.bits.BitVector

final class RESPFrameMixedSpec extends RESPFrameFixture {

  test(
    "Appending to a non empty mixed frame a bit vector composed of a complete sequence of integers, simple strings, bulk strings and errors gives MoreThanOne with a list of all the complete items"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bulk str".getBytes), 0)
    val inputVector   = BitVector(
      "ing\r\n+OK\r\n$0\r\n\r\n+Another simple string\r\n*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n-Possible error message\r\n*0\r\n:1\r\n:2\r\n*2\r\n$8\r\nAnother1\r\n-An error\r\n:177\r\n+Another simple string\r\n$21\r\nTest bulk string 1 11\r\n*5\r\n$16\r\nTest bulk string\r\n:13\r\n-1234 An error with numbers\r\n:100\r\n+A simple string\r\n-And an error message\r\n".getBytes
    )
    nonEmptyFrame.append(inputVector) onRightAll {
      case r @ MoreThanOneFrame(_, _) =>
        assertEquals(
          r.complete,
          Vector(
            CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes())),
            CompleteFrame(BitVector("+OK\r\n".getBytes())),
            CompleteFrame(BitVector("$0\r\n\r\n".getBytes())),
            CompleteFrame(BitVector("+Another simple string\r\n".getBytes())),
            CompleteFrame(BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)),
            CompleteFrame(BitVector("-Possible error message\r\n".getBytes())),
            CompleteFrame(BitVector("*0\r\n".getBytes())),
            CompleteFrame(BitVector(":1\r\n".getBytes())),
            CompleteFrame(BitVector(":2\r\n".getBytes())),
            CompleteFrame(BitVector("*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes)),
            CompleteFrame(BitVector(":177\r\n".getBytes())),
            CompleteFrame(BitVector("+Another simple string\r\n".getBytes())),
            CompleteFrame(BitVector("$21\r\nTest bulk string 1 11\r\n".getBytes())),
            CompleteFrame(
              BitVector(
                "*5\r\n$16\r\nTest bulk string\r\n:13\r\n-1234 An error with numbers\r\n:100\r\n+A simple string\r\n".getBytes
              )
            ),
            CompleteFrame(BitVector("-And an error message\r\n".getBytes()))
          )
        )
      case _ => fail(s"expected a MoreThanOne type")
    }
  }

  test(
    "Appending to a non empty mixed frame a bit vector composed of sequence of integers, simple strings, bulk strings and errors that are not complete gives MoreThanOne with a list of all the complete items plus the remainder"
  ) {
    val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bulk str".getBytes), 0)
    val inputVector   = BitVector(
      "ing\r\n+OK\r\n+Another simple string\r\n-Possible error message\r\n:1\r\n:2\r\n:177\r\n+Another simple string\r\n$21\r\nTest bulk string 1 11\r\n-And an error message\r\n".getBytes
    )
    nonEmptyFrame.append(inputVector) onRightAll {
      case r @ MoreThanOneFrame(_, _) =>
        assertEquals(
          r.complete,
          Vector(
            CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes())),
            CompleteFrame(BitVector("+OK\r\n".getBytes())),
            CompleteFrame(BitVector("+Another simple string\r\n".getBytes())),
            CompleteFrame(BitVector("-Possible error message\r\n".getBytes())),
            CompleteFrame(BitVector(":1\r\n".getBytes())),
            CompleteFrame(BitVector(":2\r\n".getBytes())),
            CompleteFrame(BitVector(":177\r\n".getBytes())),
            CompleteFrame(BitVector("+Another simple string\r\n".getBytes())),
            CompleteFrame(BitVector("$21\r\nTest bulk string 1 11\r\n".getBytes())),
            CompleteFrame(BitVector("-And an error message\r\n".getBytes()))
          )
        )
      case _ => fail(s"expected a MoreThanOne type")
    }
  }

  property("Appending to an empty frame a random sequence of complete messages gives MoreThanOne with all the complete items") {
    forAll { testSet: OneOrMore[ProtocolEncoded] =>
      val vector = BitVector(testSet.value.map(_.encoded).mkString.getBytes)

      EmptyFrame.append(vector) onRightAll {
        case MoreThanOneFrame(complete, remainder) =>
          assertEquals(complete.size, testSet.value.size)
          assert(remainder.isEmpty)
        case CompleteFrame(_) => succeed
        case other            => fail(s"expected a MoreThanOne type. Was $other")
      }
    }
  }
}
