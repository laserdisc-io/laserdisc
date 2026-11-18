/*
 * Copyright (c) 2018-2026 LaserDisc
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

import laserdisc.protocol.BitVectorDecoding.{Complete, CompleteWithRemainder, Incomplete, MissingBits}
import scodec.bits.BitVector

final class RESPFunctionsSpec extends BaseSpec {

  test("A RESP codec checking the state of a bit vector with the size prefix not complete gives IncompleteVector") {
    assertEquals(RESP.stateOf(BitVector("$2362".getBytes)), Incomplete)
  }

  test("A RESP codec checking the state of a bit vector with only the data type selector gives IncompleteVector") {
    assertEquals(RESP.stateOf(BitVector("$".getBytes)), Incomplete)
  }

  test("A RESP codec checking the state of a bit vector that's complete gives CompleteVector") {
    assertEquals(RESP.stateOf(BitVector("$16\r\nTest bulk string\r\n".getBytes)), Complete)
  }

  test(
    "A RESP codec checking the state of a bit vector with the size prefix complete and an incomplete payload gives MissingBits with the correct number of bits missing"
  ) {
    assertEquals(RESP.stateOf(BitVector("$40\r\nIncomplete test bulk string".getBytes)), MissingBits(120))
  }

  test("A RESP codec checking the state of a bit vector that represents an empty bulk gives CompleteVector") {
    assertEquals(RESP.stateOf(BitVector("$-1\r\n".getBytes)), Complete)
  }

  test("A RESP codec checking the state of an incomplete bit vector that represents an empty bulk gives MissingBits") {
    assertEquals(RESP.stateOf(BitVector("$-".getBytes)), Incomplete)
  }

  test(
    "A RESP codec checking the state of a bulk bit vector that contains one message complete and one not complete gives CompleteWithRemainder"
  ) {
    assertEquals(
      RESP.stateOf(BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk".getBytes)),
      CompleteWithRemainder(
        BitVector("$16\r\nTest bulk string\r\n".toCharArray.map(_.toByte)),
        BitVector("$16\r\nTest bulk".toCharArray.map(_.toByte))
      )
    )
  }

  test("A RESP codec checking the state of a bulk bit vector that contains more than one complete messages gives CompleteWithRemainder") {
    assertEquals(
      RESP.stateOf(BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".getBytes)),
      CompleteWithRemainder(
        BitVector("$16\r\nTest bulk string\r\n".toCharArray.map(_.toByte)),
        BitVector("$16\r\nTest bulk string\r\n$16\r\nTest bulk string\r\n".toCharArray.map(_.toByte))
      )
    )
  }

  test("A RESP codec checking the state of a bulk bit vector that contains more than one null message gives CompleteWithRemainder") {
    assertEquals(
      RESP.stateOf(BitVector("$-1\r\n$-1\r\n$-1\r\n".getBytes)),
      CompleteWithRemainder(
        BitVector("$-1\r\n".toCharArray.map(_.toByte)),
        BitVector("$-1\r\n$-1\r\n".toCharArray.map(_.toByte))
      )
    )
  }

  test("A bulk string or an array declaring more than 2147483647 bytes or elements is refused, as its end can't be worked out") {
    List("$9223372036854775784\r\n", "$2147483648\r\n", "*9223372036854775807\r\n:1\r\n", "*2147483648\r\n").foreach { reply =>
      val bits = BitVector(reply.getBytes)
      assert(RESP.stateOf(bits).isLeft, s"$reply: ${RESP.stateOf(bits)}")
      assert(EmptyFrame.append(bits).isLeft, s"$reply: ${EmptyFrame.append(bits)}")
      assert(RESP.respCodec.decode(bits).isFailure, s"$reply: ${RESP.respCodec.decode(bits)}")
    }
    assertEquals(RESP.stateOf(BitVector("$2147483647\r\n".getBytes)), MissingBits((2147483647L + 2L) * 8L))
  }

  test("A reply nesting arrays more than 128 deep is refused, before decoding it can run out of stack") {
    def nested(depth: Int) = BitVector(("*1\r\n" * depth + ":1\r\n").getBytes)
    assertEquals(RESP.stateOf(nested(128)), Complete)
    assert(RESP.respCodec.decode(nested(128)).isSuccessful)
    List(129, 100000).foreach { depth =>
      assert(RESP.stateOf(nested(depth)).isLeft, s"$depth")
      assert(EmptyFrame.append(nested(depth)).isLeft, s"$depth")
      val (first, second) = nested(depth).splitAt(nested(depth).size / 2)
      assert(EmptyFrame.append(first).flatMap(_.append(second)).isLeft, s"$depth in two pieces")
    }
    val wide = BitVector(("*200\r\n" + "*1\r\n:1\r\n" * 200).getBytes)
    assertEquals(RESP.stateOf(wide), Complete)
  }
}
