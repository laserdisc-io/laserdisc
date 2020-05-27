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
}
