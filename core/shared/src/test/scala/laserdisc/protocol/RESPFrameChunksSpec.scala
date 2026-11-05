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

import scodec.bits.{BitVector, ByteVector}

final class RESPFrameChunksSpec extends BaseSpec {
  private def bytes(s: String): ByteVector = ByteVector(s.getBytes("UTF-8"))

  // the complete frames of a reply arriving in chunks of this size, as RedisChannel reads them
  private def frames(reply: ByteVector, chunkSize: Long): Vector[BitVector] =
    reply
      .grouped(chunkSize)
      .foldLeft[(RESPFrame, Vector[BitVector])](EmptyFrame -> Vector.empty) { case ((frame, complete), chunk) =>
        frame.append(chunk.bits) match {
          case Right(CompleteFrame(bits))    => EmptyFrame -> (complete :+ bits)
          case Right(more: MoreThanOneFrame) =>
            val next = if (more.remainder.isEmpty) EmptyFrame else IncompleteFrame(more.remainder, 0L)()
            next -> (complete ++ more.complete.map(_.bits))
          case Right(incomplete: IncompleteFrame) => incomplete -> complete
          case Left(e)                            => fail(s"unexpected $e")
        }
      }
      ._2

  private val replies = List(
    "+OK\r\n",
    "-ERR nope\r\n",
    ":42\r\n",
    "$5\r\nhello\r\n",
    "$0\r\n\r\n",
    "$-1\r\n",
    "$4\r\n\r\n\r\n\r\n",
    "*-1\r\n",
    "*0\r\n",
    "*2\r\n$3\r\nfoo\r\n$3\r\nbar\r\n",
    "*3\r\n:1\r\n$-1\r\n+OK\r\n",
    "*2\r\n*2\r\n:1\r\n:2\r\n*0\r\n",
    "*2\r\n*1\r\n*1\r\n$1\r\na\r\n*-1\r\n",
    "*1\r\n*1\r\n*1\r\n*1\r\n:1\r\n"
  )

  test("a reply is found whatever size of chunks it arrives in") {
    replies.foreach { reply =>
      (1L to reply.length.toLong).foreach(size =>
        assertEquals(frames(bytes(reply), size), Vector(bytes(reply).bits), s"$reply in chunks of $size")
      )
    }
  }

  test("replies one after the other are told apart whatever size of chunks they arrive in") {
    val all = bytes(replies.mkString)
    (1L to all.size).foreach(size => assertEquals(frames(all, size), replies.map(bytes(_).bits).toVector, s"in chunks of $size"))
  }

  test("an array isn't complete when what arrives only completes one of its elements") {
    val frame = EmptyFrame.append(bytes("*2\r\n$3\r\nfo").bits).flatMap(_.append(bytes("o\r\n").bits))
    assertEquals(frame.map(_.isInstanceOf[IncompleteFrame]), true)
    assertEquals(
      frame.flatMap(_.append(bytes("$1\r\na\r\n").bits)),
      CompleteFrame(bytes("*2\r\n$3\r\nfoo\r\n$1\r\na\r\n").bits)
    )
  }

  test("a size that isn't a number, or that's below -1, is an error, in an array as well") {
    List("$a\r\n", "*a\r\n", "$-2\r\n", "*-2\r\n", "*1\r\n$a\r\n", "*1\r\n*-2\r\n").foreach { reply =>
      assert(EmptyFrame.append(bytes(reply).bits).isLeft, reply)
    }
  }

  test("an unknown type is an error, in an array as well") {
    List("?\r\n", "*1\r\n?\r\n").foreach(reply => assert(EmptyFrame.append(bytes(reply).bits).isLeft, reply))
  }
}
