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
package fs2

import _root_.fs2.Chunk
import cats.effect.IO
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets.UTF_8

final class RedisChannelSpec extends CatsEffectSuite {
  // what's read from the socket, chunk by chunk
  private def received(chunks: String*): IO[List[RESP]] =
    Stream
      .emits(chunks.toList)
      .flatMap(chunk => Stream.chunk(Chunk.array(chunk.getBytes(UTF_8))))
      .covary[IO]
      .through(RedisChannel.impl.receiveResp[IO])
      .compile
      .toList

  test("a chunk with one reply") {
    received("+a\r\n").assertEquals(List(Str("a")))
  }

  test("a chunk with several replies") {
    received("+a\r\n+b\r\n").assertEquals(List(Str("a"), Str("b")))
  }

  test("a chunk with a reply and part of the next") {
    received("+a\r\n+b", "\r\n").assertEquals(List(Str("a"), Str("b")))
  }

  test("a reply over several chunks") {
    received("$5\r\nhe", "llo", "\r\n").assertEquals(List(Bulk("hello")))
  }

  test("what isn't a reply fails") {
    received("?\r\n").attempt.map(outcome => assert(outcome.isLeft, outcome.toString))
  }
}
