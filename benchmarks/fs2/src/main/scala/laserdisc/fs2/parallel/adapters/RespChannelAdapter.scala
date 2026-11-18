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
package parallel
package adapters

import _root_.fs2.{Chunk, Pipe}
import _root_.fs2.interop.scodec.{StreamDecoder, StreamEncoder}
import cats.ApplicativeThrow
import laserdisc.protocol.*
import scodec.Codec
import scodec.bits.BitVector

private[parallel] object RespChannelAdapter {
  def send[F[_]: ApplicativeThrow](socketWrite: Chunk[Byte] => F[Unit]): Pipe[F, RESP, Unit] = {
    val streamEncoder = StreamEncoder.many(Codec[RESP])

    _.through(streamEncoder.encode[F]).chunks
      .evalMap(chunks => socketWrite(Chunk.array(chunks.foldLeft(BitVector.empty)(_ ++ _).toByteArray)))
  }

  def sendChunks[F[_]](socketWrite: Chunk[Byte] => F[Unit]): Pipe[F, Chunk[RESP], Unit] = {
    val encoder = Codec[RESP]

    _.map(chunk =>
      chunk.map { resp =>
        encoder.encode(resp).fold(_ => BitVector.empty, identity[BitVector])
      }
    ).evalMap(chunks => socketWrite(Chunk.array(chunks.foldLeft(BitVector.empty)(_ ++ _).toByteArray)))
  }

  def receive[F[_]: ApplicativeThrow]: Pipe[F, Byte, RESP] = {
    val streamDecoder = StreamDecoder.many(Codec[RESP])

    pipeIn => streamDecoder.decode(pipeIn.through(RedisChannel.impl.framing[F]).map(_.bits))
  }
}
