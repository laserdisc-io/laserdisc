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

import _root_.fs2.{Chunk, Pipe, Pull, Stream}
import cats.ApplicativeThrow
import laserdisc.protocol.*
import scodec.bits.BitVector

private[parallel] object BitVectorChannelAdapter {
  def send[F[_]](socketWrite: Chunk[Byte] => F[Unit]): Pipe[F, BitVector, Unit] =
    _.chunks
      .evalMap(chunks => socketWrite(Chunk.array(chunks.foldLeft(BitVector.empty)(_ ++ _).toByteArray)))

  def receive[F[_]: ApplicativeThrow]: Pipe[F, Byte, BitVector] = {
    def framing: Pipe[F, Byte, CompleteFrame] = {
      def loopScan(bytesIn: Stream[F, Byte], previous: RESPFrame): Pull[F, CompleteFrame, Unit] =
        bytesIn.pull.uncons.flatMap {
          case Some((chunk, rest)) =>
            previous.append(chunk.toBitVector) match {
              case Left(ex)                       => Pull.raiseError(ex)
              case Right(frame: CompleteFrame)    => Pull.output1(frame) >> loopScan(rest, EmptyFrame)
              case Right(frame: MoreThanOneFrame) =>
                Pull.output(Chunk.from(frame.complete)) >> {
                  if (frame.remainder.isEmpty) loopScan(rest, EmptyFrame)
                  else loopScan(rest, IncompleteFrame(frame.remainder, 0L)())
                }
              case Right(frame: IncompleteFrame) => loopScan(rest, frame)
            }

          case _ => Pull.done
        }

      bytesIn => loopScan(bytesIn, EmptyFrame).stream
    }

    _.through(framing).map(_.bits)
  }
}
