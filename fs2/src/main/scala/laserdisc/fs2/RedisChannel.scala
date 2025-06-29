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
package fs2

import _root_.fs2.{Chunk, Pull}
import _root_.fs2.io.net.Socket
import cats.MonadError
import cats.effect.{Concurrent, Resource}
import cats.syntax.flatMap._
import com.comcast.ip4s.{Host, SocketAddress}
import laserdisc.protocol._
import log.effect.fs2.LogSelector
import scodec.Codec
import scodec.bits.BitVector
import scodec.stream.{StreamDecoder, StreamEncoder}

object RedisChannel {
  private[this] final val streamDecoder = StreamDecoder.many(Codec[RESP])
  private[this] final val streamEncoder = StreamEncoder.many(Codec[RESP])

  private[fs2] final def apply[F[_]: Network: LogSelector: Concurrent](
      address: SocketAddress[Host],
      receiveBufferSizeBytes: Int
  ): Pipe[F, RESP, RESP] =
    stream =>
      Stream.resource(connectedSocket(address, receiveBufferSizeBytes)) >>= { socket =>
        val send    = stream.through(impl.send(socket.write))
        val receive = socket.reads.through(impl.receiveResp)

        send.drain
          .covaryOutput[RESP]
          .mergeHaltBoth(receive)
          .onFinalizeWeak(socket.endOfOutput)
      }

  private[fs2] final def connectedSocket[F[_]: Network](
      address: SocketAddress[Host],
      receiveBufferSizeBytes: Int
  ): Resource[F, Socket[F]] =
    Network[F].client(address, PlatformDependent.socketOptions(receiveBufferSizeBytes))

  private[this] final object impl {
    def send[F[_]: MonadError[*[_], Throwable]](socketWrite: Chunk[Byte] => F[Unit])(
        implicit logSelector: LogSelector[F]
    ): Pipe[F, RESP, Unit] =
      _.evalTap(resp => logSelector.log.trace(s"sending $resp"))
        .through(streamEncoder.encode[F])
        .chunks
        .evalMap(chunks => socketWrite(Chunk.array(chunks.foldLeft(BitVector.empty)(_ ++ _).toByteArray)))

    def receiveResp[F[_]: MonadError[*[_], Throwable]](implicit logSelector: LogSelector[F]): Pipe[F, Byte, RESP] = {
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
                    else loopScan(rest, IncompleteFrame(frame.remainder, 0L))
                  }
                case Right(frame: IncompleteFrame) => loopScan(rest, frame)
              }

            case _ => Pull.done
          }

        bytesIn => loopScan(bytesIn, EmptyFrame).stream
      }

      pipeIn =>
        streamDecoder
          .decode(pipeIn.through(framing) map (_.bits))
          .evalTap(resp => logSelector.log.trace(s"receiving $resp"))
    }
  }
}
