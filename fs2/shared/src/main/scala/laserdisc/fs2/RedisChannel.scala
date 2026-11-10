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

import _root_.fs2.{Chunk, Pull}
import _root_.fs2.interop.scodec.{StreamDecoder, StreamEncoder}
import _root_.fs2.io.net.Socket
import cats.MonadThrow
import cats.effect.syntax.all.*
import cats.effect.{Resource, Temporal}
import cats.syntax.all.*
import com.comcast.ip4s.{Host, SocketAddress}
import laserdisc.protocol.*
import log.effect.fs2.LogSelector
import scodec.Codec
import scodec.bits.BitVector

import scala.concurrent.duration.*

object RedisChannel {
  private final val streamDecoder = StreamDecoder.many(Codec[RESP])
  private final val streamEncoder = StreamEncoder.many(Codec[RESP])

  private[fs2] final def apply[F[_]: Network: LogSelector: Temporal](
      address: SocketAddress[Host],
      receiveBufferSizeBytes: Int,
      connectTimeout: FiniteDuration,
      onConnected: F[Unit]
  ): Pipe[F, RESP, RESP] =
    stream =>
      Stream.resource(connectedSocket(address, receiveBufferSizeBytes, connectTimeout)).evalTap(_ => onConnected).flatMap { socket =>
        val send    = stream.through(impl.send(socket.write))
        val receive = socket.reads.through(impl.receiveResp)

        send.drain
          .covaryOutput[RESP]
          .mergeHaltBoth(receive)
          .onFinalizeWeak(socket.endOfOutput)
      }

  /** How long connecting waits, unless told otherwise: an address that drops what's sent to it is otherwise waited on for as long as the
    * system allows, which can be minutes
    */
  final val DefaultConnectTimeout: FiniteDuration = 10.seconds

  private[fs2] final def connectedSocket[F[_]: Network: Temporal](
      address: SocketAddress[Host],
      receiveBufferSizeBytes: Int,
      connectTimeout: FiniteDuration = DefaultConnectTimeout
  ): Resource[F, Socket[F]] =
    withTimeout(
      Network[F].connect(address, PlatformDependent.socketOptions(receiveBufferSizeBytes)),
      connectTimeout,
      ConnectTimedOut(address, connectTimeout)
    )

  // acquiring the resource fails with the error once the time is up, and what it acquired by then is released
  private[fs2] final def withTimeout[F[_]: Temporal, A](
      resource: Resource[F, A],
      after: FiniteDuration,
      error: =>Throwable
  ): Resource[F, A] =
    Resource.applyFull { poll =>
      poll(resource.allocated.timeoutTo(after, Temporal[F].raiseError[(A, F[Unit])](error))).map { case (a, release) =>
        a -> ((_: Resource.ExitCase) => release)
      }
    }

  private[fs2] object impl {
    // what the trace log says of a command or a reply: never its values, which can be passwords or the application's data
    def described(resp: RESP): String = resp match {
      case Arr(Bulk(command) :: arguments) => s"$command with ${arguments.size} argument(s)"
      case Arr(elements)                   => s"an array of ${elements.size}"
      case bulk: Bulk                      => s"a bulk string of ${bulk.bytes.size} bytes"
      case NullBulk                        => "a null bulk string"
      case NilArr                          => "a null array"
      case Num(_)                          => "an integer"
      case Str(_)                          => "a simple string"
      case Err(message)                    => s"the error $message"
    }

    def send[F[_]: MonadThrow](socketWrite: Chunk[Byte] => F[Unit])(
        implicit logSelector: LogSelector[F]
    ): Pipe[F, RESP, Unit] =
      _.evalTap(resp => logSelector.log.trace(s"sending ${described(resp)}"))
        .through(streamEncoder.encode[F])
        .chunks
        .evalMap(chunks => socketWrite(Chunk.array(chunks.foldLeft(BitVector.empty)(_ ++ _).toByteArray)))

    def receiveResp[F[_]: MonadThrow](implicit logSelector: LogSelector[F]): Pipe[F, Byte, RESP] = {
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

      pipeIn =>
        streamDecoder
          .decode(pipeIn.through(framing).map(_.bits))
          .evalTap(resp => logSelector.log.trace(s"receiving ${described(resp)}"))
    }
  }
}
