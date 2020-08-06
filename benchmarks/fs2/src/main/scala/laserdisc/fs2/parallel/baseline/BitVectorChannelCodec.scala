package laserdisc
package fs2
package parallel
package baseline

import _root_.fs2.{Chunk, Pipe, Pull, Stream}
import cats.MonadError
import laserdisc.protocol._
import scodec.bits.BitVector

private[fs2] object BitVectorChannelCodec {
  def send[F[_]: MonadError[*[_], Throwable]](socketChannel: Pipe[F, Byte, Unit]): Pipe[F, BitVector, Unit] =
    _.flatMap(bits => Stream.chunk(Chunk.bytes(bits.toByteArray)))
      .through(socketChannel)

  def receive[F[_]: MonadError[*[_], Throwable]]: Pipe[F, Byte, BitVector] = {
    def framing: Pipe[F, Byte, CompleteFrame] = {
      def loopScan(bytesIn: Stream[F, Byte], previous: RESPFrame): Pull[F, CompleteFrame, Unit] =
        bytesIn.pull.uncons.flatMap {
          case Some((chunk, rest)) =>
            previous.append(chunk.toByteBuffer) match {
              case Left(ex)                    => Pull.raiseError(ex)
              case Right(frame: CompleteFrame) => Pull.output1(frame) >> loopScan(rest, EmptyFrame)
              case Right(frame: MoreThanOneFrame) =>
                Pull.output(Chunk.vector(frame.complete)) >> {
                  if (frame.remainder.isEmpty) loopScan(rest, EmptyFrame)
                  else loopScan(rest, IncompleteFrame(frame.remainder, 0L))
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
