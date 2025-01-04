package laserdisc
package fs2
package parallel
package adapters

import _root_.fs2.{Chunk, Pipe, Pull, Stream}
import cats.ApplicativeError
import laserdisc.protocol._
import scodec.bits.BitVector

private[parallel] object BitVectorChannelAdapter {
  def send[F[_]](socketWrite: Chunk[Byte] => F[Unit]): Pipe[F, BitVector, Unit] =
    _.chunks
      .evalMap(chunks => socketWrite(Chunk.array(chunks.foldLeft(BitVector.empty)(_ ++ _).toByteArray)))

  def receive[F[_]: ApplicativeError[*[_], Throwable]]: Pipe[F, Byte, BitVector] = {
    def framing: Pipe[F, Byte, CompleteFrame] = {
      def loopScan(bytesIn: Stream[F, Byte], previous: RESPFrame): Pull[F, CompleteFrame, Unit] =
        bytesIn.pull.uncons.flatMap {
          case Some((chunk, rest)) =>
            previous.append(chunk.toBitVector) match {
              case Left(ex)                    => Pull.raiseError(ex)
              case Right(frame: CompleteFrame) => Pull.output1(frame) >> loopScan(rest, EmptyFrame)
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

    _.through(framing).map(_.bits)
  }
}
