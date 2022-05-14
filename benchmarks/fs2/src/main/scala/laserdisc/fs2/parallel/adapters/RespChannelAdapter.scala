package laserdisc
package fs2
package parallel
package adapters

import _root_.fs2.{Chunk, Pipe, Pull, Stream}
import cats.ApplicativeError
import laserdisc.protocol._
import scodec.Codec
import scodec.bits.BitVector
import scodec.stream.{StreamDecoder, StreamEncoder}

private[parallel] object RespChannelAdapter {
  def send[F[_]: ApplicativeError[*[_], Throwable]](socketWrite: Chunk[Byte] => F[Unit]): Pipe[F, RESP, Unit] = {
    val streamEncoder = StreamEncoder.many(Codec[RESP])

    _.through(streamEncoder.encode[F]).chunks
      .evalMap(chunks => socketWrite(Chunk.array(chunks.foldLeft(BitVector.empty)(_ ++ _).toByteArray)))
  }

  def sendChunks[F[_]: ApplicativeError[*[_], Throwable]](socketWrite: Chunk[Byte] => F[Unit]): Pipe[F, Chunk[RESP], Unit] = {
    val encoder = Codec[RESP]

    _.map(chunk =>
      chunk.map { resp =>
        encoder.encode(resp).fold(_ => BitVector.empty, identity[BitVector])
      }
    ).evalMap(chunks => socketWrite(Chunk.array(chunks.foldLeft(BitVector.empty)(_ ++ _).toByteArray)))
  }

  def receive[F[_]: ApplicativeError[*[_], Throwable]]: Pipe[F, Byte, RESP] = {
    val streamDecoder = StreamDecoder.many(Codec[RESP])

    def framing: Pipe[F, Byte, CompleteFrame] = {
      def loopScan(bytesIn: Stream[F, Byte], previous: RESPFrame): Pull[F, CompleteFrame, Unit] =
        bytesIn.pull.uncons.flatMap {
          case Some(chunk, rest) =>
            previous.append(chunk.toBitVector) match {
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

    pipeIn =>
      streamDecoder
        .decode(pipeIn.through(framing) map (_.bits))
  }
}
