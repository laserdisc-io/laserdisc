package laserdisc
package fs2

import java.net.InetSocketAddress
import java.nio.channels.AsynchronousChannelGroup

import _root_.fs2._
import _root_.fs2.io.tcp
import cats.Applicative
import cats.effect.Effect
import cats.syntax.applicative._
import cats.syntax.apply._
import laserdisc.protocol.{Complete, CompleteRESPFrame, Decoded, EmptyFrame, Incomplete, MoreThanOne, RESPFrame}
import log.effect.LogWriter
import scodec.Codec
import scodec.stream.{decode, encode}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

object RedisConnection {
  private[this] final val streamDecoder = decode.many(Codec[RESP])
  private[this] final val streamEncoder = encode.many(Codec[RESP])

  final def apply[F[_]: Effect: LogWriter](
      address: InetSocketAddress,
      writeTimeout: Option[FiniteDuration] = None,
      readMaxBytes: Int = 256 * 1024
  )(
      implicit ev0: AsynchronousChannelGroup,
      ev1: ExecutionContext
  ): Pipe[F, RESP, RESP] =
    stream =>
      tcp.client(address).flatMap { socket =>
        val send    = stream.through(impl.send(socket.writes(writeTimeout)))
        val receive = socket.reads(readMaxBytes).through(impl.receive)

        send.drain.covaryOutput[RESP].onFinalize(socket.endOfInput).mergeHaltBoth(receive)
    }

  private[fs2] final object impl {

    def send[F[_]: Applicative](sink: Sink[F, Byte])(implicit log: LogWriter[F]): Sink[F, RESP] =
      _.evalMap(resp => log.debug(s"sending $resp") *> resp.pure)
        .through(streamEncoder.encode)
        .flatMap(bits => Stream.segment(Segment.array(bits.toByteArray)))
        .to(sink)

    def receive[F[_]: Effect](implicit log: LogWriter[F]): Pipe[F, Byte, RESP] = {

      def framing: Pipe[F, Byte, CompleteRESPFrame] = {

        def loopStream(stream: Stream[F, Byte], previous: RESPFrame): Pull[F, CompleteRESPFrame, Unit] =
          stream.pull.unconsChunk flatMap {

            case Some((chunk, rest)) =>
              previous.append(chunk.toByteBuffer) match {
                case Left(ex) => Pull.raiseError(ex)
                case Right(f) => f match {
                  case frame: CompleteRESPFrame =>
                    Pull.output1(frame) >> loopStream(rest, EmptyFrame)

                  case frame: MoreThanOne =>
                    Pull.outputChunk(Chunk.vector(frame.complete)) >> (
                      if (frame.remainder.isEmpty) loopStream(rest, EmptyFrame)
                      else loopStream(rest, Incomplete(frame.remainder, 0L))
                    )

                  case frame: Incomplete =>
                    loopStream(rest, frame)
                }
              }

            case _ => Pull.done
          }

        stream => loopStream(stream, EmptyFrame).stream
      }

      _.through(framing) flatMap {
        case Complete(v)      => streamDecoder.decode(v)
        case Decoded(s)       => Stream.emit(s)
      } evalMap (
        resp => log.debug(s"receiving $resp") *> resp.pure
      )
    }
    
  }
}
