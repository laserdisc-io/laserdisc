package laserdisc
package fs2

import java.net.InetSocketAddress
import java.nio.channels.AsynchronousChannelGroup

import cats.Applicative
import cats.effect.Effect
import cats.syntax.all._
import _root_.fs2.{Pipe, Pull, Segment, Sink, Stream}
import _root_.fs2.io.tcp
import scodec.Codec
import scodec.bits.BitVector
import scodec.stream.{decode, encode}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

object RedisConnection {
  private[this] final val streamDecoder = decode.many(Codec[RESP])
  private[this] final val streamEncoder = encode.many(Codec[RESP])

  final def apply[F[_]: Effect: Logger](
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
    def send[F[_]: Applicative](sink: Sink[F, Byte])(implicit log: Logger[F]): Sink[F, RESP] =
      _.evalMap(resp => log.debug(s"sending $resp") *> resp.pure)
        .through(streamEncoder.encode)
        .flatMap(bits => Stream.segment(Segment.array(bits.toByteArray)))
        .to(sink)

    def receive[F[_]: Effect](implicit log: Logger[F]): Pipe[F, Byte, RESP] = {
      def toBitVector: Pipe[F, Byte, BitVector] = {
        def go(stream: Stream[F, Byte]): Pull[F, BitVector, Unit] = stream.pull.unconsChunk.flatMap {
          case Some((chunk, rest)) => Pull.output1(BitVector.view(chunk.toByteBuffer)) >> go(rest)
          case _                   => Pull.done
        }

        stream =>
          go(stream).stream
      }

      _.through(toBitVector)
        .flatMap(streamDecoder.decode(_))
        .evalMap(resp => log.debug(s"receiving $resp") *> resp.pure)
    }
  }

}
