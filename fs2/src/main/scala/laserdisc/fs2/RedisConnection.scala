package laserdisc
package fs2

import java.net.InetSocketAddress
import java.nio.channels.AsynchronousChannelGroup

import _root_.fs2._
import _root_.fs2.io.tcp.Socket
import cats.{Applicative, FlatMap}
import cats.effect.{ConcurrentEffect, ContextShift, Effect}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import laserdisc.protocol._
import log.effect.LogWriter
import scodec.Codec
import scodec.stream.{decode, encode}

import scala.concurrent.duration.FiniteDuration

object RedisConnection {
  private[this] final val streamDecoder = decode.many(Codec[RESP])
  private[this] final val streamEncoder = encode.many(Codec[RESP])

  final def apply[F[_]: ConcurrentEffect: ContextShift: LogWriter](
      address: InetSocketAddress,
      writeTimeout: Option[FiniteDuration] = None,
      readMaxBytes: Int = 256 * 1024
  )(
      implicit ev0: AsynchronousChannelGroup
  ): Pipe[F, RESP, RESP] =
    stream =>
      Stream.resource(Socket.client(address)).flatMap { socket =>
        val send    = stream.through(impl.send(socket.writes(writeTimeout)))
        val receive = socket.reads(readMaxBytes).through(impl.receive)

        send.drain.covaryOutput[RESP].onFinalize(socket.endOfInput).mergeHaltBoth(receive)
    }

  private[fs2] final object impl {

    def send[F[_]: Applicative: FlatMap](sink: Sink[F, Byte])(implicit log: LogWriter[F]): Sink[F, RESP] =
      _.evalMap(resp => log.debug(s"sending $resp") >> resp.pure)
        .through(streamEncoder.encode)
        .flatMap(bits => Stream.chunk(Chunk.array(bits.toByteArray)))
        .to(sink)

    def receive[F[_]: Effect](implicit log: LogWriter[F]): Pipe[F, Byte, RESP] = {

      def framing: Pipe[F, Byte, CompleteFrame] = {

        def loopStream(stream: Stream[F, Byte], previous: RESPFrame): Pull[F, CompleteFrame, Unit] =
          stream.pull.uncons.flatMap {
            case Some((chunk, rest)) =>
              previous.append(chunk.toByteBuffer) match {
                case Left(ex)                    => Pull.raiseError(ex)
                case Right(frame: CompleteFrame) => Pull.output1(frame) >> loopStream(rest, EmptyFrame)
                case Right(frame: MoreThanOneFrame) =>
                  Pull.output(Chunk.vector(frame.complete)) >> {
                    if (frame.remainder.isEmpty) loopStream(rest, EmptyFrame)
                    else loopStream(rest, IncompleteFrame(frame.remainder, 0L))
                  }
                case Right(frame: IncompleteFrame) => loopStream(rest, frame)
              }

            case _ => Pull.done
          }

        stream =>
          loopStream(stream, EmptyFrame).stream
      }

      _.through(framing)
        .flatMap(complete => streamDecoder.decode(complete.bits))
        .evalMap(resp => log.debug(s"receiving $resp") >> resp.pure)
    }

  }
}
