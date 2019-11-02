package laserdisc
package fs2

import java.net.InetSocketAddress

import _root_.fs2._
import _root_.fs2.io.tcp.{Socket, SocketGroup}
import cats.MonadError
import cats.effect.{Blocker, Concurrent, ContextShift, Resource}
import cats.syntax.flatMap._
import laserdisc.protocol._
import log.effect.LogWriter
import scodec.Codec
import scodec.stream.{StreamDecoder, StreamEncoder}

import scala.concurrent.duration.FiniteDuration

object RedisChannel {
  private[this] final val streamDecoder = StreamDecoder.many(Codec[RESP])
  private[this] final val streamEncoder = StreamEncoder.many(Codec[RESP])

  final def apply[F[_]: Concurrent: ContextShift: LogWriter](
      address: InetSocketAddress,
      writeTimeout: Option[FiniteDuration] = None,
      readMaxBytes: Int = 256 * 1024
  )(blocker: Blocker): Pipe[F, RESP, RESP] = {
    def connectedSocket: Resource[F, Socket[F]] =
      SocketGroup(blocker) >>= (_.client(address))

    stream =>
      Stream.resource(connectedSocket) >>= { socket =>
        val send    = stream.through(impl.send(socket.writes(writeTimeout)))
        val receive = socket.reads(readMaxBytes).through(impl.receive)

        send.drain
          .covaryOutput[RESP]
          .onFinalizeWeak(socket.endOfInput)
          .mergeHaltBoth(
            receive.onFinalizeWeak(socket.endOfOutput)
          )
      }
  }

  private[this] final object impl {
    def send[F[_]: MonadError[*[_], Throwable]](socketChannel: Pipe[F, Byte, Unit])(
        implicit log: LogWriter[F]
    ): Pipe[F, RESP, Unit] =
      _.evalTap(resp => log.debug(s"sending $resp"))
        .through(streamEncoder.encode[F])
        .flatMap(bits => Stream.chunk(Chunk.bytes(bits.toByteArray)))
        .through(socketChannel)

    def receive[F[_]: MonadError[*[_], Throwable]](implicit log: LogWriter[F]): Pipe[F, Byte, RESP] = {
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

      pipeIn =>
        streamDecoder
          .decode(pipeIn.through(framing) map (_.bits))
          .evalTap(resp => log.debug(s"receiving $resp"))
    }
  }
}
