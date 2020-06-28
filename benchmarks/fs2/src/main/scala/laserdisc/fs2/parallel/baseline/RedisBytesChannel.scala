package laserdisc
package fs2
package parallel
package baseline

import java.net.InetSocketAddress

import _root_.fs2._
import _root_.fs2.io.tcp.{Socket, SocketGroup}
import cats.MonadError
import cats.effect.{Blocker, Concurrent, ContextShift, Resource}
import cats.syntax.flatMap._
import laserdisc.protocol._
import log.effect.LogWriter
import scodec.bits.BitVector

import scala.concurrent.duration.FiniteDuration

object RedisBytesChannel {

  private[fs2] final def apply[F[_]: ContextShift: LogWriter: Concurrent](
      address: InetSocketAddress,
      writeTimeout: Option[FiniteDuration],
      readMaxBytes: Int
  )(blocker: Blocker): Pipe[F, Byte, BitVector] = {
    def connectedSocket: Resource[F, Socket[F]] =
      SocketGroup(blocker, nonBlockingThreadCount = 4) >>= (_.client(address, noDelay = true))

    stream =>
      Stream.resource(connectedSocket) >>= { socket =>
        val send    = stream.through(socket.writes(writeTimeout))
        val receive = socket.reads(readMaxBytes).through(impl.receiveResp)

        send.drain
          .covaryOutput[BitVector]
          .mergeHaltBoth(receive)
          .onFinalizeWeak(socket.endOfOutput)
      }
  }

  private[this] final object impl {

    def receiveResp[F[_]: MonadError[*[_], Throwable]]: Pipe[F, Byte, BitVector] = {
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
}
