package laserdisc
package fs2
package parallel
package channels

import java.net.InetSocketAddress

import _root_.fs2._
import _root_.fs2.io.tcp.{Socket, SocketGroup}
import cats.effect.{Blocker, Concurrent, ContextShift, Resource}
import cats.syntax.flatMap._
import log.effect.LogWriter
import scodec.bits.BitVector

import scala.concurrent.duration.FiniteDuration

object ByteInBitVectorOutChannel {

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
        val receive = socket.reads(readMaxBytes).through(BitVectorChannelAdapter.receive)

        send.drain
          .covaryOutput[BitVector]
          .mergeHaltBoth(receive)
          .onFinalizeWeak(socket.endOfOutput)
      }
  }
}
