package laserdisc
package fs2
package parallel
package channels

import _root_.fs2._
import _root_.fs2.io.tcp.{Socket, SocketGroup}
import cats.effect.{Blocker, Concurrent, ContextShift, Resource}
import cats.syntax.flatMap._

import java.net.InetSocketAddress
import scala.concurrent.duration.FiniteDuration

object RespInByteOutChannel {

  private[fs2] final def apply[F[_]: ContextShift: Concurrent](
      address: InetSocketAddress,
      writeTimeout: Option[FiniteDuration],
      readMaxBytes: Int
  )(blocker: Blocker): Pipe[F, RESP, Byte] = {
    def connectedSocket: Resource[F, Socket[F]] =
      SocketGroup(blocker, nonBlockingThreadCount = 4) >>= (_.client(address, noDelay = true))

    stream =>
      Stream.resource(connectedSocket) >>= { socket =>
        val send    = stream.through(RespChannelAdapter.send(socket.write(_, writeTimeout)))
        val receive = socket.reads(readMaxBytes)

        send.drain
          .mergeHaltBoth(receive)
          .onFinalizeWeak(socket.endOfOutput)
      }
  }
}
