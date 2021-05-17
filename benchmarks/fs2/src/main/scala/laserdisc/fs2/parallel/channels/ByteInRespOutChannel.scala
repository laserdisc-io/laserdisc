package laserdisc
package fs2
package parallel
package channels

import _root_.fs2._
import _root_.fs2.io.net.Network
import cats.effect.kernel.Concurrent
import cats.syntax.flatMap._
import com.comcast.ip4s.{Host, SocketAddress}
import laserdisc.fs2.RedisChannel.connectedSocket
import laserdisc.fs2.parallel.adapters.RespChannelAdapter

object ByteInRespOutChannel {

  private[fs2] final def apply[F[_]: Network: Concurrent](
      address: SocketAddress[Host],
      receiveBufferSizeBytes: Int
  ): Pipe[F, Byte, RESP] =
    stream =>
      Stream.resource(connectedSocket(address, receiveBufferSizeBytes)) >>= { socket =>
        val send    = stream.through(socket.writes)
        val receive = socket.reads.through(RespChannelAdapter.receive)

        send.drain
          .covaryOutput[RESP]
          .mergeHaltBoth(receive)
          .onFinalizeWeak(socket.endOfOutput)
      }
}
