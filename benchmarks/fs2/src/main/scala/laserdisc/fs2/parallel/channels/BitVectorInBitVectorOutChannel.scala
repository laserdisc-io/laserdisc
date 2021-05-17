package laserdisc
package fs2
package parallel
package channels

import _root_.fs2._
import _root_.fs2.io.net.Network
import cats.effect.Concurrent
import cats.syntax.flatMap._
import com.comcast.ip4s.{Host, SocketAddress}
import laserdisc.fs2.RedisChannel.connectedSocket
import laserdisc.fs2.parallel.adapters.BitVectorChannelAdapter
import scodec.bits.BitVector

object BitVectorInBitVectorOutChannel {

  private[fs2] final def apply[F[_]: Network: Concurrent](
      address: SocketAddress[Host],
      receiveBufferSizeBytes: Int
  ): Pipe[F, BitVector, BitVector] =
    stream =>
      Stream.resource(connectedSocket(address, receiveBufferSizeBytes)) >>= { socket =>
        val send    = stream.through(BitVectorChannelAdapter.send(socket.write))
        val receive = socket.reads.through(BitVectorChannelAdapter.receive)

        send.drain
          .covaryOutput[BitVector]
          .mergeHaltBoth(receive)
          .onFinalizeWeak(socket.endOfOutput)
      }
}
