/*
 * Copyright (c) 2018-2026 LaserDisc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package laserdisc
package fs2
package parallel
package channels

import _root_.fs2.*
import _root_.fs2.io.net.Network
import cats.effect.Concurrent
import com.comcast.ip4s.{Host, SocketAddress}
import laserdisc.fs2.RedisChannel.connectedSocket
import laserdisc.fs2.parallel.adapters.{BitVectorChannelAdapter, RespChannelAdapter}
import scodec.bits.BitVector

object BitVectorInRespOutChannel {

  private[fs2] final def apply[F[_]: Network: Concurrent](
      address: SocketAddress[Host],
      receiveBufferSizeBytes: Int
  ): Pipe[F, BitVector, RESP] =
    stream =>
      Stream.resource(connectedSocket(address, receiveBufferSizeBytes)).flatMap { socket =>
        val send    = stream.through(BitVectorChannelAdapter.send(socket.write))
        val receive = socket.reads.through(RespChannelAdapter.receive)

        send.drain
          .covaryOutput[RESP]
          .mergeHaltBoth(receive)
          .onFinalizeWeak(socket.endOfOutput)
      }
}
