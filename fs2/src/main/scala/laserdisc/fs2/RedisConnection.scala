package laserdisc
package fs2

import java.net.InetSocketAddress
import java.nio.channels.AsynchronousChannelGroup

import _root_.fs2._
import _root_.fs2.io.tcp
import cats.Applicative
import cats.effect.Effect
import cats.syntax.all._
import laserdisc.protocol.RESP
import log.effect.LogWriter
import scodec.Codec
import scodec.bits.BitVector
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

      def toBitVector: Pipe[F, Byte, Frame] = {

        def go(stream: Stream[F, Byte], previous: Frame): Pull[F, Frame, Unit] =
          stream.pull.unconsChunk flatMap {
            case Some((chunk, rest)) =>
              previous.append(chunk).fold(
                ex => Pull.raiseError(ex),
                {
                  case frame@Complete(_)                => Pull.output1(frame) >> go(rest, Empty)
                  case frame@(Empty | Incomplete(_, _)) => go(rest, frame)
                }
              )
            case _ => Pull.done
          }

        stream => go(stream, Empty).stream
      }

      _.through(toBitVector) flatMap {
        case Complete(v)      => streamDecoder.decode(v)
        case Empty            => Stream.empty
        case Incomplete(v, _) => Stream.raiseError(
          new Exception(s"Trying to decode an incomplete frame. Content: ${v.toByteArray.map(_.toChar).mkString}")
        )
      } evalMap (
        resp => log.debug(s"receiving $resp") *> resp.pure
      )
    }
  }

}

sealed trait Frame {

  def append(chunk: Chunk[Byte]): Exception | Frame =
    nextFrame(BitVector.view(chunk.toByteBuffer))

  protected final def nextFrame(bits: BitVector): Exception | Frame =
    RESP.stillToReceive(bits) map {
      n => if (n > 0) Incomplete(bits, n) else Complete(bits)
    } leftMap (new Exception(_))
}

case object Empty extends Frame
final case class Complete(full: BitVector) extends Frame
final case class Incomplete(partial: BitVector, neededBits: Long) extends Frame {

  override def append(chunk: Chunk[Byte]): Exception | Frame = {
    val newBits = BitVector.view(chunk.toByteBuffer)

    //  Saves some size inspections
    if (neededBits == newBits.size) Right(Complete(partial ++ newBits))
    else nextFrame(partial ++ newBits)
  }
}
