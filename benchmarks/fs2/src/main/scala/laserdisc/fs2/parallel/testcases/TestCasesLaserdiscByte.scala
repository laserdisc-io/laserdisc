package laserdisc
package fs2
package parallel
package testcases

import _root_.fs2.Chunk
import cats.effect.Sync
import scodec.bits.BitVector

private[fs2] object TestCasesLaserdiscByte {
  final def apply[F[_]: Sync](ch: Pipe[F, Byte, BitVector]): TestCasesLaserdiscByte[F] =
    new TestCasesLaserdiscByte[F](ch) {}
}

private[fs2] abstract class TestCasesLaserdiscByte[F[_]: Sync](ch: Pipe[F, Byte, BitVector]) extends TestSendByte {
  final def case1 = longSend.through(ch).compile.toVector
  final def case2 = shortSend.through(ch).compile.toVector
}

private[fs2] sealed trait TestSendByte extends TestCommandsBitVector {

  protected val longSend =
    Stream.chunk(
      Chunk.concat(
        Seq(
          Chunk.bytes(longBitVector1.toByteArray),
          Chunk.bytes(longBitVector2.toByteArray),
          Chunk.bytes(longBitVector3.toByteArray),
          Chunk.bytes(longBitVector4.toByteArray),
          Chunk.bytes(longBitVector5.toByteArray),
          Chunk.bytes(longBitVector6.toByteArray),
          Chunk.bytes(longBitVector7.toByteArray),
          Chunk.bytes(longBitVector8.toByteArray),
          Chunk.bytes(longBitVector9.toByteArray),
          Chunk.bytes(longBitVector10.toByteArray),
          Chunk.bytes(longBitVector11.toByteArray),
          Chunk.bytes(longBitVector12.toByteArray),
          Chunk.bytes(longBitVector13.toByteArray),
          Chunk.bytes(longBitVector14.toByteArray),
          Chunk.bytes(longBitVector15.toByteArray),
          Chunk.bytes(longBitVector16.toByteArray),
          Chunk.bytes(longBitVector17.toByteArray),
          Chunk.bytes(longBitVector18.toByteArray),
          Chunk.bytes(longBitVector19.toByteArray),
          Chunk.bytes(longBitVector20.toByteArray),
          Chunk.bytes(longBitVector21.toByteArray),
          Chunk.bytes(longBitVector22.toByteArray),
          Chunk.bytes(longBitVector23.toByteArray),
          Chunk.bytes(longBitVector24.toByteArray),
          Chunk.bytes(longBitVector25.toByteArray),
          Chunk.bytes(longBitVector26.toByteArray),
          Chunk.bytes(longBitVector27.toByteArray),
          Chunk.bytes(longBitVector28.toByteArray),
          Chunk.bytes(longBitVector29.toByteArray),
          Chunk.bytes(longBitVector30.toByteArray),
          Chunk.bytes(longBitVector31.toByteArray),
          Chunk.bytes(longBitVector32.toByteArray),
          Chunk.bytes(longBitVector33.toByteArray),
          Chunk.bytes(longBitVector34.toByteArray),
          Chunk.bytes(longBitVector35.toByteArray),
          Chunk.bytes(longBitVector36.toByteArray),
          Chunk.bytes(longBitVector37.toByteArray),
          Chunk.bytes(longBitVector38.toByteArray),
          Chunk.bytes(longBitVector39.toByteArray),
          Chunk.bytes(longBitVector40.toByteArray),
          Chunk.bytes(longBitVector41.toByteArray),
          Chunk.bytes(longBitVector42.toByteArray),
          Chunk.bytes(longBitVector43.toByteArray),
          Chunk.bytes(longBitVector44.toByteArray),
          Chunk.bytes(longBitVector45.toByteArray),
          Chunk.bytes(longBitVector46.toByteArray),
          Chunk.bytes(longBitVector47.toByteArray),
          Chunk.bytes(longBitVector48.toByteArray)
        )
      )
    )

  protected val shortSend =
    Stream.chunk(
      Chunk.concat(
        Seq(
          Chunk.bytes(shortBitVector1.toByteArray),
          Chunk.bytes(shortBitVector2.toByteArray),
          Chunk.bytes(shortBitVector3.toByteArray),
          Chunk.bytes(shortBitVector4.toByteArray),
          Chunk.bytes(shortBitVector5.toByteArray),
          Chunk.bytes(shortBitVector6.toByteArray),
          Chunk.bytes(shortBitVector7.toByteArray),
          Chunk.bytes(shortBitVector8.toByteArray),
          Chunk.bytes(shortBitVector9.toByteArray),
          Chunk.bytes(shortBitVector10.toByteArray),
          Chunk.bytes(shortBitVector11.toByteArray),
          Chunk.bytes(shortBitVector12.toByteArray),
          Chunk.bytes(shortBitVector13.toByteArray),
          Chunk.bytes(shortBitVector14.toByteArray),
          Chunk.bytes(shortBitVector15.toByteArray),
          Chunk.bytes(shortBitVector16.toByteArray),
          Chunk.bytes(shortBitVector17.toByteArray),
          Chunk.bytes(shortBitVector18.toByteArray),
          Chunk.bytes(shortBitVector19.toByteArray),
          Chunk.bytes(shortBitVector20.toByteArray),
          Chunk.bytes(shortBitVector21.toByteArray),
          Chunk.bytes(shortBitVector22.toByteArray),
          Chunk.bytes(shortBitVector23.toByteArray),
          Chunk.bytes(shortBitVector24.toByteArray),
          Chunk.bytes(shortBitVector25.toByteArray),
          Chunk.bytes(shortBitVector26.toByteArray),
          Chunk.bytes(shortBitVector27.toByteArray),
          Chunk.bytes(shortBitVector28.toByteArray),
          Chunk.bytes(shortBitVector29.toByteArray),
          Chunk.bytes(shortBitVector30.toByteArray),
          Chunk.bytes(shortBitVector31.toByteArray),
          Chunk.bytes(shortBitVector32.toByteArray),
          Chunk.bytes(shortBitVector33.toByteArray),
          Chunk.bytes(shortBitVector34.toByteArray),
          Chunk.bytes(shortBitVector35.toByteArray),
          Chunk.bytes(shortBitVector36.toByteArray),
          Chunk.bytes(shortBitVector37.toByteArray),
          Chunk.bytes(shortBitVector38.toByteArray),
          Chunk.bytes(shortBitVector39.toByteArray),
          Chunk.bytes(shortBitVector40.toByteArray),
          Chunk.bytes(shortBitVector41.toByteArray),
          Chunk.bytes(shortBitVector42.toByteArray),
          Chunk.bytes(shortBitVector43.toByteArray),
          Chunk.bytes(shortBitVector44.toByteArray),
          Chunk.bytes(shortBitVector45.toByteArray),
          Chunk.bytes(shortBitVector46.toByteArray),
          Chunk.bytes(shortBitVector47.toByteArray),
          Chunk.bytes(shortBitVector48.toByteArray)
        )
      )
    )
}
