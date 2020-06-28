package laserdisc
package fs2
package parallel
package testcases

import _root_.fs2.Chunk
import cats.effect.Sync
import scodec.bits.BitVector

private[fs2] object TestCasesLaserdiscBitVector {
  final def apply[F[_]: Sync](ch: Pipe[F, BitVector, BitVector]): TestCasesLaserdiscBitVector[F] =
    new TestCasesLaserdiscBitVector[F](ch) {}
}

private[fs2] abstract class TestCasesLaserdiscBitVector[F[_]: Sync](ch: Pipe[F, BitVector, BitVector]) extends TestSendBitVector {
  final def case1 = longSend.through(ch).compile.toVector
  final def case2 = shortSend.through(ch).compile.toVector
}

private[fs2] sealed trait TestSendBitVector extends TestCommandsBitVector {

  protected val longSend =
    Stream.chunk(
      Chunk(
        longBitVector1,
        longBitVector2,
        longBitVector3,
        longBitVector4,
        longBitVector5,
        longBitVector6,
        longBitVector7,
        longBitVector8,
        longBitVector9,
        longBitVector10,
        longBitVector11,
        longBitVector12,
        longBitVector13,
        longBitVector14,
        longBitVector15,
        longBitVector16,
        longBitVector17,
        longBitVector18,
        longBitVector19,
        longBitVector20,
        longBitVector21,
        longBitVector22,
        longBitVector23,
        longBitVector24,
        longBitVector25,
        longBitVector26,
        longBitVector27,
        longBitVector28,
        longBitVector29,
        longBitVector30,
        longBitVector31,
        longBitVector32,
        longBitVector33,
        longBitVector34,
        longBitVector35,
        longBitVector36,
        longBitVector37,
        longBitVector38,
        longBitVector39,
        longBitVector40,
        longBitVector41,
        longBitVector42,
        longBitVector43,
        longBitVector44,
        longBitVector45,
        longBitVector46,
        longBitVector47,
        longBitVector48
      )
    )

  protected val shortSend =
    Stream.chunk(
      Chunk(
        shortBitVector1,
        shortBitVector2,
        shortBitVector3,
        shortBitVector4,
        shortBitVector5,
        shortBitVector6,
        shortBitVector7,
        shortBitVector8,
        shortBitVector9,
        shortBitVector10,
        shortBitVector11,
        shortBitVector12,
        shortBitVector13,
        shortBitVector14,
        shortBitVector15,
        shortBitVector16,
        shortBitVector17,
        shortBitVector18,
        shortBitVector19,
        shortBitVector20,
        shortBitVector21,
        shortBitVector22,
        shortBitVector23,
        shortBitVector24,
        shortBitVector25,
        shortBitVector26,
        shortBitVector27,
        shortBitVector28,
        shortBitVector29,
        shortBitVector30,
        shortBitVector31,
        shortBitVector32,
        shortBitVector33,
        shortBitVector34,
        shortBitVector35,
        shortBitVector36,
        shortBitVector37,
        shortBitVector38,
        shortBitVector39,
        shortBitVector40,
        shortBitVector41,
        shortBitVector42,
        shortBitVector43,
        shortBitVector44,
        shortBitVector45,
        shortBitVector46,
        shortBitVector47,
        shortBitVector48
      )
    )
}
