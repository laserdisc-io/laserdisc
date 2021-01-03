package laserdisc
package fs2
package parallel
package testcases

import cats.effect.Sync
import scodec.bits.BitVector

private[fs2] object TestCasesLaserdiscBitVectorByte {
  final def apply[F[_]: Sync](ch: Pipe[F, BitVector, Byte]): TestCasesLaserdiscBitVectorByte[F] =
    new TestCasesLaserdiscBitVectorByte[F](ch) {}
}

private[fs2] abstract class TestCasesLaserdiscBitVectorByte[F[_]: Sync](ch: Pipe[F, BitVector, Byte]) extends TestSendBitVector {
  final def case1 = longSend.through(ch).compile.toVector
  final def case2 = shortSend.through(ch).compile.toVector
}
