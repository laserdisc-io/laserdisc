package laserdisc
package fs2
package parallel
package testcases

import cats.effect.kernel.Concurrent
import scodec.bits.BitVector

private[fs2] object TestCasesLaserdiscByteBitVector {
  final def apply[F[_]: Concurrent](ch: Pipe[F, Byte, BitVector]): TestCasesLaserdiscByteBitVector[F] =
    new TestCasesLaserdiscByteBitVector[F](ch) {}
}

private[fs2] abstract class TestCasesLaserdiscByteBitVector[F[_]: Concurrent](ch: Pipe[F, Byte, BitVector]) extends TestSendBaseline {
  final def case1 = longSend.through(ch).compile.toVector
  final def case2 = shortSend.through(ch).compile.toVector
}
