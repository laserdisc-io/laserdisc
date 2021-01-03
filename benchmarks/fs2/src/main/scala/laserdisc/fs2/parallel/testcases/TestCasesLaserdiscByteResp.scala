package laserdisc
package fs2
package parallel
package testcases

import cats.effect.Sync

private[fs2] object TestCasesLaserdiscByteResp {
  final def apply[F[_]: Sync](ch: Pipe[F, Byte, RESP]): TestCasesLaserdiscByteResp[F] =
    new TestCasesLaserdiscByteResp[F](ch) {}
}

private[fs2] abstract class TestCasesLaserdiscByteResp[F[_]: Sync](ch: Pipe[F, Byte, RESP]) extends TestSendBaseline {
  final def case1 = longSend.through(ch).compile.toVector
  final def case2 = shortSend.through(ch).compile.toVector
}
