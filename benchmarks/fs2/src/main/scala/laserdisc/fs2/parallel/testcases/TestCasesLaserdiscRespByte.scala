package laserdisc
package fs2
package parallel
package testcases

import cats.effect.Sync
import laserdisc.protocol.RESP

private[fs2] object TestCasesLaserdiscRespByte {
  final def apply[F[_]: Sync](ch: Pipe[F, RESP, Byte]): TestCasesLaserdiscRespByte[F] =
    new TestCasesLaserdiscRespByte[F](ch) {}
}

private[fs2] abstract class TestCasesLaserdiscRespByte[F[_]: Sync](ch: Pipe[F, RESP, Byte]) extends TestSendResp {
  final def case1 = longSend.through(ch).compile.toVector
  final def case2 = shortSend.through(ch).compile.toVector
}
