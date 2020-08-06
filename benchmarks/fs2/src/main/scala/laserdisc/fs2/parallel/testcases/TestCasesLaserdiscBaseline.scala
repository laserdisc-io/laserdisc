package laserdisc
package fs2
package parallel
package testcases

import _root_.fs2.Chunk
import cats.effect.Sync

private[fs2] object TestCasesLaserdiscBaseline {
  final def apply[F[_]: Sync](ch: Pipe[F, Byte, Byte]): TestCasesLaserdiscBaseline[F] =
    new TestCasesLaserdiscBaseline[F](ch) {}
}

private[fs2] abstract class TestCasesLaserdiscBaseline[F[_]: Sync](ch: Pipe[F, Byte, Byte]) extends TestSendBaseline {
  final def case1 = longSend.through(ch).compile.toVector
  final def case2 = shortSend.through(ch).compile.toVector
}

private[fs2] sealed trait TestSendBaseline extends TestCommandsBitVector {

  protected val longSend =
    Stream.chunk(
      Chunk.bytes(
        longBitVector1.toByteArray ++
          longBitVector2.toByteArray ++
          longBitVector3.toByteArray ++
          longBitVector4.toByteArray ++
          longBitVector5.toByteArray ++
          longBitVector6.toByteArray ++
          longBitVector7.toByteArray ++
          longBitVector8.toByteArray ++
          longBitVector9.toByteArray ++
          longBitVector10.toByteArray ++
          longBitVector11.toByteArray ++
          longBitVector12.toByteArray ++
          longBitVector13.toByteArray ++
          longBitVector14.toByteArray ++
          longBitVector15.toByteArray ++
          longBitVector16.toByteArray ++
          longBitVector17.toByteArray ++
          longBitVector18.toByteArray ++
          longBitVector19.toByteArray ++
          longBitVector20.toByteArray ++
          longBitVector21.toByteArray ++
          longBitVector22.toByteArray ++
          longBitVector23.toByteArray ++
          longBitVector24.toByteArray ++
          longBitVector25.toByteArray ++
          longBitVector26.toByteArray ++
          longBitVector27.toByteArray ++
          longBitVector28.toByteArray ++
          longBitVector29.toByteArray ++
          longBitVector30.toByteArray ++
          longBitVector31.toByteArray ++
          longBitVector32.toByteArray ++
          longBitVector33.toByteArray ++
          longBitVector34.toByteArray ++
          longBitVector35.toByteArray ++
          longBitVector36.toByteArray ++
          longBitVector37.toByteArray ++
          longBitVector38.toByteArray ++
          longBitVector39.toByteArray ++
          longBitVector40.toByteArray ++
          longBitVector41.toByteArray ++
          longBitVector42.toByteArray ++
          longBitVector43.toByteArray ++
          longBitVector44.toByteArray ++
          longBitVector45.toByteArray ++
          longBitVector46.toByteArray ++
          longBitVector47.toByteArray ++
          longBitVector48.toByteArray
      )
    )

  protected val shortSend =
    Stream.chunk(
      Chunk.bytes(
        shortBitVector1.toByteArray ++
          shortBitVector2.toByteArray ++
          shortBitVector3.toByteArray ++
          shortBitVector4.toByteArray ++
          shortBitVector5.toByteArray ++
          shortBitVector6.toByteArray ++
          shortBitVector7.toByteArray ++
          shortBitVector8.toByteArray ++
          shortBitVector9.toByteArray ++
          shortBitVector10.toByteArray ++
          shortBitVector11.toByteArray ++
          shortBitVector12.toByteArray ++
          shortBitVector13.toByteArray ++
          shortBitVector14.toByteArray ++
          shortBitVector15.toByteArray ++
          shortBitVector16.toByteArray ++
          shortBitVector17.toByteArray ++
          shortBitVector18.toByteArray ++
          shortBitVector19.toByteArray ++
          shortBitVector20.toByteArray ++
          shortBitVector21.toByteArray ++
          shortBitVector22.toByteArray ++
          shortBitVector23.toByteArray ++
          shortBitVector24.toByteArray ++
          shortBitVector25.toByteArray ++
          shortBitVector26.toByteArray ++
          shortBitVector27.toByteArray ++
          shortBitVector28.toByteArray ++
          shortBitVector29.toByteArray ++
          shortBitVector30.toByteArray ++
          shortBitVector31.toByteArray ++
          shortBitVector32.toByteArray ++
          shortBitVector33.toByteArray ++
          shortBitVector34.toByteArray ++
          shortBitVector35.toByteArray ++
          shortBitVector36.toByteArray ++
          shortBitVector37.toByteArray ++
          shortBitVector38.toByteArray ++
          shortBitVector39.toByteArray ++
          shortBitVector40.toByteArray ++
          shortBitVector41.toByteArray ++
          shortBitVector42.toByteArray ++
          shortBitVector43.toByteArray ++
          shortBitVector44.toByteArray ++
          shortBitVector45.toByteArray ++
          shortBitVector46.toByteArray ++
          shortBitVector47.toByteArray ++
          shortBitVector48.toByteArray
      )
    )
}
