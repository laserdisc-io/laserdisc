package laserdisc
package interop

import munit.Assertions

private[interop] trait EitherTestSyntax extends Assertions {

  private[laserdisc] def assertEquals[A, B](eab: Either[A, B], b: B): Unit =
    eab.fold(err => fail(s"It Should be right but was left with $err"), r => assertEquals(r, b))

  implicit final class EitherSyntax[A, B](private val eab: Either[A, B]) {
    def onRight[C](f: B => Unit): Unit =
      eab.fold(err => fail(s"It Should be right but was left with $err"), f)

    def onLeft[C](e: A => Unit): Unit =
      eab.fold(e, res => fail(s"It Should be left but was right with $res"))
  }
}
