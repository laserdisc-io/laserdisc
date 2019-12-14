package laserdisc
package interop

import org.scalatest.{Assertion, WordSpec}

private[interop] trait EitherTestSyntax extends WordSpec {
  implicit final class EitherSyntax[A, B](private val eab: Either[A, B]) {
    def onRight[C](f: B => Assertion): Assertion =
      eab.fold(err => fail(s"It Should be right but was left with $err"), f)

    def onLeft[C](e: A => Assertion): Assertion =
      eab.fold(e, res => fail(s"It Should be left but was right with $res"))
  }
}
