package laserdisc

package object protocol {
  implicit def eitherSyntaxBase[A, B](eab: Either[A, B]) = new EitherSyntaxBaseOps(eab)
}

final private[laserdisc] class EitherSyntaxBaseOps[A, B](private val eab: Either[A, B]) extends AnyVal {

  def flatMap[C](f: B => Either[A, C]): Either[A, C] =
    eab match {
      case left @ Left(_) => left.asInstanceOf[Either[A, C]]
      case Right(b)       => f(b)
    }

  def getOrElse[BB >: B](default: =>BB): BB =
    eab match {
      case Left(_)  => default
      case Right(b) => b
    }
}