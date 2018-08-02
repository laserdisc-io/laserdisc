package laserdisc

package object protocol {

  implicit def eitherSyntax[A, B](eab: Either[A, B]) = new EitherSyntaxOps(eab)
}

final private[laserdisc] class EitherSyntaxOps[A, B](private val eab: Either[A, B]) {
  def flatMap[C](f: B => Either[A, C]): Either[A, C] =
    eab match {
      case left @ Left(_) => left.asInstanceOf[Either[A, C]]
      case Right(b)       => f(b)
    }
}