package laserdisc
package protocol

private[protocol] trait EitherSyntax {
  implicit def eitherValuesSyntax[A](a: A) = new EitherValuesSyntaxOps(a)
  implicit def eitherSyntax[A, B](eab: Either[A, B]) = new EitherSyntaxOps(eab)
}

final private[laserdisc] class EitherValuesSyntaxOps[A](private val a: A) extends AnyVal {
  def asLeft[B]: Either[A, B] =
    Left(a).asInstanceOf[Either[A, B]]

  def asRight[B]: Either[B, A] =
    Right(a).asInstanceOf[Either[B, A]]
}

final private[laserdisc] class EitherSyntaxOps[A, B](private val eab: Either[A, B]) extends AnyVal {
  def leftMap[C](f: A => C): Either[C, B] =
    eab match {
      case Left(a)      => Left(f(a))
      case r @ Right(_) => r.asInstanceOf[Either[C, B]]
    }
}