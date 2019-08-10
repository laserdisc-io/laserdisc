package laserdisc
package protocol

private[protocol] trait EitherSyntax {
  implicit def eitherValuesSyntax[A](a: A): EitherValuesSyntaxOps[A] = new EitherValuesSyntaxOps(a)
  implicit def eitherSyntax[A, B](eab: A | B): EitherSyntaxOps[A, B] = new EitherSyntaxOps(eab)
}

final private[protocol] class EitherValuesSyntaxOps[A](private val a: A) extends AnyVal {
  def asLeft[B]: A | B  = Left(a)
  def asRight[B]: B | A = Right(a)
}

final private[protocol] class EitherSyntaxOps[A, B](private val aOrB: A | B) extends AnyVal {
  def leftMap[C](f: A => C): C | B = aOrB match {
    case Left(a) => Left(f(a))
    case right   => right.coerceLeft[C, |]
  }
}
