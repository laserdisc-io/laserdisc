package laserdisc

package object protocol {
  final type NilArr   = NilArr.type
  final type NullBulk = NullBulk.type

  implicit def eitherSyntaxBase[A, B](aOrB: A | B): EitherSyntaxBaseOps[A, B] =
    new EitherSyntaxBaseOps(aOrB)
}

final private[laserdisc] class EitherSyntaxBaseOps[A, B](private val aOrB: A | B) extends AnyVal {

  def flatMap[C](f: B => A | C): A | C = aOrB match {
    case left @ Left(_) => left.coerceRight[|, C]
    case Right(b)       => f(b)
  }

  def getOrElse[BB >: B](default: => BB): BB = aOrB match {
    case Left(_)  => default
    case Right(b) => b
  }
}
