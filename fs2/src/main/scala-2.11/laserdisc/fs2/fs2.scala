package laserdisc

import scala.util.{Failure, Success, Try}

package object fs2 {
  final type Queue[F[_]] = _root_.fs2.async.mutable.Queue[F, Request[F]]

  implicit final class RichTry[+T](val self: Try[T]) extends AnyVal {
    @inline def toEither: Either[Throwable, T] = self match {
      case Success(t) => Right(t)
      case Failure(t) => Left(t)
    }
  }
}
