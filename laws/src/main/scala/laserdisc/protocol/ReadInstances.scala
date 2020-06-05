package laserdisc
package protocol

import cats.{Contravariant, Monad}

import scala.util.{Left, Right}

private[protocol] object ReadInstances {
  implicit def readContravariant[X]: Contravariant[* ==> X] =
    new Contravariant[* ==> X] {
      override def contramap[A, B](fa: A ==> X)(f: B => A): B ==> X = fa.contramap(f)
    }

  implicit def readMonad[X]: Monad[X ==> *] =
    new Monad[X ==> *] {
      override def pure[A](x: A): X ==> A                               = Read.const(x)
      override def flatMap[A, B](fa: X ==> A)(f: A => X ==> B): X ==> B = fa.flatMap(f)
      override def tailRecM[A, B](a: A)(f: A => X ==> Either[A, B]): X ==> B =
        flatMap(f(a)) {
          case Left(a)  => tailRecM(a)(f)
          case Right(b) => pure(b)
        }
    }
}
