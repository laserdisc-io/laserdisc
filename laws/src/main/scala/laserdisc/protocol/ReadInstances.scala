/*
 * Copyright (c) 2018-2026 LaserDisc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package laserdisc
package protocol

import cats.{Contravariant, Monad}

import scala.util.{Left, Right}

private[protocol] object ReadInstances {
  implicit def readContravariant[X]: Contravariant[Read[_, X]] =
    new Contravariant[Read[_, X]] {
      override def contramap[A, B](fa: Read[A, X])(f: B => A): Read[B, X] = fa.contramap(f)
    }

  implicit def readMonad[X]: Monad[Read[X, _]] =
    new Monad[Read[X, _]] {
      override def pure[A](x: A): Read[X, A]                                       = Read.const(x)
      override def flatMap[A, B](fa: Read[X, A])(f: A => Read[X, B]): Read[X, B]   = fa.flatMap(f)
      override def tailRecM[A, B](a: A)(f: A => Read[X, Either[A, B]]): Read[X, B] =
        flatMap(f(a)) {
          case Left(a)  => tailRecM(a)(f)
          case Right(b) => pure(b)
        }
    }
}
