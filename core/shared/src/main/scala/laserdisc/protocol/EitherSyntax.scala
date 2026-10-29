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

private[protocol] trait EitherSyntax {
  implicit def eitherValuesSyntax[A](a: A): EitherValuesSyntaxOps[A]        = new EitherValuesSyntaxOps(a)
  implicit def eitherSyntax[A, B](eab: Either[A, B]): EitherSyntaxOps[A, B] = new EitherSyntaxOps(eab)
}

private[protocol] final class EitherValuesSyntaxOps[A](private val a: A) extends AnyVal {
  def asLeft[B]: Either[A, B]  = Left(a)
  def asRight[B]: Either[B, A] = Right(a)
}

private[protocol] final class EitherSyntaxOps[A, B](private val aOrB: Either[A, B]) extends AnyVal {
  def leftMap[C](f: A => C): Either[C, B] =
    aOrB match {
      case Left(a) => Left(f(a))
      case right   => right.coerceLeft[C, Either]
    }
}
