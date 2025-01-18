/*
 * Copyright (c) 2018-2025 LaserDisc
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
package fs2

import cats.effect.kernel.{Deferred, Temporal}
import cats.effect.syntax.temporal._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monadError._
import shapeless.Poly1

import scala.concurrent.TimeoutException

object PromiseMapper extends Poly1 {
  private[this] final def mapper[F[_]: Temporal, A](
      in: (Protocol.Aux[A], Env[F])
  ): F[Maybe[A]] = {
    val (protocol, (queue, duration)) = in
    Deferred[F, Maybe[A]] >>= { promise =>
      val complete: Maybe[A] => F[Unit] =
        mba => promise.complete(mba).as(())

      queue.offer(Request(protocol, complete)) >>
        promise.get
          .timeout(duration)
          .adaptError { case _: TimeoutException => RequestTimedOut(protocol) }
    }
  }

  implicit def mkOne[F[_]: Temporal, A]: Case.Aux[(Protocol.Aux[A], Env[F]), F[Maybe[A]]] =
    at[(Protocol.Aux[A], Env[F])](mapper(_))
}
