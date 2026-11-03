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

import scala.annotation.implicitNotFound
import scala.concurrent.duration.*

trait ClientBase[F[_], Env] {
  def defaultTimeout: FiniteDuration = 20.seconds

  def send[In <: Tuple, Out <: Tuple](in: In, timeout: FiniteDuration)(implicit handler: Handler.Aux[F, Env, In, Out]): F[Out]
  final def send[In <: Tuple, Out <: Tuple](in: In)(implicit ev: Handler.Aux[F, Env, In, Out]): F[Out] = send(in, defaultTimeout)

  final def send[A1](protocolA1: Protocol.Aux[A1], timeout: FiniteDuration)(
      implicit F: Functor[F],
      ev: Handler.Aux[F, Env, Protocol.Aux[A1] *: EmptyTuple, Maybe[A1] *: EmptyTuple]
  ): F[Maybe[A1]] = F.map(send(protocolA1 *: EmptyTuple, timeout))(_.head)
  final def send[A1](protocolA1: Protocol.Aux[A1])(
      implicit F: Functor[F],
      ev: Handler.Aux[F, Env, Protocol.Aux[A1] *: EmptyTuple, Maybe[A1] *: EmptyTuple]
  ): F[Maybe[A1]] = send(protocolA1, defaultTimeout)

  /** Runs the loop, sending each of its commands once the previous reply is in, as a stream of the values they give. Each command has
    * `timeout`, plus the loop's wait when its commands block
    */
  final def stream[S[_], A](loop: Loop[A], timeout: FiniteDuration)(
      implicit F: Functor[F],
      U: Unfold[F, S],
      ev: Handler.Aux[F, Env, Protocol.Aux[Loop.Step[A]] *: EmptyTuple, Maybe[Loop.Step[A]] *: EmptyTuple]
  ): S[A] =
    U.unfold(loop.first)(command => F.map(send(command, timeout + loop.waits))(_.map(step => step.values -> step.next)))
  final def stream[S[_], A](loop: Loop[A])(
      implicit F: Functor[F],
      U: Unfold[F, S],
      ev: Handler.Aux[F, Env, Protocol.Aux[Loop.Step[A]] *: EmptyTuple, Maybe[Loop.Step[A]] *: EmptyTuple]
  ): S[A] = stream(loop, defaultTimeout)
}

trait Client[F[_], Env] extends ClientBase[F, Env] with ClientExt[F, Env]

trait Handler[F[_], Env, In <: Tuple] {
  type LOut <: Tuple

  def apply(env: Env, in: In): F[LOut]
}

object Handler {
  @implicitNotFound(
    """Cannot derive Handler[${F}, ${Env}, ${In}] { type LOut = ${LOut0} }

This could depend on many things but most likely:
  - ${In} is not a Tuple of only laserdisc.Protocol types
  - deriving this Handler requires other type classes to be available in implicit scope

Try running scalac with -Xlog-implicits (or https://github.com/tek/splain)
"""
  ) type Aux[F[_], Env, In <: Tuple, LOut0 <: Tuple] = Handler[F, Env, In] { type LOut = LOut0 }
}
