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

import cats.effect.kernel.{Deferred, Temporal}
import cats.effect.syntax.temporal.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monadError.*

import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration

package object fs2 {
  final type Network[F[_]]      = _root_.fs2.io.net.Network[F]
  final type Pipe[F[_], -I, +O] = _root_.fs2.Pipe[F, I, O]
  final type Queue[F[_]]        = cats.effect.std.Queue[F, Request[F]]
  final type Signal[F[_], A]    = _root_.fs2.concurrent.SignallingRef[F, A]
  final type Stream[+F[_], +O]  = _root_.fs2.Stream[F, O]

  final val Network = _root_.fs2.io.net.Network
  final val Queue   = cats.effect.std.Queue
  final val Signal  = _root_.fs2.concurrent.SignallingRef
  final val Stream  = _root_.fs2.Stream

  final type Env[F[_]]                       = (Queue[F], FiniteDuration)
  final type RedisClient[F[_]]               = Client[F, Env[F]]
  final type RedisHandler[F[_], In <: Tuple] = Handler[F, Env[F], In]

  object RedisHandler {
    type Aux[F[_], In <: Tuple, LOut0 <: Tuple] = RedisHandler[F, In] { type LOut = LOut0 }
  }

  private[this] final def request[F[_]: Temporal, A](protocol: Protocol.Aux[A], env: Env[F]): F[Maybe[A]] = {
    val (queue, duration) = env
    Deferred[F, Maybe[A]].flatMap { promise =>
      val complete: Maybe[A] => F[Unit] =
        mba => promise.complete(mba).as(())

      queue.offer(Request(protocol, complete)) >>
        promise.get
          .timeout(duration)
          .adaptError { case _: TimeoutException => RequestTimedOut(protocol) }
    }
  }

  implicit final def emptyRedisHandler[F[_]](implicit F: cats.Applicative[F]): RedisHandler.Aux[F, EmptyTuple, EmptyTuple] =
    new RedisHandler[F, EmptyTuple] {
      override final type LOut = EmptyTuple
      override final def apply(env: Env[F], in: EmptyTuple): F[EmptyTuple] = F.pure(EmptyTuple)
    }

  implicit final def consRedisHandler[F[_], A, T <: Tuple, LT <: Tuple](
      implicit F: Temporal[F],
      T: RedisHandler.Aux[F, T, LT]
  ): RedisHandler.Aux[F, Protocol.Aux[A] *: T, Maybe[A] *: LT] =
    new RedisHandler[F, Protocol.Aux[A] *: T] {
      override final type LOut = Maybe[A] *: LT
      override final def apply(env: Env[F], in: Protocol.Aux[A] *: T): F[LOut] =
        in match {
          case protocol *: rest => F.map2(request(protocol, env), T(env, rest))(_ *: _)
        }
    }

  implicit final def functorForCats[F[_]](implicit F: cats.Functor[F]): Functor[F] =
    new Functor[F] {
      override final def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
    }

  implicit final def showForCats[A](implicit S: cats.Show[A]): Show[A] =
    (a: A) => S.show(a)
}
