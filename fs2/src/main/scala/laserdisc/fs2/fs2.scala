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

import cats.sequence.Sequencer
import shapeless.HList
import shapeless.ops.hlist.{Mapper, ZipConst}

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
  final type RedisHandler[F[_], In <: HList] = Handler[F, Env[F], In]

  final object RedisHandler {
    type Aux[F[_], In <: HList, LOut0 <: HList] = RedisHandler[F, In] { type LOut = LOut0 }
  }

  implicit final def derive[
      F[_],
      In <: HList,
      InEnvL <: HList,
      FuncL <: HList,
      LOut0 <: HList
  ](
      implicit zc: ZipConst.Aux[Env[F], In, InEnvL],
      ma: Mapper.Aux[PromiseMapper.type, InEnvL, FuncL],
      sequencer: Sequencer.Aux[F, FuncL, LOut0]
  ): RedisHandler.Aux[F, In, LOut0] =
    new RedisHandler[F, In] {
      override final type LOut = LOut0
      override final def apply(envF: Env[F], in: In): F[LOut] =
        sequencer(in.zipConst(envF).map(PromiseMapper))
    }

  implicit final def functorForCats[F[_]](implicit F: cats.Functor[F]): Functor[F] =
    new Functor[F] {
      override final def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
    }

  implicit final def showForCats[A](implicit S: cats.Show[A]): Show[A] =
    (a: A) => S.show(a)
}
