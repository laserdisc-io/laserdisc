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
import cats.effect.syntax.all.*
import cats.syntax.all.*

import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration

package object fs2 {
  final type Network[F[_]]      = _root_.fs2.io.net.Network[F]
  final type Pipe[F[_], -I, +O] = _root_.fs2.Pipe[F, I, O]
  final type Queue[F[_]]        = cats.effect.std.Queue[F, Batch[F]]
  final type Signal[F[_], A]    = _root_.fs2.concurrent.SignallingRef[F, A]
  final type Stream[+F[_], +O]  = _root_.fs2.Stream[F, O]

  final val Network = _root_.fs2.io.net.Network
  final val Queue   = cats.effect.std.Queue
  final val Signal  = _root_.fs2.concurrent.SignallingRef
  final val Stream  = _root_.fs2.Stream

  final type Env[F[_]]         = (Queue[F], FiniteDuration)
  final type RedisClient[F[_]] = Client[F, Env[F]]

  // the requests of a send, each with a check for whether its reply arrived, and the action that waits for all the replies
  private[fs2] final type Prepared[F[_], Out] = (Vector[(Request[F], F[Boolean])], F[Out])

  // a send's requests are queued in one go, so they reach the server together, and then their replies are awaited
  sealed abstract class RedisHandler[F[_], In <: Tuple](implicit F: Temporal[F]) extends Handler[F, Env[F], In] {
    def prepare(in: In): F[Prepared[F, LOut]]

    override final def apply(env: Env[F], in: In): F[LOut] = {
      val (queue, duration) = env
      prepare(in).flatMap {
        case (requests, replies) if requests.isEmpty => replies
        case (requests, replies)                     =>
          Deferred[F, Unit].flatMap { claim =>
            val giveUp = claim.complete(()).void

            queue.offer(Batch(requests.map(_._1), claim)) >>
              replies
                .timeout(duration)
                .onCancel(giveUp)
                .recoverWith { case _: TimeoutException =>
                  giveUp >> requests.findM(_._2.map(!_)).flatMap { unanswered =>
                    F.raiseError(RequestTimedOut(unanswered.getOrElse(requests.head)._1.protocol))
                  }
                }
          }
      }
    }
  }

  object RedisHandler {
    type Aux[F[_], In <: Tuple, LOut0 <: Tuple] = RedisHandler[F, In] { type LOut = LOut0 }
  }

  implicit final def emptyRedisHandler[F[_]](implicit F: Temporal[F]): RedisHandler.Aux[F, EmptyTuple, EmptyTuple] =
    new RedisHandler[F, EmptyTuple] {
      override final type LOut = EmptyTuple
      override final def prepare(in: EmptyTuple): F[Prepared[F, EmptyTuple]] = F.pure(Vector.empty -> F.pure(EmptyTuple))
    }

  implicit final def consRedisHandler[F[_], A, T <: Tuple, LT <: Tuple](
      implicit F: Temporal[F],
      T: RedisHandler.Aux[F, T, LT]
  ): RedisHandler.Aux[F, Protocol.Aux[A] *: T, Maybe[A] *: LT] =
    new RedisHandler[F, Protocol.Aux[A] *: T] {
      override final type LOut = Maybe[A] *: LT
      override final def prepare(in: Protocol.Aux[A] *: T): F[Prepared[F, LOut]] =
        in match {
          case protocol *: rest =>
            F.map2(Deferred[F, Maybe[A]], T.prepare(rest)) { case (promise, (requests, replies)) =>
              val request = Request(protocol, (reply: Maybe[A]) => promise.complete(reply).void)
              ((request -> promise.tryGet.map(_.isDefined)) +: requests) -> F.map2(promise.get, replies)(_ *: _)
            }
        }
    }

  implicit final def functorForCats[F[_]](implicit F: cats.Functor[F]): Functor[F] =
    new Functor[F] {
      override final def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
    }

  implicit final def showForCats[A](implicit S: cats.Show[A]): Show[A] =
    (a: A) => S.show(a)
}
