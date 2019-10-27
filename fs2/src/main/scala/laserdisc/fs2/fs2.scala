package laserdisc

import cats.sequence.Sequencer
import shapeless.HList
import shapeless.ops.hlist.{ConstMapper, Mapper, ZipApply}

import scala.concurrent.duration.FiniteDuration

package object fs2 {
  final type Pipe[F[_], -I, +O] = _root_.fs2.Pipe[F, I, O]
  final type Queue[F[_]]        = _root_.fs2.concurrent.Queue[F, Request[F]]
  final type Signal[F[_], A]    = _root_.fs2.concurrent.SignallingRef[F, A]
  final type Stream[+F[_], +O]  = _root_.fs2.Stream[F, O]

  final val Queue  = _root_.fs2.concurrent.Queue
  final val Signal = _root_.fs2.concurrent.SignallingRef
  final val Stream = _root_.fs2.Stream

  final type Env[F[_]]                       = (Queue[F], FiniteDuration)
  final type RedisClient[F[_]]               = Client[F, Env[F]]
  final type RedisHandler[F[_], In <: HList] = Handler[F, Env[F], In]

  final object RedisHandler {
    type Aux[F[_], In <: HList, LOut0 <: HList] = RedisHandler[F, In] { type LOut = LOut0 }
  }

  implicit final def derive[
      F[_],
      In <: HList,
      AL <: HList,
      FL <: HList,
      Out0 <: HList,
      LOut0 <: HList
  ](
      implicit constMapper: ConstMapper.Aux[Env[F], In, AL],
      mapper: Mapper.Aux[PromiseMapper.type, In, FL],
      zipApply: ZipApply.Aux[FL, AL, Out0],
      sequencer: Sequencer.Aux[Out0, F, LOut0]
  ): RedisHandler.Aux[F, In, LOut0] = new RedisHandler[F, In] {
    override final type LOut = LOut0
    override final def apply(envF: Env[F], in: In): F[LOut] = sequencer(in.map(PromiseMapper).zipApply(in.mapConst(envF)))
  }

  implicit final def functorForCats[F[_]](implicit F: cats.Functor[F]): Functor[F] = new Functor[F] {
    override final def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
  }
}
