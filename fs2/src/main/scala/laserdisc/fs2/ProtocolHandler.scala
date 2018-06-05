package laserdisc
package fs2

import _root_.fs2.Scheduler
import cats.effect.Effect
import cats.sequence.Sequencer
import shapeless.ops.hlist.{ConstMapper, Mapper, ZipApply, ZipConst}
import shapeless.{DepFn2, HList}

import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

sealed trait ProtocolHandler[F[_], In <: HList] extends DepFn2[In, (Queue[F], FiniteDuration)] {
  type LOut <: HList
  override final type Out = F[LOut]
}

object ProtocolHandler {
  @implicitNotFound(
    "Cannot derive ProtocolHandler.\n\nThis usually depends on:\n" +
      "  - Not having an HList of only Protocols\n" +
      "  - Not having available in implicit scope an instance of a cats.effect.Effect for the chosen F\n" +
      "  - Not having available in implicit scope a scala.concurrent.ExecutionContext\n" +
      "  - Not having available in implicit scope an fs2.Scheduler")
  type Aux[F[_], In <: HList, LOut0 <: HList] = ProtocolHandler[F, In] { type LOut = LOut0 }

  implicit final def derive[F[_], In <: HList, L0 <: HList, AL <: HList, FL <: HList, Out0 <: HList, LOut0 <: HList](
                                                                                                                      implicit F: Effect[F],
                                                                                                                      ec: ExecutionContext,
                                                                                                                      s: Scheduler,
                                                                                                                      zipConst: ZipConst.Aux[Effect[F], In, L0],
                                                                                                                      constMapper: ConstMapper.Aux[(Queue[F], FiniteDuration), In, AL],
                                                                                                                      mapper: Mapper.Aux[PromiseMapper.type, L0, FL],
                                                                                                                      zipApply: ZipApply.Aux[FL, AL, Out0],
                                                                                                                      sequencer: Sequencer.Aux[Out0, F, LOut0]
  ): Aux[F, In, LOut0] = new ProtocolHandler[F, In] {
    override final type LOut = LOut0

    override final def apply(in: In, queueAndFiniteDuration: (Queue[F], FiniteDuration)): F[LOut] =
      sequencer(in.zipConst(F).map(PromiseMapper).zipApply(in.mapConst(queueAndFiniteDuration)))
  }
}
