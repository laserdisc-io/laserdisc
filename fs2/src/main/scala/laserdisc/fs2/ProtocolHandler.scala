package laserdisc
package fs2

import cats.effect.{Concurrent, Timer}
import cats.sequence.Sequencer
import shapeless.ops.hlist.{ConstMapper, Mapper, ZipApply}
import shapeless.{DepFn2, HList}

import scala.annotation.implicitNotFound
import scala.concurrent.duration.FiniteDuration

sealed trait ProtocolHandler[F[_], In <: HList] extends DepFn2[In, (Queue[F], FiniteDuration)] {
  type LOut <: HList
  override final type Out = F[LOut]
}

object ProtocolHandler {
  @implicitNotFound(
    "Cannot derive ProtocolHandler.\n\nThis usually depends on:\n" +
      "  - Not having an HList of only Protocols\n" +
      "  - Not having available in implicit scope an instance of a cats.effect.Concurrent for the chosen F\n" +
      "  - Not having available in implicit scope an instance of a cats.effect.Timer for the chosen F")
  type Aux[F[_], In <: HList, LOut0 <: HList] = ProtocolHandler[F, In] { type LOut = LOut0 }

  implicit final def derive[
      F[_]: Concurrent: Timer,
      In <: HList,
      AL <: HList,
      FL <: HList,
      Out0 <: HList,
      LOut0 <: HList
  ](
      implicit constMapper: ConstMapper.Aux[(Queue[F], FiniteDuration), In, AL],
      mapper: Mapper.Aux[PromiseMapper.type, In, FL],
      zipApply: ZipApply.Aux[FL, AL, Out0],
      sequencer: Sequencer.Aux[Out0, F, LOut0]
  ): Aux[F, In, LOut0] = new ProtocolHandler[F, In] {
    override final type LOut = LOut0

    override final def apply(in: In, queueAndFiniteDuration: (Queue[F], FiniteDuration)): F[LOut] =
      sequencer(in.map(PromiseMapper).zipApply(in.mapConst(queueAndFiniteDuration)))
  }
}
