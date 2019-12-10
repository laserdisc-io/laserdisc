package laserdisc
package protocol

import shapeless._
import shapeless.ops.coproduct._

import scala.annotation.implicitNotFound

@implicitNotFound(
  """Implicit not found RESPRead[${A}].

You should not need to define one manually, as one will be derived for you automatically iff:
- evidence of a Read instance from some sum/co-product to ${A} can be provided
- this sum/co-product is a subset of the sum-co-product for RESP
  """
) trait RESPRead[A] {
  type Sub

  def read(resp: RESP): Maybe[A]
}

object RESPRead {
  import RESP._

  final type Aux[Sub0, A] = RESPRead[A] { type Sub = Sub0 }
  final def apply[Sub, A](implicit instance: RESPRead.Aux[Sub, A]): RESPRead.Aux[Sub, A] = instance

  private[this] implicit val respInject: Inject[RESPCoproduct, RESP] =
    (resp: RESP) =>
      resp match {
        case arr: Arr   => Inl(arr)
        case bulk: Bulk => Inr(Inl(bulk))
        case err: Err   => Inr(Inr(Inl(err)))
        case NilArr     => Inr(Inr(Inr(Inl(NilArr))))
        case NullBulk   => Inr(Inr(Inr(Inr(Inl(NullBulk)))))
        case num: Num   => Inr(Inr(Inr(Inr(Inr(Inl(num))))))
        case str: Str   => Inr(Inr(Inr(Inr(Inr(Inr(Inl(str)))))))
      }

  sealed abstract class DefaultRESPRead[A <: Coproduct, B, Rest <: Coproduct](R: A ==> B)(
      implicit ev0: Basis.Aux[RESPCoproduct, A, Rest],
      ev1: Selector[Rest, Err]
  ) extends RESPRead[B] {
    override final type Sub = A
    override def read(resp: RESP): Maybe[B] = Coproduct[RESPCoproduct](resp).deembed match {
      case Right(R(Right(b))) => Right(b)
      case Right(R(Left(RESPDecErr(m)))) =>
        Left(RESPDecErr(s"RESP type(s) of $resp matched but failed to deserialize correctly with error $m")).widenLeft[Throwable]
      case Left(rest) =>
        rest
          .select[Err]
          .fold(Left(RESPDecErr(s"RESP type(s) did not match: $resp")).widenLeft[Throwable])(Left(_).widenLeft[Throwable])
    }
  }

  final def instance[A <: Coproduct, B, Rest <: Coproduct](R: A ==> B)(
      implicit ev0: Basis.Aux[RESPCoproduct, A, Rest],
      ev1: Selector[Rest, Err]
  ): RESPRead.Aux[A, B] = new DefaultRESPRead(R) {}

  implicit final def derive[A <: Coproduct, B, Rest <: Coproduct](
      implicit R: A ==> B,
      basis: Basis.Aux[RESPCoproduct, A, Rest],
      selector: Selector[Rest, Err]
  ): RESPRead.Aux[A, B] = new DefaultRESPRead(R) {}
}
