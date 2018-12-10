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
  import Read.==>
  import RESP._

  final type Aux[Sub0, A] = RESPRead[A] { type Sub = Sub0 }
  final def apply[Sub, A](implicit instance: RESPRead.Aux[Sub, A]): RESPRead.Aux[Sub, A] = instance

  private[this] final type RESPCoproduct =
    SimpleString :+: Error :+: Integer :+: NullBulkString :+: NonNullBulkString :+: NilArray :+: NonNilArray :+: CNil

  private[this] implicit val respInject: Inject[RESPCoproduct, RESP] = new Inject[RESPCoproduct, RESP] {
    override def apply(resp: RESP): RESPCoproduct = resp match {
      case simpleString: SimpleString           => Inl(simpleString)
      case error: Error                         => Inr(Inl(error))
      case integer: Integer                     => Inr(Inr(Inl(integer)))
      case NullBulkString                       => Inr(Inr(Inr(Inl(nullBulk))))
      case nonNullBulkString: NonNullBulkString => Inr(Inr(Inr(Inr(Inl(nonNullBulkString)))))
      case NilArray                             => Inr(Inr(Inr(Inr(Inr(Inl(nilArray))))))
      case nonEmptyArray: NonNilArray           => Inr(Inr(Inr(Inr(Inr(Inr(Inl(nonEmptyArray)))))))
    }
  }

  sealed abstract class DefaultRESPRead[A <: Coproduct, B, Rest <: Coproduct](R: A ==> B)(
      implicit ev0: Basis.Aux[RESPCoproduct, A, Rest],
      ev1: Selector[Rest, Error]
  ) extends RESPRead[B] {
    override final type Sub = A
    override def read(resp: RESP): Maybe[B] = Coproduct[RESPCoproduct](resp).deembed match {
      case Right(R(b)) => Right(b)
      case Right(_) =>
        Left(err(s"RESP type(s) matched but failed to deserialize correctly: $resp")).widenLeft[Throwable]
      case Left(rest) =>
        rest.select[Error].fold(Left(err(s"RESP type(s) did not match: $resp")))(Left(_)).widenLeft[Throwable]
    }
  }

  final def instance[A <: Coproduct, B, Rest <: Coproduct](R: A ==> B)(
      implicit ev0: Basis.Aux[RESPCoproduct, A, Rest],
      ev1: Selector[Rest, Error]
  ): RESPRead.Aux[A, B] = new DefaultRESPRead(R) {}

  implicit final def derive[A <: Coproduct, B, Rest <: Coproduct](
      implicit R: A ==> B,
      basis: Basis.Aux[RESPCoproduct, A, Rest],
      selector: Selector[Rest, Error]
  ): RESPRead.Aux[A, B] = new DefaultRESPRead(R) {}
}
