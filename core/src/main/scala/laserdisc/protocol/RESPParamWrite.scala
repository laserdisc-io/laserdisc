package laserdisc.protocol

import shapeless._
import shapeless.labelled.FieldType

import scala.annotation.implicitNotFound

@implicitNotFound(
  """Implicit not found RESPParamWrite[${A}].

Normally you would not need to define one manually, as one will be derived for you automatically iff:
- an instance of Show[${A}] is in scope
- ${A} is a List whose LUB has a RESPParamWrite instance defined
- ${A} is an HList whose elements all have a RESPParamWrite instance defined
"""
) trait RESPParamWrite[A] {
  def write(a: A): Seq[GenBulk]
}

object RESPParamWrite extends RESPParamWriteInstances {
  @inline final def apply[A](implicit instance: RESPParamWrite[A]): RESPParamWrite[A] = instance

  final def const[A](thunk: => Seq[GenBulk]): RESPParamWrite[A] = new RESPParamWrite[A] {
    override def write(a: A): Seq[GenBulk] = thunk
  }
  final def instance[A](f: A => Seq[GenBulk]): RESPParamWrite[A] = new RESPParamWrite[A] {
    override def write(a: A): Seq[GenBulk] = f(a)
  }
}

private[protocol] sealed trait RESPParamWriteInstances extends RESPParamWriteInstances1 {
  implicit final def showRESPParamWrite[A: Show]: RESPParamWrite[A] = RESPParamWrite.instance { a =>
    Seq(Bulk(a))
  }
  implicit final def pairRESPParamWrite[A, B](
      implicit A: RESPParamWrite[A],
      B: RESPParamWrite[B]
  ): RESPParamWrite[(A, B)] = RESPParamWrite.instance {
    case (a, b) => A.write(a) ++ B.write(b)
  }
  implicit final def listRESPParamWrite[A](implicit A: RESPParamWrite[A]): RESPParamWrite[List[A]] =
    RESPParamWrite.instance(_.flatMap(A.write))
  implicit final def taggedRESPParamWrite[K <: Symbol, V](
      implicit K: Witness.Aux[K],
      V: RESPParamWrite[V]
  ): RESPParamWrite[FieldType[K, V]] = RESPParamWrite.instance(Bulk(K.value.name) +: V.write(_))
}

private[protocol] sealed trait RESPParamWriteInstances1 {
  implicit final val nilRESPParamWrite: RESPParamWrite[Nil.type] = RESPParamWrite.const(Seq.empty)
  implicit final val hNilRESPParamWrite: RESPParamWrite[HNil]    = RESPParamWrite.const(Seq.empty)

  implicit final def hConsRESPParamWrite[H, T <: HList](
      implicit H: RESPParamWrite[H],
      T: RESPParamWrite[T]
  ): RESPParamWrite[H :: T] = RESPParamWrite.instance {
    case h :: t => H.write(h) ++: T.write(t)
  }
}
