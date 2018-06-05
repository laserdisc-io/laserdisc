package laserdisc.protocol

import shapeless._
import shapeless.labelled.FieldType

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Implicit not found: RESPParamWrite[${A}].\n\n" +
    "Normally you would not need to define one manually, as one will be derived for you automatically iff:\n" +
    "- an instance of Show[${A}] is in scope\n" +
    "- ${A} is a List whose LUB has a RESPParamWrite instance defined\n" +
    "- ${A} is an HList whose elements all have a RESPParamWrite instance defined\n"
) trait RESPParamWrite[A] {
  def write(a: A): Seq[BulkString]
}

object RESPParamWrite extends LowPriorityRESPParamWrite {
  @inline final def apply[A](implicit instance: RESPParamWrite[A]): RESPParamWrite[A] = instance

  final def const[A](thunk: => Seq[BulkString]): RESPParamWrite[A] = new RESPParamWrite[A] {
    override def write(a: A): Seq[BulkString] = thunk
  }
  final def instance[A](f: A => Seq[BulkString]): RESPParamWrite[A] = new RESPParamWrite[A] {
    override def write(a: A): Seq[BulkString] = f(a)
  }
}

trait LowPriorityRESPParamWrite extends LowestPriorityRESPParamWrite {
  implicit final def showRESPParamWrite[A](implicit A: Show[A]): RESPParamWrite[A] = RESPParamWrite.instance { a =>
    Seq(RESP.bulk(A.show(a)))
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
  ): RESPParamWrite[FieldType[K, V]] = RESPParamWrite.instance { v =>
    RESP.bulk(K.value.name) +: V.write(v)
  }
}

sealed trait LowestPriorityRESPParamWrite {
  implicit final val nilRESPParamWrite: RESPParamWrite[Nil.type] = RESPParamWrite.const(Seq.empty)
  implicit final val hNilRESPParamWrite: RESPParamWrite[HNil]    = RESPParamWrite.const(Seq.empty)

  implicit final def hConsRESPParamWrite[H, T <: HList](
      implicit H: RESPParamWrite[H],
      T: RESPParamWrite[T]
  ): RESPParamWrite[H :: T] = RESPParamWrite.instance {
    case h :: t => H.write(h) ++: T.write(t)
  }
}
