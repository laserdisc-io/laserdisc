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
package protocol

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

  final def const[A](thunk: =>Seq[GenBulk]): RESPParamWrite[A] =
    (_: A) => thunk
  final def instance[A](f: A => Seq[GenBulk]): RESPParamWrite[A] =
    (a: A) => f(a)
}

private[protocol] sealed trait RESPParamWriteInstances extends RESPParamWriteInstances1 {
  implicit final def showRESPParamWrite[A: Show]: RESPParamWrite[A] = RESPParamWrite.instance(a => Seq(Bulk(a)))
  implicit final def pairRESPParamWrite[A, B](
      implicit A: RESPParamWrite[A],
      B: RESPParamWrite[B]
  ): RESPParamWrite[(A, B)] =
    RESPParamWrite.instance { case (a, b) =>
      A.write(a) ++ B.write(b)
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
  ): RESPParamWrite[H :: T] =
    RESPParamWrite.instance { case h :: t =>
      H.write(h) ++: T.write(t)
    }
}
