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
package protocol

import shapeless.{IsTuple, LabelledGeneric, Refute, Witness}
import shapeless.labelled.*

import scala.annotation.nowarn

/** The fields of a case class, read by their names */
private[protocol] sealed trait FieldsRead[L <: Tuple] {
  def read(fields: Map[String, Bulk]): Either[RESPDecErr, L]
}

private[protocol] object FieldsRead {
  implicit final val emptyFieldsRead: FieldsRead[EmptyTuple] = new FieldsRead[EmptyTuple] {
    override final def read(fields: Map[String, Bulk]): Either[RESPDecErr, EmptyTuple] = Right(EmptyTuple)
  }

  implicit final def fieldsRead[K <: Symbol, V, T <: Tuple](
      implicit K: Witness.Aux[K],
      V: FieldRead[V],
      T: FieldsRead[T]
  ): FieldsRead[FieldType[K, V] *: T] = new FieldsRead[FieldType[K, V] *: T] {
    override final def read(fields: Map[String, Bulk]): Either[RESPDecErr, FieldType[K, V] *: T] =
      V.read(K.value.name, fields).flatMap(v => T.read(fields).map(field[K](v) *: _))
  }
}

private[protocol] trait ReadLabelledInstances {

  /** A case class (not a tuple) read from a reply made of fields, like HGETALL's, by the names of its fields */
  implicit final def productRead[P <: Product, L <: Tuple](
      implicit @nowarn ev: Refute[IsTuple[P]],
      gen: LabelledGeneric.Aux[P, L],
      L: FieldsRead[L]
  ): Read[Arr.Pairs[Bulk, Bulk], P] = ProductRead(L.read(_).map(gen.from))
}
