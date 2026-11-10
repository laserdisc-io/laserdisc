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

import shapeless.{LabelledGeneric, Nat, Refute, Witness}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Length
import shapeless.ops.nat.GTEq.>=
import shapeless.nat._1

import scala.annotation.nowarn

private[protocol] trait RESPParamWriteProductInstances {
  implicit final def taggedRESPParamWrite[K <: Symbol, V](
      implicit K: Witness.Aux[K],
      V: FieldWrite[V]
  ): RESPParamWrite[FieldType[K, V]] = RESPParamWrite.instance(V.write(K.value.name, _))

  implicit final def productRESPParamWrite[P <: Product, L <: Tuple, N <: Nat](
      implicit gen: LabelledGeneric.Aux[P, L],
      @nowarn ev0: Length.Aux[L, N],
      @nowarn ev1: N >= _1,
      @nowarn ev2: LUBConstraint[L, FieldType[?, ?]],
      @nowarn ev3: Refute[Show[P]],
      rpw: RESPParamWrite[L]
  ): RESPParamWrite[P] = rpw.contramap(gen.to)
}
