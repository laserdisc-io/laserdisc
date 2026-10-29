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

import shapeless.Witness
import shapeless.labelled.*

private[protocol] trait ReadLabelledInstances {
  implicit final def arr2LabelledTuple[HK <: Symbol, HV, T <: Tuple](
      implicit HK: Witness.Aux[HK],
      RHV: Read[Bulk, HV],
      RT: Read[Arr, T]
  ): Read[Arr, FieldType[HK, HV] *: T] = Read.instance {
    case Arr(Bulk(HK.value.`name`) +: RHV(Right(hv)) +: rest) => RT.read(Arr(rest)).map(field[HK](hv) *: _)
    case Arr(other)                                           => Left(RESPDecErr(s"Read Error: expected `[K, V] *: T` but was $other"))
  }
}
