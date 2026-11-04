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

import scala.annotation.nowarn
import scala.compiletime.{constValueTuple, summonAll}
import scala.deriving.Mirror
import scala.util.NotGiven

private[protocol] trait ReadLabelledInstances {

  /** A case class (not a tuple) read from a reply made of fields, like HGETALL's, by the names of its fields */
  inline implicit final def productRead[P <: Product](
      implicit m: Mirror.ProductOf[P],
      @nowarn ev: NotGiven[P <:< Tuple]
  ): Read[Arr.OfPairs[Bulk, Bulk], P] = {
    val labels  = constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val readers = summonAll[Tuple.Map[m.MirroredElemTypes, FieldRead]].toList.asInstanceOf[List[FieldRead[Any]]]

    ProductRead { fields =>
      labels
        .lazyZip(readers)
        .foldRight[Either[RESPDecErr, List[Any]]](Right(Nil)) { case ((label, reader), acc) =>
          reader.read(label, fields).flatMap(a => acc.map(a :: _))
        }
        .map(values => m.fromProduct(Tuple.fromArray(values.toArray)))
    }
  }
}
