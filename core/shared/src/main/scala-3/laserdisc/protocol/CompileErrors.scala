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

import scala.annotation.unused
import scala.quoted.*

// found only when `H` is listed again, so that the error says so instead of the generic one about the reply's RESPRead, several implicits
// up: the search succeeds, and expanding what it found doesn't compile
private[protocol] trait NotInListedTwice {
  inline implicit final def listedTwice[H, T <: Tuple]: NotIn[H, H *: T] = ${ CompileErrors.listedTwice[H, NotIn[H, H *: T]] }
}

// found only when there's no Show for `A`, so that the error says so instead of a type mismatch with Value
private[protocol] trait ValueWithoutShow {
  inline implicit final def withoutShow[A](@unused a: A): Value = ${ CompileErrors.withoutShow[A] }
}

object CompileErrors {
  private def name[A: Type](using q: Quotes): String = {
    import q.reflect.*
    TypeRepr.of[A].show(using Printer.TypeReprShortCode)
  }

  def listedTwice[H: Type, R: Type](using q: Quotes): Expr[R] =
    q.reflect.report.errorAndAbort(s"The alternatives of a OneOf must be distinct types: ${name[H]} is listed more than once")

  def withoutShow[A: Type](using q: Quotes): Expr[Value] =
    q.reflect.report.errorAndAbort(s"Implicit not found Show[${name[A]}], to send it as a value")
}
