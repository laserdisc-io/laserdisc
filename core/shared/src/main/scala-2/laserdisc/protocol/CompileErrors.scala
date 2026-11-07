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

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

// found only when `H` is listed again, so that the error says so instead of the generic one about the reply's RESPRead, several implicits
// up: the search succeeds, and using what it found doesn't compile (shapeless's @implicitAmbiguous is lost that deep)
private[protocol] trait NotInListedTwice {
  implicit final def listedTwice[H, T <: Tuple]: NotIn[H, H *: T] = macro CompileErrors.listedTwice[H]
}

// found only when there's no Show for `A`, so that the error says so instead of a type mismatch with Value
private[protocol] trait ValueWithoutShow {
  implicit final def withoutShow[A](a: A): Value = macro CompileErrors.withoutShow[A]
}

// what these expand to doesn't compile. An error raised here would be lost in the implicit search, one found on what's left isn't
object CompileErrors {
  private def failing(c: blackbox.Context)(message: String): c.Tree = {
    import c.universe._
    q"""{
      @_root_.scala.annotation.compileTimeOnly($message)
      def compileError: _root_.scala.Nothing = throw new _root_.java.lang.IllegalStateException("never called: it doesn't compile")
      compileError
    }"""
  }

  def listedTwice[H: c.WeakTypeTag](c: blackbox.Context): c.Tree =
    failing(c)(s"The alternatives of a OneOf must be distinct types: ${c.weakTypeOf[H]} is listed more than once")

  def withoutShow[A: c.WeakTypeTag](c: blackbox.Context)(a: c.Tree): c.Tree = {
    val _ = a
    failing(c)(s"Implicit not found Show[${c.weakTypeOf[A]}], to send it as a value")
  }
}
