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

import io.github.iltotore.iron.Constraint

import scala.quoted.{Expr, Quotes}

final case class ValidGlobPattern()
object ValidGlobPattern {
  given Constraint[String, ValidGlobPattern] with {
    inline override def test(inline s: String): Boolean = ${ testExpr('s) }
    inline override def message: String                 = "Should be a glob pattern"
  }

  // runs the check at compile time on a literal, so that iron can refine it, and at run time otherwise
  private def testExpr(s: Expr[String])(using Quotes): Expr[Boolean] =
    s.value.fold('{ GlobCheck.isGlobPattern($s) })(p => Expr(GlobCheck.isGlobPattern(p)))
}

final case class ValidAclPattern()
object ValidAclPattern {
  given Constraint[String, ValidAclPattern] with {
    inline override def test(inline s: String): Boolean = ${ testExpr('s) }
    inline override def message: String                 = "Should be a glob pattern without spaces"
  }

  private def testExpr(s: Expr[String])(using Quotes): Expr[Boolean] =
    s.value.fold('{ GlobCheck.isAclPattern($s) })(p => Expr(GlobCheck.isAclPattern(p)))
}
