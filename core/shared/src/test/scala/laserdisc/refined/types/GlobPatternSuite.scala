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
package refined.types

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

final class GlobPatternSuite extends BaseSpec {
  test("GlobPattern fails to compile given an empty String") {
    assertCompileErrorsContain(compileErrors("""GlobPattern("")"""), "Predicate failed: isGlobPattern()", "Should be a glob pattern")
  }

  test("GlobPattern fails to compile given a class that's never closed") {
    assertCompileErrorsContain(compileErrors("""GlobPattern("a[b")"""), "Predicate failed: isGlobPattern(a[b)", "Should be a glob pattern")
  }

  test("GlobPattern fails to compile given a ] that closes no class") {
    assertCompileErrorsContain(compileErrors("""GlobPattern("a]")"""), "Predicate failed: isGlobPattern(a])", "Should be a glob pattern")
  }

  test("GlobPattern fails to compile given an empty class") {
    assertCompileErrorsContain(compileErrors("""GlobPattern("a[]")"""), "Predicate failed: isGlobPattern(a[])", "Should be a glob pattern")
    assertCompileErrorsContain(
      compileErrors("""GlobPattern("a[^]")"""),
      "Predicate failed: isGlobPattern(a[^])",
      "Should be a glob pattern"
    )
  }

  test("GlobPattern checks a long pattern in one pass, giving a Left instead of overflowing the stack") {
    List("*", "a*", "[a]", "\\x", "[^a-z]?").foreach(unit => assert(GlobPattern.from(unit * 100000).isRight, unit))
    assert(GlobPattern.from("[a" * 100000).isLeft)
    assert(GlobPattern.from(("[a]" * 100000) + "]").isLeft)
  }

  test("GlobPattern refuses a \\ with nothing after it, and control characters") {
    assert(GlobPattern.from("a\\").isLeft)
    assert(GlobPattern.from("a\u0000b").isLeft)
    assert(GlobPattern.from("a[\u0007]").isLeft)
  }

  // a valid glob with one broken piece in it: a class never closed, a ] closing no class, an empty class or a control character, or a \
  // with nothing after it at the end
  private val brokenGlob = for {
    glob   <- globPatternGen
    at     <- Gen.choose(0, glob.length)
    broken <- Gen.oneOf("[", "[a", "]", "[]", "[^]", "\u0000", "\u001F", "\u007F", "\\")
  } yield if (broken == "\\") glob + broken else glob.take(at) + broken + glob.drop(at)

  property("GlobPattern fails at runtime provided non literal cases of non conformant Strings") {
    forAll(brokenGlob.filterNot(globPatternIsValid)) { s =>
      intercept[IllegalArgumentException](GlobPattern.unsafeFrom(s))
    }
  }

  test("GlobPattern compiles given conformant String") {
    GlobPattern("abc*fg?1jk")
    GlobPattern("a[bc*]fg?1jk")
    GlobPattern("user:*")
    GlobPattern("user:1:*")
    GlobPattern("a-b*")
    GlobPattern("h[^e]llo")
    GlobPattern("h[a-b]llo")
    GlobPattern("h[b-a]llo")
    GlobPattern("a\\*")
    GlobPattern("a\\]")
    GlobPattern("a[\\]]")
    GlobPattern("[^\\^]")
  }

  property("GlobPattern refines correctly provided non literal cases of conformant Strings") {
    forAll(globPatternGen) { s =>
      GlobPattern.from(s).onRight(_.value == s)
    }
  }
}
