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

final class AclPatternSuite extends BaseSpec {
  test("AclPattern takes a glob without spaces") {
    AclPattern("app:*")
    AclPattern("[a-c]?\\*x")
  }

  test("AclPattern fails to compile given a space, an empty String or a broken class") {
    assertCompileErrorsContain(
      compileErrors("""AclPattern("a b")"""),
      "Predicate failed: isAclPattern(a b)",
      "Should be a glob pattern without spaces"
    )
    assertCompileErrorsContain(
      compileErrors("""AclPattern("")"""),
      "Predicate failed: isAclPattern()",
      "Should be a glob pattern without spaces"
    )
    assertCompileErrorsContain(
      compileErrors("""AclPattern("[a")"""),
      "Predicate failed: isAclPattern([a)",
      "Should be a glob pattern without spaces"
    )
  }

  test("AclPattern checks a long pattern in one pass, giving a Left instead of overflowing the stack") {
    assert(AclPattern.from("[a]*" * 100000).isRight)
    assert(AclPattern.from(("a" * 100000) + " ").isLeft)
  }
}
