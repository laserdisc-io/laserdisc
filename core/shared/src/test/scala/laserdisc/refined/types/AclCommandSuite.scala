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

final class AclCommandSuite extends BaseSpec {
  test("AclCommand fails to compile given an empty String") {
    assertCompileErrorsContain(compileErrors("""AclCommand("")"""), """Predicate failed: "".matches(""", "Should match")
  }

  test("AclCommand fails to compile given a category, which starts with @") {
    assertCompileErrorsContain(compileErrors("""AclCommand("@all")"""), """Predicate failed: "@all".matches(""", "Value: @all")
  }

  test("AclCommand refuses spaces and control characters") {
    assert(AclCommand.from("get set").isLeft)
    assert(AclCommand.from(" get").isLeft)
    assert(AclCommand.from("get\u0000").isLeft)
    assert(AclCommand.from("get\u007F").isLeft)
  }

  test("AclCommand compiles given a command, a subcommand, or a name with @ after its first character") {
    AclCommand("get")
    AclCommand("client|kill")
    AclCommand("select|0")
    AclCommand("a@b")
  }
}
