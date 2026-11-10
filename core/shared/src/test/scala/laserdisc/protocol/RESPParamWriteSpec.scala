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

object RESPParamWriteSpec {
  final case class WithShow(a: Int, b: Int)
  object WithShow {
    implicit final val withShowShow: Show[WithShow] = Show.instance(w => s"${w.a}/${w.b}")
  }

  final case class WithoutShow(a: Int, b: Int)
}

final class RESPParamWriteSpec extends BaseSpec {
  import RESPParamWriteSpec.*

  test("RESPParamWrite writes a case class with a Show as one parameter") {
    assertEquals(RESPParamWrite[WithShow].write(WithShow(1, 2)), Seq[GenBulk](Bulk("1/2")))
  }

  test("RESPParamWrite writes a case class without a Show field by field") {
    assertEquals(RESPParamWrite[WithoutShow].write(WithoutShow(1, 2)), Seq[GenBulk](Bulk("a"), Bulk("1"), Bulk("b"), Bulk("2")))
  }

  test("RESPParamWrite writes a case class with a Show in a tuple as one parameter") {
    assertEquals(
      RESPParamWrite[String *: WithShow *: EmptyTuple].write("x" *: WithShow(1, 2) *: EmptyTuple),
      Seq[GenBulk](Bulk("x"), Bulk("1/2"))
    )
  }
}
