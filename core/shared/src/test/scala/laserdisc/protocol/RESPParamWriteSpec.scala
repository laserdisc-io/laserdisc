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

  final case class WithShowField(a: Int, b: WithShow, c: Option[WithShow])
  final case class WithCaseClassField(a: Int, b: WithoutShow)
  final case class WithOptionalCaseClassField(a: Int, b: Option[WithoutShow])
  final case class WithTupleField(a: Int, b: (Int, Int))
}

final class RESPParamWriteSpec extends BaseSpec {
  import RESPParamWriteSpec.*

  test("RESPParamWrite writes a case class with a Show as one parameter") {
    assertEquals(RESPParamWrite[WithShow].write(WithShow(1, 2)), Seq[GenBulk](Bulk("1/2")))
  }

  test("RESPParamWrite writes a case class without a Show field by field") {
    assertEquals(RESPParamWrite[WithoutShow].write(WithoutShow(1, 2)), Seq[GenBulk](Bulk("a"), Bulk("1"), Bulk("b"), Bulk("2")))
  }

  test("RESPParamWrite writes each field of a case class as one parameter, with its Show") {
    assertEquals(
      RESPParamWrite[WithShowField].write(WithShowField(1, WithShow(2, 3), Some(WithShow(4, 5)))),
      Seq[GenBulk](Bulk("a"), Bulk("1"), Bulk("b"), Bulk("2/3"), Bulk("c"), Bulk("4/5"))
    )
  }

  test("RESPParamWrite doesn't write a case class with a field without a Show, which would be written as several parameters") {
    assert(compileErrors("RESPParamWrite[RESPParamWriteSpec.WithCaseClassField]").nonEmpty)
    assert(compileErrors("RESPParamWrite[RESPParamWriteSpec.WithOptionalCaseClassField]").nonEmpty)
    assert(compileErrors("RESPParamWrite[RESPParamWriteSpec.WithTupleField]").nonEmpty)
  }

  test("RESPParamWrite writes a case class with a Show in a tuple as one parameter") {
    assertEquals(
      RESPParamWrite[String *: WithShow *: EmptyTuple].write("x" *: WithShow(1, 2) *: EmptyTuple),
      Seq[GenBulk](Bulk("x"), Bulk("1/2"))
    )
  }

  test("RESPParamWrite writes a plain tuple as its elements") {
    assertEquals(RESPParamWrite[(String, Int, String)].write(("a", 1, "c")), Seq[GenBulk](Bulk("a"), Bulk("1"), Bulk("c")))
  }

  test("RESPParamWrite writes a plain tuple as its elements even when there's a Show for it") {
    implicit val tupleShow: Show[(String, Int, String)] = Show.instance(_ => "one")
    assertEquals(tupleShow.show(("a", 1, "c")), "one")
    assertEquals(RESPParamWrite[(String, Int, String)].write(("a", 1, "c")), Seq[GenBulk](Bulk("a"), Bulk("1"), Bulk("c")))
  }

  test("A command's parameters can't hold a nil, which would be a protocol error closing the connection") {
    assert(compileErrors("RESPParamWrite[Seq[GenBulk]]").nonEmpty)
    assert(compileErrors("Protocol(\"SET\", Seq[GenBulk](Bulk(\"k\"), NullBulk)).as[Str, OK]").nonEmpty)
    assert(compileErrors("RESPParamWrite[NullBulk.type]").nonEmpty)
    assertEquals(RESPParamWrite[Seq[Bulk]].write(Seq(Bulk("a"))), Seq(Bulk("a")))
  }

  test("A transaction takes at least one command") {
    assert(compileErrors("Protocol.multi(EmptyTuple)").nonEmpty)
  }

  test("A RESPParamWrite can only be built with RESPParamWrite's own functions, not written by hand") {
    assertCompileErrorsContain(
      compileErrors("new RESPParamWrite[Int] { override def write(a: Int): Seq[GenBulk] = Seq(Bulk(a.toString)) }"),
      "illegal inheritance from sealed trait RESPParamWrite",
      "Cannot extend sealed trait RESPParamWrite in a different source file"
    )
  }
}
