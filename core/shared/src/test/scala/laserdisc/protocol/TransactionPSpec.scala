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

final class TransactionPSpec extends BaseSpec with TransactionP with StringP {
  private val k: Key = Key("k")

  private val Queued = Str("QUEUED")

  test("The Transaction protocol using multi sends MULTI, its commands and EXEC") {
    assertEquals(
      multi(set(k, 1), get[Int](k)).frames,
      Seq(Arr(Bulk("MULTI")), Arr(Bulk("SET"), Bulk("k"), Bulk(1)), Arr(Bulk("GET"), Bulk("k")), Arr(Bulk("EXEC")))
    )
  }

  test("The Transaction protocol using multi does not compile given another multi") {
    assert(compileErrors("multi(multi(get[Int](k)))").nonEmpty)
  }

  test("The Transaction protocol using multi reads the result of each command") {
    assertEquals(
      multi(set(k, 1), get[Int](k)).decodeReplies(Seq(Str(OK.value), Queued, Queued, Arr(Str(OK.value), Bulk(1)))),
      Some((Right(OK), Right(Some(1))))
    )
  }

  test("Protocol.multi reads a tuple of commands into a tuple of results") {
    val protocol: Protocol[Option[Maybe[OK] *: Maybe[Option[Int]] *: EmptyTuple]] =
      Protocol.multi(set(k, 1) *: get[Int](k) *: EmptyTuple)

    assertEquals(
      protocol.decodeReplies(Seq(Str(OK.value), Queued, Queued, Arr(Str(OK.value), Bulk(1)))),
      Some(Right(OK) *: Right(Some(1)) *: EmptyTuple)
    )
  }

  test("The Transaction protocol using multi reads a command's own failure as that command's result") {
    assertEquals(
      multi(set(k, 1), get[Int](k)).decodeReplies(Seq(Str(OK.value), Queued, Queued, Arr(Str(OK.value), Err("WRONGTYPE")))),
      Some((Right(OK), Left(Err("WRONGTYPE"))))
    )
  }

  test("The Transaction protocol using multi reads None when a watched key changed") {
    assertEquals(multi(get[Int](k)).decodeReplies(Seq(Str(OK.value), Queued, NilArr)), None)
  }

  test("The Transaction protocol using multi fails with the reason a command was rejected") {
    assertLeftEquals(
      multi(set(k, 1), get[Int](k)).decodeReplies(Seq(Str(OK.value), Err("ERR unknown"), Queued, Err("EXECABORT"))),
      Err("ERR unknown")
    )
  }

  test("The Transaction protocol using multi fails when EXEC fails") {
    assertLeftEquals(multi(get[Int](k)).decodeReplies(Seq(Str(OK.value), Queued, Err("EXECABORT"))), Err("EXECABORT"))
  }

  test("The Transaction protocol using multi fails when EXEC returns the wrong number of results") {
    assert(multi(set(k, 1), get[Int](k)).decodeReplies(Seq(Str(OK.value), Queued, Queued, Arr(Str(OK.value)))).isLeft)
  }

  test("The Transaction protocol using multi keeps its frames and replies when mapped") {
    val mapped = multi(get[Int](k)).map(_.isDefined)
    assertEquals(mapped.frames.size, 3)
    assertEquals(mapped.decodeReplies(Seq(Str(OK.value), Queued, NilArr)), false)
  }
}
