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

final class TransactionExtPSpec extends BaseSpec with TransactionP with StringP {
  private val k    = Key("k")
  private val g    = get[Int](k)
  private val read = Right(Some(1)): Maybe[Option[Int]]

  private def frames(n: Int): Seq[RESP] =
    (Arr(Bulk("MULTI")) +: Seq.fill(n)(Arr(Bulk("GET"), Bulk("k")))) :+ Arr(Bulk("EXEC"))
  private def replies(n: Int): Seq[RESP] =
    (Str(OK.value) +: Seq.fill(n)(Str("QUEUED"))) :+ Arr(List.fill(n)(Bulk(1)))

  //format: off
  test("The Transaction extended protocol using multi sends and reads from 2 to 8 commands") {
    assertEquals(multi(g, g).frames, frames(2))
    assertEquals(multi(g, g).decodeReplies(replies(2)), Some((read, read)))
    assertEquals(multi(g, g, g).frames, frames(3))
    assertEquals(multi(g, g, g).decodeReplies(replies(3)), Some((read, read, read)))
    assertEquals(multi(g, g, g, g).frames, frames(4))
    assertEquals(multi(g, g, g, g).decodeReplies(replies(4)), Some((read, read, read, read)))
    assertEquals(multi(g, g, g, g, g).frames, frames(5))
    assertEquals(multi(g, g, g, g, g).decodeReplies(replies(5)), Some((read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g).frames, frames(6))
    assertEquals(multi(g, g, g, g, g, g).decodeReplies(replies(6)), Some((read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g).frames, frames(7))
    assertEquals(multi(g, g, g, g, g, g, g).decodeReplies(replies(7)), Some((read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g).frames, frames(8))
    assertEquals(multi(g, g, g, g, g, g, g, g).decodeReplies(replies(8)), Some((read, read, read, read, read, read, read, read)))
  }
  test("The Transaction extended protocol using multi sends and reads from 9 to 15 commands") {
    assertEquals(multi(g, g, g, g, g, g, g, g, g).frames, frames(9))
    assertEquals(multi(g, g, g, g, g, g, g, g, g).decodeReplies(replies(9)), Some((read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g).frames, frames(10))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(10)), Some((read, read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g).frames, frames(11))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(11)), Some((read, read, read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g).frames, frames(12))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(12)), Some((read, read, read, read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g).frames, frames(13))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(13)), Some((read, read, read, read, read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g).frames, frames(14))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(14)), Some((read, read, read, read, read, read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).frames, frames(15))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(15)), Some((read, read, read, read, read, read, read, read, read, read, read, read, read, read, read)))
  }
  test("The Transaction extended protocol using multi sends and reads from 16 to 22 commands") {
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).frames, frames(16))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(16)), Some((read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).frames, frames(17))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(17)), Some((read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).frames, frames(18))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(18)), Some((read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).frames, frames(19))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(19)), Some((read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).frames, frames(20))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(20)), Some((read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).frames, frames(21))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(21)), Some((read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read)))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).frames, frames(22))
    assertEquals(multi(g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g, g).decodeReplies(replies(22)), Some((read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read, read)))
  }
}
