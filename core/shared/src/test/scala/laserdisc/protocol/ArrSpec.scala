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

final class ArrSpec extends BaseSpec with EitherSyntax {
  test("decoding the correct type Read[Arr(Bulk), Seq[A]] with the wrong encoding gives details about the decoding error") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr, Seq[Long]]
    val request  = "id" *: EmptyTuple
    val response = Arr(Num(1L), Num(2L), Num(3L), Num(4L))

    assertLeftEquals(
      protocol(request).decode(response).leftMap(_.getMessage),
      "RESP type(s) of Arr(Num(1),Num(2),Num(3),Num(4)) matched but failed to deserialize correctly with error Read[Arr(Bulk), Seq[A]] error at element 1: Unexpected for Bulk. Was Num(4)"
    )
  }

  test("decoding Arr.Of[Bulk] with the wrong encoding names the first element that fails") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.Of[Bulk], Seq[Long]]
    val request  = "id" *: EmptyTuple

    assertLeftEquals(
      protocol(request).decode(Arr(Bulk("1"), Num(2L), Bulk("x"))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(1),Num(2),Bulk(x)) matched but failed to deserialize correctly with error Read[Arr.Of, Seq] error at element 2: Unexpected element. Was Num(2)"
    )
    assertLeftEquals(protocol(request).decode(Arr(Bulk("1"), Bulk("x"))).leftMap(_.getMessage.contains("element 2")), true)
  }

  test("decoding Arr.Of[Option[Bulk]] reads NullBulk as None, and names the first element that fails") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.Of[Option[Bulk]], Seq[Option[Long]]]
    val request  = "id" *: EmptyTuple

    assertEquals(protocol(request).decode(Arr(Bulk("1"), NullBulk)), Seq(Some(1L), None))
    assertLeftEquals(
      protocol(request).decode(Arr(Bulk("1"), NullBulk, Num(3L), NullBulk)).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(1),NullBulk,Num(3),NullBulk) matched but failed to deserialize correctly with error Read[Arr.Of, Seq] error at element 3: Unexpected element. Was Num(3)"
    )
  }

  test("decoding Arr.Pairs[Bulk, Bulk] reads the pairs in order") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.Pairs[Bulk, Bulk], Seq[(String, Long)]]
    val request  = "id" *: EmptyTuple

    assertEquals(protocol(request).decode(Arr(Bulk("a"), Bulk("1"), Bulk("b"), Bulk("2"))), Seq("a" -> 1L, "b" -> 2L))
    assertEquals(protocol(request).decode(Arr(Nil)), Seq.empty[(String, Long)])
  }

  test("decoding Arr.Pairs[Bulk, Bulk] with the wrong encoding names the pair, and which of its elements, that fails") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.Pairs[Bulk, Bulk], Seq[(String, Long)]]
    val request  = "id" *: EmptyTuple

    assertLeftEquals(
      protocol(request).decode(Arr(Bulk("a"), Bulk("1"), Bulk("b"), Bulk("x"))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(a),Bulk(1),Bulk(b),Bulk(x)) matched but failed to deserialize correctly with error Read[Arr.Pairs, Seq] error in the second element of pair 2: Read Error: expected Bulk(Long) but was Bulk(x)"
    )
    assertLeftEquals(
      protocol(request).decode(Arr(Num(1L), Bulk("1"))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Num(1),Bulk(1)) matched but failed to deserialize correctly with error Read[Arr.Pairs, Seq] error in the first element of pair 1: Unexpected element. Was Num(1)"
    )
    assertLeftEquals(
      protocol(request).decode(Arr(Bulk("a"), Bulk("1"), Bulk("b"))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(a),Bulk(1),Bulk(b)) matched but failed to deserialize correctly with error Read[Arr.Pairs, Seq] error: odd number of elements, the last being Bulk(b)"
    )
  }

  test("decoding Arr.Pairs[Bulk, RESP] as a Map takes values of any type, but never an error") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.Pairs[Bulk, RESP], Map[String, RESP]]
    val request  = "id" *: EmptyTuple

    assertEquals(
      protocol(request).decode(Arr(Bulk("a"), Num(1L), Bulk("b"), Arr(Bulk("c")), Bulk("d"), NullBulk)),
      Map[String, RESP]("a" -> Num(1L), "b" -> Arr(Bulk("c")), "d" -> NullBulk)
    )
    assertLeftEquals(
      protocol(request)
        .decode(Arr(Bulk("a"), Err("ERR")))
        .leftMap(_.getMessage.contains("error in the second element of pair 1: Unexpected element")),
      true
    )
  }

  test("a reply made of fields fails when its names and values don't pair up, rather than dropping them") {
    assertEquals(Read.record("thing")(_.get("a")).read(Arr(Bulk("a"), Num(1L))), Num(1L))
    assertLeftEquals(
      Read.record("thing")(_.get("a")).read(Arr(Bulk("a"), Num(1L), Bulk("b"))).leftMap(_.message.contains("odd number")),
      true
    )
    assertLeftEquals(
      Read.record("thing")(_.get("a")).read(Arr(Num(1L), Bulk("a"))).leftMap(_.message.contains("first element of pair 1")),
      true
    )
    assertLeftEquals(
      Read.record("thing")(_.get("a")).read(Arr(Bulk("b"), Num(1L))).leftMap(_.message),
      "Unexpected thing. Was Arr(Bulk(b),Num(1))"
    )
  }
}
