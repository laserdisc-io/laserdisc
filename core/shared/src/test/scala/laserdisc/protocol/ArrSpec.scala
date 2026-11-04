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
  test("decoding Arr.Of[Bulk] with the wrong encoding names the first element that fails") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.Of[Bulk], Seq[Long]]
    val request  = "id" *: EmptyTuple

    assertLeftEquals(
      protocol(request).decode(Arr(Bulk("1"), Num(2L), Bulk("x"))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(1),Num(2),Bulk(x)) matched but failed to deserialize correctly with error Read[Arr.Of, Seq] error at element 2: Unexpected element. Was Num(2)"
    )
    assertLeftEquals(protocol(request).decode(Arr(Bulk("1"), Bulk("x"))).leftMap(_.getMessage.contains("element 2")), true)
  }

  test("decoding Arr.Of[OrNullBulk[Bulk]] reads NullBulk as None, and names the first element that fails") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.Of[OrNullBulk[Bulk]], Seq[Option[Long]]]
    val request  = "id" *: EmptyTuple

    assertEquals(protocol(request).decode(Arr(Bulk("1"), NullBulk)), Seq(Some(1L), None))
    assertLeftEquals(
      protocol(request).decode(Arr(Bulk("1"), NullBulk, Num(3L), NullBulk)).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(1),NullBulk,Num(3),NullBulk) matched but failed to deserialize correctly with error Read[Arr.Of, Seq] error at element 3: Unexpected element. Was Num(3)"
    )
  }

  test("decoding Arr.OfPairs[Bulk, Bulk] reads the pairs in order") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(String, Long)]]
    val request  = "id" *: EmptyTuple

    assertEquals(protocol(request).decode(Arr(Bulk("a"), Bulk("1"), Bulk("b"), Bulk("2"))), Seq("a" -> 1L, "b" -> 2L))
    assertEquals(protocol(request).decode(Arr(Nil)), Seq.empty[(String, Long)])
  }

  test("decoding Arr.OfPairs[Bulk, Bulk] with the wrong encoding names the pair, and which of its elements, that fails") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(String, Long)]]
    val request  = "id" *: EmptyTuple

    assertLeftEquals(
      protocol(request).decode(Arr(Bulk("a"), Bulk("1"), Bulk("b"), Bulk("x"))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(a),Bulk(1),Bulk(b),Bulk(x)) matched but failed to deserialize correctly with error Read[Arr.OfPairs, Seq] error in the second element of pair 2: Read Error: expected Bulk(Long) but was Bulk(x)"
    )
    assertLeftEquals(
      protocol(request).decode(Arr(Num(1L), Bulk("1"))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Num(1),Bulk(1)) matched but failed to deserialize correctly with error Read[Arr.OfPairs, Seq] error in the first element of pair 1: Unexpected element. Was Num(1)"
    )
    assertLeftEquals(
      protocol(request).decode(Arr(Bulk("a"), Bulk("1"), Bulk("b"))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(a),Bulk(1),Bulk(b)) matched but failed to deserialize correctly with error Read[Arr.OfPairs, Seq] error: odd number of elements, the last being Bulk(b)"
    )
  }

  test("decoding Arr.OfPairs[Bulk, RESP] as a Map takes values of any type, but never an error") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.OfPairs[Bulk, RESP], Map[String, RESP]]
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

  test("a record reads its fields by name, in any order, with what each one is") {
    val record                 = Read.record("thing")(f => f.num[Long]("a").flatMap(a => f.optionalBulk[String]("b").map(a -> _)))
    def pairs(elements: RESP*) = new Arr.OfPairs[Bulk, RESP](elements.toList)

    assertEquals(record.read(pairs(Bulk("b"), Bulk("x"), Bulk("a"), Num(1L))), 1L -> Option("x"))
    assertEquals(record.read(pairs(Bulk("a"), Num(1L))), 1L                       -> Option.empty[String])
    assertLeftEquals(record.read(pairs(Bulk("b"), Bulk("x"))).leftMap(_.message), "thing: missing field a")
    assertLeftEquals(record.read(pairs(Bulk("a"), Bulk("1"))).leftMap(_.message), "thing: field a: Unexpected value. Was Bulk(1)")
    assertLeftEquals(
      record.read(pairs(Bulk("a"), Num(1L), Bulk("b"), Num(2L))).leftMap(_.message),
      "thing: field b: Unexpected value. Was Num(2)"
    )
  }

  test("a list of records names the element, the record and the field that fail") {
    implicit val record: Read[Record, Long] = Read.record("thing")(_.num[Long]("a"))
    def protocol                            = Protocol("CUSTOM", _: String *: EmptyTuple).as[Records, Seq[Long]]
    val request                             = "id" *: EmptyTuple

    assertEquals(protocol(request).decode(Arr(Arr(Bulk("a"), Num(1L)), Arr(Bulk("a"), Num(2L)))), Seq(1L, 2L))
    assertLeftEquals(
      protocol(request).decode(Arr(Arr(Bulk("a"), Num(1L)), Arr(Bulk("a"), Bulk("x")))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Arr(Bulk(a),Num(1)),Arr(Bulk(a),Bulk(x))) matched but failed to deserialize correctly with error Read[Arr.Of, Seq] error at element 2: thing: field a: Unexpected value. Was Bulk(x)"
    )
  }

  test("a record fails when its names and values don't pair up, rather than dropping them") {
    val record                 = Read.record("thing")(_.num[Long]("a"))
    def pairs(elements: RESP*) = new Arr.OfPairs[Bulk, RESP](elements.toList)

    assertLeftEquals(record.read(pairs(Bulk("a"), Num(1L), Bulk("b"))).leftMap(_.message.contains("odd number")), true)
    assertLeftEquals(record.read(pairs(Num(1L), Bulk("a"))).leftMap(_.message.contains("first element of pair 1")), true)
  }

  test("decoding Arr.AsTuple reads each element as the type at its position, with the null of a OneOf position as None") {
    def protocol =
      Protocol("CUSTOM", _: String *: EmptyTuple)
        .as[Arr.AsTuple[Bulk *: OrNilArr[Arr.Of[Bulk]] *: EmptyTuple], NonNegLong *: Option[Seq[Int]] *: EmptyTuple]
    val request = "id" *: EmptyTuple

    assertEquals(protocol(request).decode(Arr(Bulk("3"), Arr(Bulk("1"), Bulk("2")))), NonNegLong(3L) *: Option(Seq(1, 2)) *: EmptyTuple)
    assertEquals(protocol(request).decode(Arr(Bulk("0"), NilArr)), NonNegLong(0L) *: Option.empty[Seq[Int]] *: EmptyTuple)
  }

  test("decoding Arr.AsTuple with the wrong encoding names the element that fails, or says how many elements there were") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.AsTuple[Bulk *: Num *: EmptyTuple], String *: Long *: EmptyTuple]
    val request  = "id" *: EmptyTuple

    assertLeftEquals(
      protocol(request).decode(Arr(Bulk("a"), Bulk("1"))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(a),Bulk(1)) matched but failed to deserialize correctly with error Read[Arr.AsTuple] error at element 2: Unexpected element. Was Bulk(1)"
    )
    assertLeftEquals(
      protocol(request).decode(Arr(Bulk("a"))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(a)) matched but failed to deserialize correctly with error Read[Arr.AsTuple] error: expected 2 elements, got 1"
    )
    assertLeftEquals(
      protocol(request)
        .decode(Arr(Bulk("a"), NullBulk))
        .leftMap(_.getMessage.endsWith("error at element 2: Unexpected element. Was NullBulk")),
      true
    )
  }

  test("decoding Arr.AsTupleThen of RESP as the positions alone reads what they name and leaves out what follows, but not fewer") {
    def protocol =
      Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.AsTupleThen[Bulk *: Num *: EmptyTuple, RESP], String *: Long *: EmptyTuple]
    val request = "id" *: EmptyTuple

    assertEquals(protocol(request).decode(Arr(Bulk("a"), Num(1L))), "a" *: 1L *: EmptyTuple)
    assertEquals(protocol(request).decode(Arr(Bulk("a"), Num(1L), Bulk("newer"), Num(2L))), "a" *: 1L *: EmptyTuple)
    assertLeftEquals(
      protocol(request)
        .decode(Arr(Bulk("a")))
        .leftMap(_.getMessage.endsWith("Read[Arr.AsTupleThen] error: expected at least 2 elements, got 1")),
      true
    )
    assertLeftEquals(
      protocol(request)
        .decode(Arr(Bulk("a"), Bulk("1"), Num(2L)))
        .leftMap(_.getMessage.endsWith("Read[Arr.AsTupleThen] error at element 2: Unexpected element. Was Bulk(1)")),
      true
    )
  }

  test("decoding Arr.AsTupleThen reads the positions it names and then any number of the elements that follow") {
    def protocol =
      Protocol("CUSTOM", _: String *: EmptyTuple)
        .as[Arr.AsTupleThen[Bulk *: Num *: EmptyTuple, Num], (String *: Long *: EmptyTuple, Seq[Long])]
    val request = "id" *: EmptyTuple

    assertEquals(protocol(request).decode(Arr(Bulk("a"), Num(1L))), ("a" *: 1L *: EmptyTuple, Seq.empty[Long]))
    assertEquals(protocol(request).decode(Arr(Bulk("a"), Num(1L), Num(2L), Num(3L))), ("a" *: 1L *: EmptyTuple, Seq(2L, 3L)))
    assertLeftEquals(
      protocol(request)
        .decode(Arr(Bulk("a")))
        .leftMap(_.getMessage.endsWith("Read[Arr.AsTupleThen] error: expected at least 2 elements, got 1")),
      true
    )
    assertLeftEquals(
      protocol(request)
        .decode(Arr(Bulk("a"), Num(1L), Num(2L), Bulk("x")))
        .leftMap(
          _.getMessage
            .endsWith(
              "Read[Arr.AsTupleThen] after the first 2 elements: Read[Arr.Of, Seq] error at element 2: Unexpected element. Was Bulk(x)"
            )
        ),
      true
    )
  }

  test("decoding Arr.AsTupleThen of RESP reads what follows as it is, when asked for it") {
    def protocol =
      Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.AsTupleThen[Bulk *: EmptyTuple, RESP], (String *: EmptyTuple, Seq[RESP])]

    assertEquals(
      protocol("id" *: EmptyTuple).decode(Arr(Bulk("a"), Num(1L), Bulk("b"))),
      ("a" *: EmptyTuple, Seq[RESP](Num(1L), Bulk("b")))
    )
  }

  test("decoding Arr.Of[OrNullBulk[Bulk]] as a tuple reads a missing value as None at an Option position, and as an error anywhere else") {
    def lenient = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.Of[OrNullBulk[Bulk]], Int *: Option[String] *: EmptyTuple]
    def strict  = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.Of[OrNullBulk[Bulk]], Int *: String *: EmptyTuple]
    val request = "id" *: EmptyTuple

    assertEquals(lenient(request).decode(Arr(Bulk("36"), NullBulk)), 36 *: Option.empty[String] *: EmptyTuple)
    assertEquals(lenient(request).decode(Arr(Bulk("36"), Bulk("ada"))), 36 *: Option("ada") *: EmptyTuple)
    assertLeftEquals(
      strict(request).decode(Arr(Bulk("36"), NullBulk)).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(36),NullBulk) matched but failed to deserialize correctly with error Read[Arr.Of] error at element 2: Read[OneOf] error: missing value"
    )
    assertLeftEquals(
      strict(request)
        .decode(Arr(Bulk("x"), Bulk("ada")))
        .leftMap(_.getMessage.endsWith("error at element 1: Read[OneOf] error: Read Error: expected Bulk(Int) but was Bulk(x)")),
      true
    )
    assertLeftEquals(
      strict(request).decode(Arr(Bulk("36"), Bulk("ada"), Bulk("extra"))).leftMap(_.getMessage.endsWith("expected 2 elements, got 3")),
      true
    )
  }
}
