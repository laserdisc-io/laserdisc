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

sealed trait OneOfSpecRole
final case class OneOfSpecMaster(offset: Long)              extends OneOfSpecRole
final case class OneOfSpecReplica(host: String, port: Long) extends OneOfSpecRole

// outside laserdisc.protocol, as users are, so that every instance found here is one they can find too
final class OneOfSpec extends BaseSpec {
  private final val request                                         = "id" *: EmptyTuple
  private final def message[A](result: Maybe[A]): Either[String, A] = result.left.map(_.getMessage)

  test("a value that may be missing reads as Some, its null as None, and anything else as an error") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[OrNullBulk[Bulk], Option[Int]]

    assertEquals(protocol(request).decode(Bulk("1")), Some(1))
    assertEquals(protocol(request).decode(NullBulk), None)
    assertLeftEquals(message(protocol(request).decode(Num(1L))), "RESP type(s) did not match: Num(1)")
    assertLeftEquals(
      message(protocol(request).decode(Bulk("x"))),
      "RESP type(s) of Bulk(x) matched but failed to deserialize correctly with error Read[OneOf] error: Read Error: expected Bulk(Int) but was Bulk(x)"
    )
  }

  test("a null read as anything but an Option is a missing value") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[OrNullBulk[Bulk], Int]

    assertEquals(protocol(request).decode(Bulk("1")), 1)
    assertLeftEquals(
      message(protocol(request).decode(NullBulk)),
      "RESP type(s) of NullBulk matched but failed to deserialize correctly with error Read[OneOf] error: missing value"
    )
  }

  test("a number that may be missing, as a script replies with") {
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple).as[OrNullBulk[Num], Option[Long]]

    assertEquals(protocol(request).decode(Num(3L)), Some(3L))
    assertEquals(protocol(request).decode(NullBulk), None)
  }

  test("an array that may be missing, as either null (as LPOP with a count replies before Redis 7.0 and after)") {
    def protocol =
      Protocol("CUSTOM", _: String *: EmptyTuple).as[OneOf[NilArr *: NullBulk *: Arr.Of[Bulk] *: EmptyTuple], Option[Seq[String]]]

    assertEquals(protocol(request).decode(Arr(Bulk("a"), Bulk("b"))), Some(Seq("a", "b")))
    assertEquals(protocol(request).decode(NilArr), None)
    assertEquals(protocol(request).decode(NullBulk), None)
  }

  test("as the elements of an array, and as a position of a tuple") {
    def elements = Protocol("CUSTOM", _: String *: EmptyTuple).as[Arr.Of[OrNullBulk[Bulk]], Seq[Option[Int]]]
    def scan     = Protocol("CUSTOM", _: String *: EmptyTuple)
      .as[Arr.Tuple[Bulk *: OrNilArr[Arr.Of[Bulk]] *: EmptyTuple], NonNegLong *: Option[Seq[String]] *: EmptyTuple]

    assertEquals(elements(request).decode(Arr(Bulk("1"), NullBulk, Bulk("3"))), Seq(Some(1), None, Some(3)))
    assertEquals(scan(request).decode(Arr(Bulk("7"), Arr(Bulk("a")))), NonNegLong(7L) *: Option(Seq("a")) *: EmptyTuple)
    assertEquals(scan(request).decode(Arr(Bulk("0"), NilArr)), NonNegLong(0L) *: Option.empty[Seq[String]] *: EmptyTuple)
  }

  test("alternatives of the same RESP type are told apart by what they hold: the first that matches and reads wins") {
    implicit val master: Read[Arr.Tuple[Bulk *: Num *: EmptyTuple], OneOfSpecRole] = Read.instance { arr =>
      Read[Arr.Tuple[Bulk *: Num *: EmptyTuple], String *: Long *: EmptyTuple].read(arr).flatMap {
        case "master" *: offset *: _ => Right(OneOfSpecMaster(offset))
        case other *: _              => Left(RESPDecErr(s"not a master: $other"))
      }
    }
    implicit val replica: Read[Arr.Tuple[Bulk *: Bulk *: Num *: EmptyTuple], OneOfSpecRole] = Read.instance { arr =>
      Read[Arr.Tuple[Bulk *: Bulk *: Num *: EmptyTuple], String *: String *: Long *: EmptyTuple].read(arr).flatMap {
        case "slave" *: host *: port *: _ => Right(OneOfSpecReplica(host, port))
        case other *: _                   => Left(RESPDecErr(s"not a replica: $other"))
      }
    }
    def protocol = Protocol("CUSTOM", _: String *: EmptyTuple)
      .as[OneOf[Arr.Tuple[Bulk *: Num *: EmptyTuple] *: Arr.Tuple[Bulk *: Bulk *: Num *: EmptyTuple] *: EmptyTuple], OneOfSpecRole]

    assertEquals(protocol(request).decode(Arr(Bulk("master"), Num(3129659L))), OneOfSpecMaster(3129659L): OneOfSpecRole)
    assertEquals(
      protocol(request).decode(Arr(Bulk("slave"), Bulk("127.0.0.1"), Num(9000L))),
      OneOfSpecReplica("127.0.0.1", 9000L): OneOfSpecRole
    )
    assertLeftEquals(
      message(protocol(request).decode(Arr(Bulk("sentinel"), Num(1L)))),
      "RESP type(s) of Arr(Bulk(sentinel),Num(1)) matched but failed to deserialize correctly with error Read[OneOf] error: alternative 1: not a master: sentinel; alternative 2: Read[Arr.Tuple] error: expected 3 elements, got 2"
    )
  }

  test("each alternative is listed once, as in a union") {
    assume(isScala3, "Scala 2 reports it only when the code is fully compiled, which compileErrors doesn't do")
    val listedTwice = "The alternatives of a OneOf must be distinct types: one of them is listed more than once"
    val twice       = compileErrors("""Protocol("CUSTOM", _: String *: EmptyTuple).as[OneOf[Bulk *: Bulk *: EmptyTuple], String]""")
    val apart       =
      compileErrors("""Protocol("CUSTOM", _: String *: EmptyTuple).as[OneOf[NullBulk *: Bulk *: NullBulk *: EmptyTuple], Option[String]]""")
    assert(twice.contains(listedTwice), twice)
    assert(apart.contains(listedTwice), apart)
  }
}
