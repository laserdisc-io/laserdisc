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

final class RESPReadSpec extends BaseSpec {
  private val num: RESPRead.Aux[Num, Long]                         = RESPRead.derive
  private val optBulk: RESPRead.Aux[OrNullBulk[Bulk], Option[Int]] = RESPRead[OrNullBulk[Bulk], Option[Int]]
  private val optArr: RESPRead.Aux[OrNilArr[Arr], Option[Arr]]     = RESPRead[OrNilArr[Arr], Option[Arr]]

  test("RESPRead reads a matching RESP type") {
    assertEquals(num.read(Num(42L)), 42L)
  }

  test("RESPRead passes Redis errors through untouched") {
    assertLeftEquals(num.read(Err("boom")), Err("boom"))
    assertLeftEquals(optBulk.read(Err("boom")), Err("boom"))
  }

  test("RESPRead fails when the RESP type does not match") {
    assertLeftEquals(num.read(Str("42")), RESPDecErr("RESP type(s) did not match: Str(42)"))
  }

  test("RESPRead fails when the RESP type matches but cannot be read") {
    assertLeftEquals(
      optBulk.read(Bulk("nope")),
      RESPDecErr(
        "RESP type(s) matched but failed to deserialize correctly with error Read[OneOf] error: Read Error: expected Bulk(Int) but was Bulk(nope), reading Bulk(nope)"
      )
    )
  }

  test("a RESPRead of a value or a null reads the null as None") {
    assertEquals(optBulk.read(Bulk("42")), Some(42))
    assertEquals(optBulk.read(NullBulk), None)
    assertEquals(optArr.read(Arr(Bulk("a"))), Some(Arr(Bulk("a"))))
    assertEquals(optArr.read(NilArr), None)
  }

  test("raw RESPRead reads any reply as is, except errors") {
    List(Str("OK"), Bulk("b"), Num(1L), NullBulk, NilArr, Arr(Bulk("a"), Num(2L))).foreach(resp =>
      assertEquals(RESPRead.raw.read(resp), resp)
    )
    assertLeftEquals(RESPRead.raw.read(Err("boom")), Err("boom"))
  }

  test("a RESPRead of a value or a null does not accept the other null") {
    assertLeftEquals(optBulk.read(NilArr), RESPDecErr("RESP type(s) did not match: NilArr"))
    assertLeftEquals(optArr.read(NullBulk), RESPDecErr("RESP type(s) did not match: NullBulk"))
  }

  test("an error reading a reply says what the reply was only up to a limit, without first writing it all out, however big it is") {
    val big   = Bulk("x" * 20000000)
    val many  = Arr(List.fill(200000)(Bulk("element")))
    val limit = RESPDecErr.MaxLength + 100
    List[Either[Throwable, Any]](
      RESPRead[Num, Long].read(big),
      RESPRead[Bulk, Int].read(big),
      RESPRead[Arr.Of[Num], Seq[Long]].read(many),
      Read[Bulk, Int].read(big),
      // read as such, so the message is only cut once, saying how much it left out
      Read[Arr.AsTuple[Num *: EmptyTuple], Long *: EmptyTuple].read(new Arr.AsTuple(List(Arr(big)))),
      Read[Arr.OfPairs[Bulk, Bulk], Seq[(String, String)]].read(new Arr.OfPairs(List(Bulk("a"), Bulk("b"), big)))
    ).foreach { result =>
      val message = result.left.toOption.map(_.getMessage).getOrElse(fail(s"expected an error, got $result"))
      assert(message.length <= limit, s"${message.length} characters: ${message.take(200)}")
      // a message cut short only once it was all written out says so with how much it left out: the whole reply
      val leftOut = """\((\d+) more characters\)""".r.findFirstMatchIn(message).fold(0L)(_.group(1).toLong)
      assert(leftOut < 10000L, s"wrote out the whole reply, leaving out $leftOut characters: ${message.take(200)}")
    }
    assertEquals(RESPDecErr.shown(Arr(Bulk("a"), Num(1L), NullBulk)), "Arr(Bulk(a),Num(1),NullBulk)")
    assert(RESPDecErr.shown(big).length <= RESPDecErr.MaxLength + 1)
  }
}
