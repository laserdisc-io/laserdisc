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

final class ArrayPSpec extends BaseSpec with ArrayP {
  import arraytypes.*

  private def bulks(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)
  private val k                       = Key("a")
  private def i(n: Long): NonNegLong  = NonNegLong.unsafeFrom(n)
  private val two                     = OneOrMore.unsafeFrom(List(i(1L), i(9L)))
  private val values                  = OneOrMore.unsafeFrom(List("x", "y"))

  test("The Array protocol sends a Bulk as its bytes in a predicate, an operation and the values it sets") {
    val binary = Bulk(scodec.bits.ByteVector(0xff, 0x80))
    assertEquals(
      argrep(k, ArrayBound.min, ArrayBound.max, ArrayPredicate.exact(binary), ArrayPredicate.matching(binary)).encode,
      Arr(Bulk("ARGREP"), Bulk("a"), Bulk("-"), Bulk("+"), Bulk("EXACT"), binary, Bulk("MATCH"), binary)
    )
    assertEquals(
      arop(k, i(0L), i(9L), ArrayOp.matching(binary)).encode,
      Arr(Bulk("AROP"), Bulk("a"), Bulk("0"), Bulk("9"), Bulk("MATCH"), binary)
    )
    assertEquals(armset(k, OneOrMore.unsafeFrom(List(i(1L) -> binary))).encode, Arr(Bulk("ARMSET"), Bulk("a"), Bulk("1"), binary))
  }

  test("The Array protocol sets and gets elements by index") {
    assertEquals(arset(k, i(2L), values).encode, bulks("ARSET", "a", "2", "x", "y"))
    assertEquals(arset(k, i(2L), values).decode(Num(2L)), i(2L))
    assertEquals(arget[String](k, i(2L)).encode, bulks("ARGET", "a", "2"))
    assertEquals(arget[String](k, i(2L)).decode(Bulk("x")), Option("x"))
    assertEquals(arget[String](k, i(2L)).decode(NullBulk), Option.empty[String])
    assertEquals(armset(k, OneOrMore.unsafeFrom(List(i(1L) -> "x", i(9L) -> "y"))).encode, bulks("ARMSET", "a", "1", "x", "9", "y"))
    assertEquals(armget[String](k, two).encode, bulks("ARMGET", "a", "1", "9"))
    assertEquals(armget[Int](k, two).decode(Arr(Bulk("1"), NullBulk)), Seq(Option(1), None))
    assertEquals(argetrange[String](k, i(0L), i(1L)).encode, bulks("ARGETRANGE", "a", "0", "1"))
    assertEquals(argetrange[String](k, i(0L), i(1L)).decode(Arr(NullBulk, Bulk("x"))), Seq(None, Option("x")))
  }

  test("The Array protocol inserts at the cursor, and in a ring buffer") {
    assertEquals(arinsert(k, values).encode, bulks("ARINSERT", "a", "x", "y"))
    assertEquals(arinsert(k, values).decode(Num(1L)), i(1L))
    assertEquals(arnext(k).encode, bulks("ARNEXT", "a"))
    assertEquals(arnext(k).decode(Num(2L)), i(2L))
    assertEquals(arseek(k, i(5L)).encode, bulks("ARSEEK", "a", "5"))
    assertEquals(arseek(k, i(5L)).decode(Num(1L)), true)
    assertEquals(arseek(k, i(5L)).decode(Num(0L)), false)
    assertEquals(arring(k, PosLong(3L), values).encode, bulks("ARRING", "a", "3", "x", "y"))
    assertEquals(arlastitems[String](k, PosLong(2L)).encode, bulks("ARLASTITEMS", "a", "2"))
    assertEquals(arlastitems[String](k, PosLong(2L), ArrayRev).encode, bulks("ARLASTITEMS", "a", "2", "REV"))
    assertEquals(arlastitems[String](k, PosLong(2L)).decode(Arr(NullBulk, Bulk("x"))), Seq(None, Option("x")))
  }

  test("The Array protocol deletes, counts and scans") {
    assertEquals(ardel(k, two).encode, bulks("ARDEL", "a", "1", "9"))
    assertEquals(
      ardelrange(k, OneOrMore.unsafeFrom(List(i(0L) -> i(1L), i(5L) -> i(6L)))).encode,
      bulks("ARDELRANGE", "a", "0", "1", "5", "6")
    )
    assertEquals(arcount(k).encode, bulks("ARCOUNT", "a"))
    assertEquals(arlen(k).encode, bulks("ARLEN", "a"))
    assertEquals(arscan[String](k, i(0L), i(9L)).encode, bulks("ARSCAN", "a", "0", "9"))
    assertEquals(arscan[String](k, i(0L), i(9L), PosLong(1L)).encode, bulks("ARSCAN", "a", "0", "9", "LIMIT", "1"))
    assertEquals(arscan[Int](k, i(0L), i(9L)).decode(Arr(Arr(Num(3L), Bulk("7")))), Seq(i(3L) -> 7))
  }

  test("The Array protocol searches with predicates and options, with or without the values") {
    val predicates = OneOrMore.unsafeFrom(
      List(ArrayPredicate.exact(1), ArrayPredicate.matching("m"), ArrayPredicate.glob(GlobPattern("g*")), ArrayPredicate.re("^r"))
    )
    assertEquals(
      argrep(k, ArrayBound.min, ArrayBound.max, predicates).encode,
      bulks("ARGREP", "a", "-", "+", "EXACT", "1", "MATCH", "m", "GLOB", "g*", "RE", "^r")
    )
    val one = OneOrMore.unsafeFrom(List(ArrayPredicate.exact("e")))
    assertEquals(
      argrep(k, ArrayBound.at(i(1L)), ArrayBound.at(i(9L)), one, ArrayGrepOptions.or.limit(PosLong(2L)).nocase).encode,
      bulks("ARGREP", "a", "1", "9", "EXACT", "e", "OR", "LIMIT", "2", "NOCASE")
    )
    assertEquals(
      argrep(k, ArrayBound.min, ArrayBound.max, one, ArrayGrepOptions.or.and).encode,
      bulks("ARGREP", "a", "-", "+", "EXACT", "e", "AND")
    )
    assertEquals(argrep(k, ArrayBound.min, ArrayBound.max, one).decode(Arr(Num(1L))), Seq(i(1L)))
    assertEquals(
      argrepwithvalues[String](k, ArrayBound.min, ArrayBound.max, one).encode,
      bulks("ARGREP", "a", "-", "+", "EXACT", "e", "WITHVALUES")
    )
    assertEquals(
      argrepwithvalues[String](k, ArrayBound.min, ArrayBound.max, one, ArrayGrepOptions.nocase).encode,
      bulks("ARGREP", "a", "-", "+", "EXACT", "e", "NOCASE", "WITHVALUES")
    )
    assertEquals(argrepwithvalues[Int](k, ArrayBound.min, ArrayBound.max, one).decode(Arr(Arr(Num(3L), Bulk("7")))), Seq(i(3L) -> 7))
  }

  test("The Array protocol aggregates, reading each operation's reply") {
    assertEquals(arop(k, i(0L), i(9L), ArrayOp.sum).encode, bulks("AROP", "a", "0", "9", "SUM"))
    assertEquals(arop(k, i(0L), i(9L), ArrayOp.sum).decode(Bulk("1.5")), Option(1.5d))
    assertEquals(arop(k, i(0L), i(9L), ArrayOp.max).decode(NullBulk), Option.empty[Double])
    assertEquals(arop(k, i(0L), i(9L), ArrayOp.xor).encode, bulks("AROP", "a", "0", "9", "XOR"))
    assertEquals(arop(k, i(0L), i(9L), ArrayOp.and).decode(Num(3L)), Option(3L))
    assertEquals(arop(k, i(0L), i(9L), ArrayOp.or).decode(NullBulk), Option.empty[Long])
    assertEquals(arop(k, i(0L), i(9L), ArrayOp.used).encode, bulks("AROP", "a", "0", "9", "USED"))
    assertEquals(arop(k, i(0L), i(9L), ArrayOp.matching(2)).encode, bulks("AROP", "a", "0", "9", "MATCH", "2"))
    assertEquals(arop(k, i(0L), i(9L), ArrayOp.used).decode(Num(3L)), i(3L))
  }

  test("The Array protocol reads the array's information") {
    val reply = List[RESP](
      Bulk("count"),
      Num(2L),
      Bulk("len"),
      Num(11L),
      Bulk("next-insert-index"),
      Num(101L),
      Bulk("slices"),
      Num(1L),
      Bulk("directory-size"),
      Num(1L),
      Bulk("super-dir-entries"),
      Num(0L),
      Bulk("slice-size"),
      Num(4096L)
    )
    val info = ArrayInfo(i(2L), i(11L), i(101L), i(1L), i(1L), i(0L), i(4096L))
    assertEquals(arinfo(k).encode, bulks("ARINFO", "a"))
    assertEquals(arinfo(k).decode(Arr(reply)), info)
    assertEquals(arinfo(k, ArrayFull).encode, bulks("ARINFO", "a", "FULL"))
    assertEquals(
      arinfo(k, ArrayFull).decode(
        Arr(
          reply ++ List[RESP](
            Bulk("dense-slices"),
            Num(0L),
            Bulk("sparse-slices"),
            Num(1L),
            Bulk("avg-dense-size"),
            Bulk("0"),
            Bulk("avg-dense-fill"),
            Bulk("0"),
            Bulk("avg-sparse-size"),
            Bulk("8")
          )
        )
      ),
      ArrayFullInfo(info, i(0L), i(1L), 0d, 0d, 8d)
    )
  }
}
