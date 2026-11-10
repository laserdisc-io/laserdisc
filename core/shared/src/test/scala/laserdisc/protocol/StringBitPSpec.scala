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

import org.scalacheck.Prop.forAll

final class StringBitPSpec extends BaseSpec with StringP {
  import StringP.{LcsMatch, LcsMatches, LcsRange}

  property("The String protocol using bitfield sends each step in order and reads a number or None for each") {
    forAll { (k: Key, s: SignedBitWidth, u: UnsignedBitWidth, o: NonNegLong, v: Long, ols: List[Option[Long]]) =>
      val bf       = stringtypes.bitfield
      val protocol = bitfield(k, bf.set(bf.u(u), bf.bit(o), v).overflow(bf.fail).incrBy(bf.i(s), bf.field(o), v).get(bf.i(s), bf.bit(o)))
      assertEquals(
        protocol.encode,
        Arr(
          (List("BITFIELD", k.value, "SET", s"u${u.value}", o.value.toString, v.toString, "OVERFLOW", "FAIL") :::
            List("INCRBY", s"i${s.value}", s"#${o.value}", v.toString, "GET", s"i${s.value}", o.value.toString)).map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(Arr(ols.map(_.fold[RESP](NullBulk)(Num(_))))), ols)
    }
  }

  property("The String protocol using bitfieldro sends a GET for each field and reads a number for each") {
    forAll { (k: Key, u: UnsignedBitWidth, o: NonNegLong, ls: List[Long]) =>
      val bf       = stringtypes.bitfield
      val protocol = bitfieldro(k, OneOrMore.unsafeFrom(List(bf.u(u) -> bf.bit(o), bf.u(u) -> bf.field(o))))
      assertEquals(
        protocol.encode,
        Arr(List("BITFIELD_RO", k.value, "GET", s"u${u.value}", o.value.toString, "GET", s"u${u.value}", s"#${o.value}").map(Bulk(_)))
      )
      assertEquals(protocol.decode(Arr(ls.map(Num(_)))), ls)
    }
  }

  property("The String protocol using bitcount and bitpos send BIT or BYTE after the range") {
    forAll { (k: Key, s: Index, e: Index, n: NonNegInt) =>
      assertEquals(bitcount(k, s, e, stringtypes.bitUnit.bit).encode, Arr(Bulk("BITCOUNT"), Bulk(k), Bulk(s), Bulk(e), Bulk("BIT")))
      assertEquals(
        bitpos(k, stringtypes.bit.set, s, e, stringtypes.bitUnit.byte).encode,
        Arr(Bulk("BITPOS"), Bulk(k), Bulk("1"), Bulk(s), Bulk(e), Bulk("BYTE"))
      )
      assertEquals(bitpos(k, stringtypes.bit.set, s, e, stringtypes.bitUnit.byte).decode(Num(-1L)), None)
      assertEquals(bitcount(k, s, e, stringtypes.bitUnit.bit).decode(Num(n.value.toLong)), n)
    }
  }

  property("The String protocol using bitop with DIFF sends it like AND and OR") {
    forAll { (ks: TwoOrMoreKeys, d: Key, n: NonNegInt) =>
      val protocol = bitop(stringtypes.bitwise.diff1, ks, d)
      assertEquals(protocol.encode, Arr(Bulk("BITOP") :: Bulk("DIFF1") :: Bulk(d) :: ks.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(n.value.toLong)), n)
    }
  }

  property("The String protocol using bitopone sends ONE with a single key") {
    forAll { (k: Key, d: Key, n: NonNegInt) =>
      val protocol = bitopone(k, d)
      assertEquals(protocol.encode, Arr(Bulk("BITOP"), Bulk("ONE"), Bulk(d), Bulk(k)))
      assertEquals(protocol.decode(Num(n.value.toLong)), n)
    }
  }

  property("The String protocol using lcs, lcslen and lcsidx read the substring, its length and where the matches are") {
    forAll { (k1: Key, k2: Key, s: String, m: PosInt) =>
      assertEquals(lcs[String](k1, k2).encode, Arr(Bulk("LCS"), Bulk(k1), Bulk(k2)))
      assertEquals(lcs[String](k1, k2).decode(Bulk(s)), s)
      assertEquals(lcslen(k1, k2).encode, Arr(Bulk("LCS"), Bulk(k1), Bulk(k2), Bulk("LEN")))
      assertEquals(
        lcsidx(k1, k2, m).encode,
        Arr(Bulk("LCS"), Bulk(k1), Bulk(k2), Bulk("IDX"), Bulk("MINMATCHLEN"), Bulk(m), Bulk("WITHMATCHLEN"))
      )
    }
  }

  test("The String protocol using lcsidx reads the reply Redis sends") {
    val reply = Arr(
      Bulk("matches"),
      Arr(
        Arr(Arr(Num(4L), Num(7L)), Arr(Num(5L), Num(8L)), Num(4L)),
        Arr(Arr(Num(2L), Num(3L)), Arr(Num(0L), Num(1L)), Num(2L))
      ),
      Bulk("len"),
      Num(6L)
    )
    assertEquals(
      lcsidx(Key("a"), Key("b")).decode(reply),
      LcsMatches(
        Seq(
          LcsMatch(LcsRange(NonNegInt(4), NonNegInt(7)), LcsRange(NonNegInt(5), NonNegInt(8)), NonNegInt(4)),
          LcsMatch(LcsRange(NonNegInt(2), NonNegInt(3)), LcsRange(NonNegInt(0), NonNegInt(1)), NonNegInt(2))
        ),
        NonNegInt(6)
      )
    )
    assertEquals(lcsidx(Key("a"), Key("b")).decode(Arr(Bulk("matches"), Arr(Nil), Bulk("len"), Num(0L))), LcsMatches(Nil, NonNegInt(0)))
  }

  test("The String protocol using stralgolcs sends the keys or the strings, then the option, and reads what it asked for") {
    def words(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)
    val keys                    = StringP.StrAlgoInput.keys(Key("a"), Key("b"))
    val strings                 = StringP.StrAlgoInput.strings(1, 2)
    assertEquals(stralgolcs[String](keys).encode, words("STRALGO", "LCS", "KEYS", "a", "b"))
    assertEquals(stralgolcs[String](strings).encode, words("STRALGO", "LCS", "STRINGS", "1", "2"))
    assertEquals(stralgolcs[String](keys).decode(Bulk("mytext")), "mytext")
    assertEquals(stralgolcslen(keys).encode, words("STRALGO", "LCS", "KEYS", "a", "b", "LEN"))
    assertEquals(stralgolcslen(keys).decode(Num(6L)), NonNegInt(6))
    assertEquals(stralgolcsidx(strings).encode, words("STRALGO", "LCS", "STRINGS", "1", "2", "IDX", "WITHMATCHLEN"))
    assertEquals(
      stralgolcsidx(keys, PosInt(4)).encode,
      words("STRALGO", "LCS", "KEYS", "a", "b", "IDX", "MINMATCHLEN", "4", "WITHMATCHLEN")
    )
    // as Redis 6.2 and KeyDB reply
    val reply = Arr(Bulk("matches"), Arr(Arr(Arr(Num(4L), Num(7L)), Arr(Num(5L), Num(8L)), Num(4L))), Bulk("len"), Num(6L))
    assertEquals(
      stralgolcsidx(keys).decode(reply),
      StringP.LcsMatches(
        Seq(StringP.LcsMatch(StringP.LcsRange(NonNegInt(4), NonNegInt(7)), StringP.LcsRange(NonNegInt(5), NonNegInt(8)), NonNegInt(4))),
        NonNegInt(6)
      )
    )
  }
}
