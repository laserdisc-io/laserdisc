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

final class SetPSpec extends BaseSpec with SetP {
  private def bulks(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)

  property("The Set protocol using smismember reads whether each member is in the set") {
    forAll { (k: Key, ms: OneOrMore[String], bs: List[Boolean]) =>
      val protocol = smismember(k, ms)
      assertEquals(protocol.encode, Arr(Bulk("SMISMEMBER") :: Bulk(k) :: ms.value.map(Bulk(_))))
      assertEquals(protocol.decode(Arr(bs.map(b => Num(if (b) 1L else 0L)))), bs)
    }
  }

  property("The Set protocol using sintercard roundtrips successfully given keys and limit") {
    forAll { (ks: TwoOrMoreKeys, l: PosInt, n: NonNegInt) =>
      val protocol = sintercard(ks, l)
      assertEquals(
        protocol.encode,
        Arr(Bulk("SINTERCARD") :: Bulk(ks.value.size) :: ks.value.map(Bulk(_)) ::: List(Bulk("LIMIT"), Bulk(l)))
      )
      assertEquals(protocol.decode(Num(n.value.toLong)), n)
    }
  }

  property("The Set protocol using sdiffcard roundtrips successfully given keys") {
    forAll { (ks: TwoOrMoreKeys, n: NonNegInt) =>
      val protocol = sdiffcard(ks)
      assertEquals(protocol.encode, Arr(Bulk("SDIFFCARD") :: Bulk(ks.value.size) :: ks.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(n.value.toLong)), n)
    }
  }

  property("The Set protocol using sunioncard sends APPROX before LIMIT") {
    forAll { (ks: TwoOrMoreKeys, l: PosInt, n: NonNegInt) =>
      val protocol = sunioncard(ks, settypes.approx, l)
      assertEquals(
        protocol.encode,
        Arr(Bulk("SUNIONCARD") :: Bulk(ks.value.size) :: ks.value.map(Bulk(_)) ::: List(Bulk("APPROX"), Bulk("LIMIT"), Bulk(l)))
      )
      assertEquals(protocol.decode(Num(n.value.toLong)), n)
    }
  }

  property("The Set protocol using scard reads None for an empty set") {
    forAll { (k: Key, n: PosInt) =>
      val protocol = scard(k)
      assertEquals(protocol.encode, Arr(Bulk("SCARD"), Bulk(k)))
      assertEquals(protocol.decode(Num(n.value.toLong)), Some(n))
      assertEquals(protocol.decode(Num(0L)), None)
    }
  }

  property("The Set protocol using sismember and smove read whether it was there") {
    forAll { (k: Key, d: Key, m: String) =>
      assertEquals(sismember(k, m).encode, Arr(Bulk("SISMEMBER"), Bulk(k), Bulk(m)))
      assertEquals(sismember(k, m).decode(Num(1L)), true)
      assertEquals(sismember(k, m).decode(Num(0L)), false)
      assertEquals(smove(k, d, m).encode, Arr(Bulk("SMOVE"), Bulk(k), Bulk(d), Bulk(m)))
      assertEquals(smove(k, d, m).decode(Num(1L)), true)
      assertEquals(smove(k, d, m).decode(Num(0L)), false)
    }
  }

  property("The Set protocol using smembers reads the members") {
    forAll { (k: Key, ms: List[String]) =>
      val protocol = smembers[String](k)
      assertEquals(protocol.encode, Arr(Bulk("SMEMBERS"), Bulk(k)))
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Set protocol using srandmember reads a member or None, and srandmembers several") {
    forAll { (k: Key, c: NonZeroInt, om: Option[String], ms: List[String]) =>
      assertEquals(srandmember[String](k).encode, Arr(Bulk("SRANDMEMBER"), Bulk(k)))
      assertEquals(srandmember[String](k).decode(om.fold(NullBulk: GenBulk)(Bulk(_))), om)
      assertEquals(srandmembers[String](k, c).encode, Arr(Bulk("SRANDMEMBER"), Bulk(k), Bulk(c)))
      assertEquals(srandmembers[String](k, c).decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Set protocol using srem sends the members and reads how many it removed") {
    forAll { (k: Key, ms: OneOrMore[String], n: NonNegInt) =>
      val protocol = srem(k, ms)
      assertEquals(protocol.encode, Arr(Bulk("SREM") :: Bulk(k) :: ms.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(n.value.toLong)), n)
    }
  }

  property("The Set protocol using sdiff, sinter and sunion sends the keys and reads the members") {
    forAll { (ks: TwoOrMoreKeys, ms: List[String]) =>
      val reply = Arr(ms.map(Bulk(_)))
      assertEquals(sdiff[String](ks).encode, Arr(Bulk("SDIFF") :: ks.value.map(Bulk(_))))
      assertEquals(sdiff[String](ks).decode(reply), ms)
      assertEquals(sinter[String](ks).encode, Arr(Bulk("SINTER") :: ks.value.map(Bulk(_))))
      assertEquals(sinter[String](ks).decode(reply), ms)
      assertEquals(sunion[String](ks).encode, Arr(Bulk("SUNION") :: ks.value.map(Bulk(_))))
      assertEquals(sunion[String](ks).decode(reply), ms)
    }
  }

  property("The Set protocol using sdiffstore, sinterstore and sunionstore sends the destination first and reads how many it stored") {
    forAll { (ks: TwoOrMoreKeys, d: Key, n: NonNegInt) =>
      val reply = Num(n.value.toLong)
      assertEquals(sdiffstore(ks, d).encode, Arr(Bulk("SDIFFSTORE") :: Bulk(d) :: ks.value.map(Bulk(_))))
      assertEquals(sdiffstore(ks, d).decode(reply), n)
      assertEquals(sinterstore(ks, d).encode, Arr(Bulk("SINTERSTORE") :: Bulk(d) :: ks.value.map(Bulk(_))))
      assertEquals(sinterstore(ks, d).decode(reply), n)
      assertEquals(sunionstore(ks, d).encode, Arr(Bulk("SUNIONSTORE") :: Bulk(d) :: ks.value.map(Bulk(_))))
      assertEquals(sunionstore(ks, d).decode(reply), n)
    }
  }

  test("The Set protocol using sscan sends each of its options, and reads members") {
    val (k, c, g, n) = (Key("k"), NonNegLong(5L), GlobPattern("a*"), PosInt(2))
    val reply        = Arr(Bulk("0"), Arr(Bulk("a")))
    val scan         = Scan(NonNegLong(0L), Some(Seq("a")))

    assertEquals(sscan[String](k, c).encode, bulks("SSCAN", "k", "5"))
    assertEquals(sscan[String](k, c, g).encode, bulks("SSCAN", "k", "5", "MATCH", "a*"))
    assertEquals(sscan[String](k, c, n).encode, bulks("SSCAN", "k", "5", "COUNT", "2"))
    assertEquals(sscan[String](k, c, g, n).encode, bulks("SSCAN", "k", "5", "MATCH", "a*", "COUNT", "2"))
    assertEquals(sscan[String](k, c, g).decode(reply), scan)
    assertEquals(sscan[String](k, c, g, n).decode(reply), scan)
  }
}
