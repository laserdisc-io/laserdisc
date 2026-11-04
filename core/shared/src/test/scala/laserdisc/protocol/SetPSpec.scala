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
}
