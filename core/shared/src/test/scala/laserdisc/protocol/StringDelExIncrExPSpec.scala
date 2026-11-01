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

final class StringDelExIncrExPSpec extends BaseSpec with StringP {
  import StringP.Incremented

  property("The String protocol using digest reads the hash, or None when the key doesn't exist") {
    forAll { (k: Key, od: Option[String]) =>
      assertEquals(digest(k).encode, Arr(Bulk("DIGEST"), Bulk(k)))
      assertEquals(digest(k).decode(od.fold(NullBulk: GenBulk)(Bulk(_))), od)
    }
  }

  property("The String protocol using delex sends the condition and reads whether it deleted") {
    forAll { (k: Key, v: String, d: String, b: Boolean) =>
      assertEquals(delex(k).encode, Arr(Bulk("DELEX"), Bulk(k)))
      assertEquals(delex(k, strings.ifEq(v)).encode, Arr(Bulk("DELEX"), Bulk(k), Bulk("IFEQ"), Bulk(v)))
      assertEquals(delex(k, strings.compare.ifDne(d)).encode, Arr(Bulk("DELEX"), Bulk(k), Bulk("IFDNE"), Bulk(d)))
      assertEquals(delex(k, strings.compare.ifNe(v)).decode(Num(if (b) 1L else 0L)), b)
    }
  }

  property("The String protocol using delifeq roundtrips successfully given key and value") {
    forAll { (k: Key, v: Int, b: Boolean) =>
      assertEquals(delifeq(k, v).encode, Arr(Bulk("DELIFEQ"), Bulk(k), Bulk(v)))
      assertEquals(delifeq(k, v).decode(Num(if (b) 1L else 0L)), b)
    }
  }

  property("The String protocol using msetex sends the number of keys, the keys and values, the flag and the expiry") {
    forAll { (kvs: OneOrMore[(Key, Int)], s: PosLong, b: Boolean) =>
      val protocol = msetex(kvs, strings.flag.nx, strings.expiry.seconds(s))
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MSETEX") :: Bulk(kvs.value.size) :: kvs.value.flatMap { case (k, v) => List(Bulk(k), Bulk(v)) } :::
            List(Bulk("NX"), Bulk("EX"), Bulk(s))
        )
      )
      assertEquals(protocol.decode(Num(if (b) 1L else 0L)), b)
    }
  }

  property("The String protocol using increx with whole numbers reads the value and the increment applied") {
    forAll { (k: Key, i: Long, l: Long, u: Long, s: PosLong, v: Long, a: Long) =>
      val protocol = increx(k, i, strings.increx.int.lowerBound(l).upperBound(u).saturate.expiryIfNone(strings.expiry.seconds(s)))
      assertEquals(
        protocol.encode,
        Arr(
          List(
            "INCREX",
            k.value,
            "BYINT",
            i.toString,
            "LBOUND",
            l.toString,
            "UBOUND",
            u.toString,
            "SATURATE",
            "EX",
            s.value.toString,
            "ENX"
          )
            .map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(Arr(Num(v), Num(a))), Incremented(v, a))
      assertEquals(increx(k).encode, Arr(Bulk("INCREX"), Bulk(k)))
    }
  }

  property("The String protocol using increx with decimal numbers reads the value and the increment applied") {
    forAll { (k: Key, i: ValidDouble, u: ValidDouble, v: ValidDouble, a: ValidDouble) =>
      val protocol = increx(k, i, strings.increx.float.upperBound(u).expiry(strings.expiry.persist))
      assertEquals(protocol.encode, Arr(Bulk("INCREX"), Bulk(k), Bulk("BYFLOAT"), Bulk(i), Bulk("UBOUND"), Bulk(u), Bulk("PERSIST")))
      assertEquals(protocol.decode(Arr(Bulk(v), Bulk(a))), Incremented(v.value, a.value))
    }
  }
}
