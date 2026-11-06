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

final class ListExtPSpec extends ListPSpec with ExtPArguments {
  property("The List extended protocol using lpush roundtrips successfully given key and value1") {
    forAll { (k: Key, v1: Int, pi: PosInt) =>
      val protocol = lpush(k, v1)
      assertEquals(protocol.encode, Arr(Bulk("LPUSH"), Bulk(k), Bulk(v1)))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }
  property("The List extended protocol using lpush roundtrips successfully given key and value1, value2") {
    forAll { (k: Key, v1: Int, v2: Int, pi: PosInt) =>
      val protocol = lpush(k, v1, v2)
      assertEquals(protocol.encode, Arr(Bulk("LPUSH"), Bulk(k), Bulk(v1), Bulk(v2)))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }
  property("The List extended protocol using lpush roundtrips successfully given key and value1, value2, value3") {
    forAll { (k: Key, v1: Int, v2: Int, v3: Int, pi: PosInt) =>
      val protocol = lpush(k, v1, v2, v3)
      assertEquals(protocol.encode, Arr(Bulk("LPUSH"), Bulk(k), Bulk(v1), Bulk(v2), Bulk(v3)))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }
  property("The List extended protocol using lpush roundtrips successfully given key and value1, value2, value3, value4") {
    forAll { (k: Key, v1: Int, v2: Int, v3: Int, v4: Int, pi: PosInt) =>
      val protocol = lpush(k, v1, v2, v3, v4)
      assertEquals(protocol.encode, Arr(Bulk("LPUSH"), Bulk(k), Bulk(v1), Bulk(v2), Bulk(v3), Bulk(v4)))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }
  property("The List extended protocol using lpush roundtrips successfully given key and value1, value2, value3, value4, value5") {
    forAll { (k: Key, v1: Int, v2: Int, v3: Int, v4: Int, v5: Int, pi: PosInt) =>
      val protocol = lpush(k, v1, v2, v3, v4, v5)
      assertEquals(protocol.encode, Arr(Bulk("LPUSH"), Bulk(k), Bulk(v1), Bulk(v2), Bulk(v3), Bulk(v4), Bulk(v5)))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }
  property("The List extended protocol using rpush roundtrips successfully given key and value1") {
    forAll { (k: Key, v1: Int, pi: PosInt) =>
      val protocol = rpush(k, v1)
      assertEquals(protocol.encode, Arr(Bulk("RPUSH"), Bulk(k), Bulk(v1)))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }
  property("The List extended protocol using rpush roundtrips successfully given key and value1, value2") {
    forAll { (k: Key, v1: Int, v2: Int, pi: PosInt) =>
      val protocol = rpush(k, v1, v2)
      assertEquals(protocol.encode, Arr(Bulk("RPUSH"), Bulk(k), Bulk(v1), Bulk(v2)))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }
  property("The List extended protocol using rpush roundtrips successfully given key and value1, value2, value3") {
    forAll { (k: Key, v1: Int, v2: Int, v3: Int, pi: PosInt) =>
      val protocol = rpush(k, v1, v2, v3)
      assertEquals(protocol.encode, Arr(Bulk("RPUSH"), Bulk(k), Bulk(v1), Bulk(v2), Bulk(v3)))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }
  property("The List extended protocol using rpush roundtrips successfully given key and value1, value2, value3, value4") {
    forAll { (k: Key, v1: Int, v2: Int, v3: Int, v4: Int, pi: PosInt) =>
      val protocol = rpush(k, v1, v2, v3, v4)
      assertEquals(protocol.encode, Arr(Bulk("RPUSH"), Bulk(k), Bulk(v1), Bulk(v2), Bulk(v3), Bulk(v4)))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }
  property("The List extended protocol using rpush roundtrips successfully given key and value1, value2, value3, value4, value5") {
    forAll { (k: Key, v1: Int, v2: Int, v3: Int, v4: Int, v5: Int, pi: PosInt) =>
      val protocol = rpush(k, v1, v2, v3, v4, v5)
      assertEquals(protocol.encode, Arr(Bulk("RPUSH"), Bulk(k), Bulk(v1), Bulk(v2), Bulk(v3), Bulk(v4), Bulk(v5)))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }

  //format: off
  test("The List extended protocol using lpush sends in order from 1 to 21 values") {
    assertEquals(lpush(k, m1).encode, frame("LPUSH", "k", "m1"))
    assertEquals(lpush(k, m1, m2).encode, frame("LPUSH", "k", "m1", "m2"))
    assertEquals(lpush(k, m1, m2, m3).encode, frame("LPUSH", "k", "m1", "m2", "m3"))
    assertEquals(lpush(k, m1, m2, m3, m4).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20", "m21"))
  }
  test("The List extended protocol using rpush sends in order from 1 to 21 values") {
    assertEquals(rpush(k, m1).encode, frame("RPUSH", "k", "m1"))
    assertEquals(rpush(k, m1, m2).encode, frame("RPUSH", "k", "m1", "m2"))
    assertEquals(rpush(k, m1, m2, m3).encode, frame("RPUSH", "k", "m1", "m2", "m3"))
    assertEquals(rpush(k, m1, m2, m3, m4).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20", "m21"))
  }
}
