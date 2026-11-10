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

  //format: off
  test("The List extended protocol using lpush sends in order any number of values") {
    assertEquals(lpush(k, m1).encode, frame("LPUSH", "k", "m1"))
    assertEquals(lpush(k, m1, m2).encode, frame("LPUSH", "k", "m1", "m2"))
    assertEquals(lpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21).encode, frame("LPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20", "m21"))
  }
  test("The List extended protocol using rpush sends in order any number of values") {
    assertEquals(rpush(k, m1).encode, frame("RPUSH", "k", "m1"))
    assertEquals(rpush(k, m1, m2).encode, frame("RPUSH", "k", "m1", "m2"))
    assertEquals(rpush(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21).encode, frame("RPUSH", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20", "m21"))
  }
}
