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

final class HashExtPSpec extends HashPSpec with ExtPArguments {
  property("The Hash extended protocol using hdel roundtrips successfully given key and field1") {
    forAll { (k: Key, f1: Key, nni: NonNegInt) =>
      val protocol = hdel(k, f1)
      assertEquals(protocol.encode, Arr(Bulk("HDEL"), Bulk(k), Bulk(f1)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Hash extended protocol using hdel roundtrips successfully given key and field1, field2") {
    forAll { (k: Key, f1: Key, f2: Key, nni: NonNegInt) =>
      val protocol = hdel(k, f1, f2)
      assertEquals(protocol.encode, Arr(Bulk("HDEL"), Bulk(k), Bulk(f1), Bulk(f2)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Hash extended protocol using hdel roundtrips successfully given key and field1, field2, field3") {
    forAll { (k: Key, f1: Key, f2: Key, f3: Key, nni: NonNegInt) =>
      val protocol = hdel(k, f1, f2, f3)
      assertEquals(protocol.encode, Arr(Bulk("HDEL"), Bulk(k), Bulk(f1), Bulk(f2), Bulk(f3)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Hash extended protocol using hdel roundtrips successfully given key and field1, field2, field3, field4") {
    forAll { (k: Key, f1: Key, f2: Key, f3: Key, f4: Key, nni: NonNegInt) =>
      val protocol = hdel(k, f1, f2, f3, f4)
      assertEquals(protocol.encode, Arr(Bulk("HDEL"), Bulk(k), Bulk(f1), Bulk(f2), Bulk(f3), Bulk(f4)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Hash extended protocol using hdel roundtrips successfully given key and field1, field2, field3, field4, field5") {
    forAll { (k: Key, f1: Key, f2: Key, f3: Key, f4: Key, f5: Key, nni: NonNegInt) =>
      val protocol = hdel(k, f1, f2, f3, f4, f5)
      assertEquals(protocol.encode, Arr(Bulk("HDEL"), Bulk(k), Bulk(f1), Bulk(f2), Bulk(f3), Bulk(f4), Bulk(f5)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Hash extended protocol using hmget roundtrips successfully given key and field1") {
    forAll { (k: Key, f1: Key) =>
      forAll { (v1: Int) =>
        val protocol = hmget[Int](k, f1)
        assertEquals(protocol.encode, Arr(Bulk("HMGET"), Bulk(k), Bulk(f1)))
        assertEquals(protocol.decode(Arr(Bulk(v1))), v1)
      }
    }
  }
  property("The Hash extended protocol using hmget roundtrips successfully given key and field1, field2") {
    forAll { (k: Key, f1: Key, f2: Key) =>
      forAll { (v1: Int, v2: Int) =>
        val protocol = hmget[Int, Int](k, f1, f2)
        assertEquals(protocol.encode, Arr(Bulk("HMGET"), Bulk(k), Bulk(f1), Bulk(f2)))
        assertEquals(protocol.decode(Arr(Bulk(v1), Bulk(v2))), (v1, v2))
      }
    }
  }
  property("The Hash extended protocol using hmget roundtrips successfully given key and field1, field2, field3") {
    forAll { (k: Key, f1: Key, f2: Key, f3: Key) =>
      forAll { (v1: Int, v2: Int, v3: Int) =>
        val protocol = hmget[Int, Int, Int](k, f1, f2, f3)
        assertEquals(protocol.encode, Arr(Bulk("HMGET"), Bulk(k), Bulk(f1), Bulk(f2), Bulk(f3)))
        assertEquals(protocol.decode(Arr(Bulk(v1), Bulk(v2), Bulk(v3))), (v1, v2, v3))
      }
    }
  }
  property("The Hash extended protocol using hmget roundtrips successfully given key and field1, field2, field3, field4") {
    forAll { (k: Key, f1: Key, f2: Key, f3: Key, f4: Key) =>
      forAll { (v1: Int, v2: Int, v3: Int, v4: Int) =>
        val protocol = hmget[Int, Int, Int, Int](k, f1, f2, f3, f4)
        assertEquals(protocol.encode, Arr(Bulk("HMGET"), Bulk(k), Bulk(f1), Bulk(f2), Bulk(f3), Bulk(f4)))
        assertEquals(protocol.decode(Arr(Bulk(v1), Bulk(v2), Bulk(v3), Bulk(v4))), (v1, v2, v3, v4))
      }
    }
  }
  property("The Hash extended protocol using hmget roundtrips successfully given key and field1, field2, field3, field4, field5") {
    forAll { (k: Key, f1: Key, f2: Key, f3: Key, f4: Key, f5: Key) =>
      forAll { (v1: Int, v2: Int, v3: Int, v4: Int, v5: Int) =>
        val protocol = hmget[Int, Int, Int, Int, Int](k, f1, f2, f3, f4, f5)
        assertEquals(protocol.encode, Arr(Bulk("HMGET"), Bulk(k), Bulk(f1), Bulk(f2), Bulk(f3), Bulk(f4), Bulk(f5)))
        assertEquals(protocol.decode(Arr(Bulk(v1), Bulk(v2), Bulk(v3), Bulk(v4), Bulk(v5))), (v1, v2, v3, v4, v5))
      }
    }
  }
  property("The Hash extended protocol using hmset roundtrips successfully given key and field1") {
    forAll { (k: Key, f1: Key) =>
      forAll { (v1: Int) =>
        val protocol = hmset(k, f1, v1)
        assertEquals(protocol.encode, Arr(Bulk("HMSET"), Bulk(k), Bulk(f1), Bulk(v1)))
        assertEquals(protocol.decode(Str(OK.value)), OK)
      }
    }
  }
  property("The Hash extended protocol using hmset roundtrips successfully given key and field1, field2") {
    forAll { (k: Key, f1: Key, f2: Key) =>
      forAll { (v1: Int, v2: Int) =>
        val protocol = hmset(k, f1, v1, f2, v2)
        assertEquals(protocol.encode, Arr(Bulk("HMSET"), Bulk(k), Bulk(f1), Bulk(v1), Bulk(f2), Bulk(v2)))
        assertEquals(protocol.decode(Str(OK.value)), OK)
      }
    }
  }
  property("The Hash extended protocol using hmset roundtrips successfully given key and field1, field2, field3") {
    forAll { (k: Key, f1: Key, f2: Key, f3: Key) =>
      forAll { (v1: Int, v2: Int, v3: Int) =>
        val protocol = hmset(k, f1, v1, f2, v2, f3, v3)
        assertEquals(protocol.encode, Arr(Bulk("HMSET"), Bulk(k), Bulk(f1), Bulk(v1), Bulk(f2), Bulk(v2), Bulk(f3), Bulk(v3)))
        assertEquals(protocol.decode(Str(OK.value)), OK)
      }
    }
  }
  property("The Hash extended protocol using hmset roundtrips successfully given key and field1, field2, field3, field4") {
    forAll { (k: Key, f1: Key, f2: Key, f3: Key, f4: Key) =>
      forAll { (v1: Int, v2: Int, v3: Int, v4: Int) =>
        val protocol = hmset(k, f1, v1, f2, v2, f3, v3, f4, v4)
        assertEquals(
          protocol.encode,
          Arr(Bulk("HMSET"), Bulk(k), Bulk(f1), Bulk(v1), Bulk(f2), Bulk(v2), Bulk(f3), Bulk(v3), Bulk(f4), Bulk(v4))
        )
        assertEquals(protocol.decode(Str(OK.value)), OK)
      }
    }
  }
  property("The Hash extended protocol using hmset roundtrips successfully given key and field1, field2, field3, field4, field5") {
    forAll { (k: Key, f1: Key, f2: Key, f3: Key, f4: Key, f5: Key) =>
      forAll { (v1: Int, v2: Int, v3: Int, v4: Int, v5: Int) =>
        val protocol = hmset(k, f1, v1, f2, v2, f3, v3, f4, v4, f5, v5)
        assertEquals(
          protocol.encode,
          Arr(Bulk("HMSET"), Bulk(k), Bulk(f1), Bulk(v1), Bulk(f2), Bulk(v2), Bulk(f3), Bulk(v3), Bulk(f4), Bulk(v4), Bulk(f5), Bulk(v5))
        )
        assertEquals(protocol.decode(Str(OK.value)), OK)
      }
    }
  }

  //format: off
  test("The Hash extended protocol using hdel sends in order from 1 to 21 fields") {
    assertEquals(hdel(k, k1).encode, frame("HDEL", "k", "k1"))
    assertEquals(hdel(k, k1, k2).encode, frame("HDEL", "k", "k1", "k2"))
    assertEquals(hdel(k, k1, k2, k3).encode, frame("HDEL", "k", "k1", "k2", "k3"))
    assertEquals(hdel(k, k1, k2, k3, k4).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
    assertEquals(hdel(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).encode, frame("HDEL", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
  }
  test("The Hash extended protocol using hmget sends in order from 1 to 7 fields") {
    assertEquals(hmget[String](k, k1).encode, frame("HMGET", "k", "k1"))
    assertEquals(hmget[String](k, k1).decode(Arr(Bulk(m1))), m1)
    assertEquals(hmget[String, String](k, k1, k2).encode, frame("HMGET", "k", "k1", "k2"))
    assertEquals(hmget[String, String](k, k1, k2).decode(Arr(Bulk(m1), Bulk(m2))), (m1, m2))
    assertEquals(hmget[String, String, String](k, k1, k2, k3).encode, frame("HMGET", "k", "k1", "k2", "k3"))
    assertEquals(hmget[String, String, String](k, k1, k2, k3).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3))), (m1, m2, m3))
    assertEquals(hmget[String, String, String, String](k, k1, k2, k3, k4).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4"))
    assertEquals(hmget[String, String, String, String](k, k1, k2, k3, k4).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4))), (m1, m2, m3, m4))
    assertEquals(hmget[String, String, String, String, String](k, k1, k2, k3, k4, k5).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(hmget[String, String, String, String, String](k, k1, k2, k3, k4, k5).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5))), (m1, m2, m3, m4, m5))
    assertEquals(hmget[String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(hmget[String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6))), (m1, m2, m3, m4, m5, m6))
    assertEquals(hmget[String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(hmget[String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7))), (m1, m2, m3, m4, m5, m6, m7))
  }
  test("The Hash extended protocol using hmget sends in order from 8 to 14 fields") {
    assertEquals(hmget[String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(hmget[String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8))), (m1, m2, m3, m4, m5, m6, m7, m8))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9))), (m1, m2, m3, m4, m5, m6, m7, m8, m9))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10), Bulk(m11))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10), Bulk(m11), Bulk(m12))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10), Bulk(m11), Bulk(m12), Bulk(m13))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10), Bulk(m11), Bulk(m12), Bulk(m13), Bulk(m14))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14))
  }
  test("The Hash extended protocol using hmget sends in order from 15 to 21 fields") {
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10), Bulk(m11), Bulk(m12), Bulk(m13), Bulk(m14), Bulk(m15))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10), Bulk(m11), Bulk(m12), Bulk(m13), Bulk(m14), Bulk(m15), Bulk(m16))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10), Bulk(m11), Bulk(m12), Bulk(m13), Bulk(m14), Bulk(m15), Bulk(m16), Bulk(m17))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10), Bulk(m11), Bulk(m12), Bulk(m13), Bulk(m14), Bulk(m15), Bulk(m16), Bulk(m17), Bulk(m18))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10), Bulk(m11), Bulk(m12), Bulk(m13), Bulk(m14), Bulk(m15), Bulk(m16), Bulk(m17), Bulk(m18), Bulk(m19))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10), Bulk(m11), Bulk(m12), Bulk(m13), Bulk(m14), Bulk(m15), Bulk(m16), Bulk(m17), Bulk(m18), Bulk(m19), Bulk(m20))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).encode, frame("HMGET", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
    assertEquals(hmget[String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String, String](k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).decode(Arr(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5), Bulk(m6), Bulk(m7), Bulk(m8), Bulk(m9), Bulk(m10), Bulk(m11), Bulk(m12), Bulk(m13), Bulk(m14), Bulk(m15), Bulk(m16), Bulk(m17), Bulk(m18), Bulk(m19), Bulk(m20), Bulk(m21))), (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21))
  }
  test("The Hash extended protocol using hmset sends in order from 1 to 21 fields") {
    assertEquals(hmset(k, k1, m1).encode, frame("HMSET", "k", "k1", "m1"))
    assertEquals(hmset(k, k1, m1, k2, m2).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18, k19, m19).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18", "k19", "m19"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18, k19, m19, k20, m20).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18", "k19", "m19", "k20", "m20"))
    assertEquals(hmset(k, k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18, k19, m19, k20, m20, k21, m21).encode, frame("HMSET", "k", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18", "k19", "m19", "k20", "m20", "k21", "m21"))
  }
}
