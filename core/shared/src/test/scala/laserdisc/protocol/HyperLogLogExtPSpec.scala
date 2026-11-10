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

final class HyperLogLogExtPSpec extends HyperLogLogPSpec with ExtPArguments {
  property("The HyperLogLog extended protocol using pfadd roundtrips successfully given key and element1") {
    forAll { (k: Key, e1: Key, b: Boolean) =>
      val protocol = pfadd(k, e1)
      assertEquals(protocol.encode, Arr(Bulk("PFADD"), Bulk(k), Bulk(e1)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }
  property("The HyperLogLog extended protocol using pfadd roundtrips successfully given key and element1, element2") {
    forAll { (k: Key, e1: Key, e2: Key, b: Boolean) =>
      val protocol = pfadd(k, e1, e2)
      assertEquals(protocol.encode, Arr(Bulk("PFADD"), Bulk(k), Bulk(e1), Bulk(e2)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }
  property(
    "The HyperLogLog extended protocol using pfadd roundtrips successfully given key and element1, element2, element3, element4, element5"
  ) {
    forAll { (k: Key, e1: Key, e2: Key, e3: Key, e4: Key, e5: Key, b: Boolean) =>
      val protocol = pfadd(k, e1, e2, e3, e4, e5)
      assertEquals(protocol.encode, Arr(Bulk("PFADD"), Bulk(k), Bulk(e1), Bulk(e2), Bulk(e3), Bulk(e4), Bulk(e5)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }
  property("The HyperLogLog extended protocol using pfcount roundtrips successfully given key1") {
    forAll { (k1: Key, nni: NonNegInt) =>
      val protocol = pfcount(k1)
      assertEquals(protocol.encode, Arr(Bulk("PFCOUNT"), Bulk(k1)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The HyperLogLog extended protocol using pfcount roundtrips successfully given key1, key2") {
    forAll { (k1: Key, k2: Key, nni: NonNegInt) =>
      val protocol = pfcount(k1, k2)
      assertEquals(protocol.encode, Arr(Bulk("PFCOUNT"), Bulk(k1), Bulk(k2)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The HyperLogLog extended protocol using pfmerge roundtrips successfully given sourcekey1, sourcekey2 and destinationkey") {
    forAll { (sk1: Key, sk2: Key, dk: Key) =>
      val protocol = pfmerge(sk1, sk2, dk)
      assertEquals(protocol.encode, Arr(Bulk("PFMERGE"), Bulk(dk), Bulk(sk1), Bulk(sk2)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }
  property(
    "The HyperLogLog extended protocol using pfmerge roundtrips successfully given sourcekey1, sourcekey2, sourcekey3 and destinationkey"
  ) {
    forAll { (sk1: Key, sk2: Key, sk3: Key, dk: Key) =>
      val protocol = pfmerge(sk1, sk2, sk3, dk)
      assertEquals(protocol.encode, Arr(Bulk("PFMERGE"), Bulk(dk), Bulk(sk1), Bulk(sk2), Bulk(sk3)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }
  property(
    "The HyperLogLog extended protocol using pfmerge roundtrips successfully given sourcekey1, sourcekey2, sourcekey3, sourcekey4 and destinationkey"
  ) {
    forAll { (sk1: Key, sk2: Key, sk3: Key, sk4: Key, dk: Key) =>
      val protocol = pfmerge(sk1, sk2, sk3, sk4, dk)
      assertEquals(protocol.encode, Arr(Bulk("PFMERGE"), Bulk(dk), Bulk(sk1), Bulk(sk2), Bulk(sk3), Bulk(sk4)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }
  property(
    "The HyperLogLog extended protocol using pfmerge roundtrips successfully given sourcekey1, sourcekey2, sourcekey3, sourcekey4, sourcekey5 and destinationkey"
  ) {
    forAll { (sk1: Key, sk2: Key, sk3: Key, sk4: Key, sk5: Key, dk: Key) =>
      val protocol = pfmerge(sk1, sk2, sk3, sk4, sk5, dk)
      assertEquals(protocol.encode, Arr(Bulk("PFMERGE"), Bulk(dk), Bulk(sk1), Bulk(sk2), Bulk(sk3), Bulk(sk4), Bulk(sk5)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  //format: off
  test("The HyperLogLog extended protocol using pfadd sends in order any number of elements") {
    assertEquals(pfadd(k, k1).encode, frame("PFADD", "k", "k1"))
    assertEquals(pfadd(k, k1, k2).encode, frame("PFADD", "k", "k1", "k2"))
    assertEquals(pfadd(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).encode, frame("PFADD", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
  }
  test("The HyperLogLog extended protocol using pfcount sends in order any number of keys") {
    assertEquals(pfcount(k1).encode, frame("PFCOUNT", "k1"))
    assertEquals(pfcount(k1, k2).encode, frame("PFCOUNT", "k1", "k2"))
    assertEquals(pfcount(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).encode, frame("PFCOUNT", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22"))
  }
  test("The HyperLogLog extended protocol using pfmerge sends in order from 2 to 21 keys") {
    assertEquals(pfmerge(k1, k2, d).encode, frame("PFMERGE", "d", "k1", "k2"))
    assertEquals(pfmerge(k1, k2, k3, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3"))
    assertEquals(pfmerge(k1, k2, k3, k4, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
    assertEquals(pfmerge(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, d).encode, frame("PFMERGE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
  }
}
