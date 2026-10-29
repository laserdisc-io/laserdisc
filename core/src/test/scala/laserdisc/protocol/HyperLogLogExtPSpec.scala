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

final class HyperLogLogExtPSpec extends HyperLogLogPSpec {
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
  property("The HyperLogLog extended protocol using pfadd roundtrips successfully given key and element1, element2, element3") {
    forAll { (k: Key, e1: Key, e2: Key, e3: Key, b: Boolean) =>
      val protocol = pfadd(k, e1, e2, e3)
      assertEquals(protocol.encode, Arr(Bulk("PFADD"), Bulk(k), Bulk(e1), Bulk(e2), Bulk(e3)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }
  property("The HyperLogLog extended protocol using pfadd roundtrips successfully given key and element1, element2, element3, element4") {
    forAll { (k: Key, e1: Key, e2: Key, e3: Key, e4: Key, b: Boolean) =>
      val protocol = pfadd(k, e1, e2, e3, e4)
      assertEquals(protocol.encode, Arr(Bulk("PFADD"), Bulk(k), Bulk(e1), Bulk(e2), Bulk(e3), Bulk(e4)))
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
  property("The HyperLogLog extended protocol using pfcount roundtrips successfully given key1, key2, key3") {
    forAll { (k1: Key, k2: Key, k3: Key, nni: NonNegInt) =>
      val protocol = pfcount(k1, k2, k3)
      assertEquals(protocol.encode, Arr(Bulk("PFCOUNT"), Bulk(k1), Bulk(k2), Bulk(k3)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The HyperLogLog extended protocol using pfcount roundtrips successfully given key1, key2, key3, key4") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, nni: NonNegInt) =>
      val protocol = pfcount(k1, k2, k3, k4)
      assertEquals(protocol.encode, Arr(Bulk("PFCOUNT"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The HyperLogLog extended protocol using pfcount roundtrips successfully given key1, key2, key3, key4, key5") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, nni: NonNegInt) =>
      val protocol = pfcount(k1, k2, k3, k4, k5)
      assertEquals(protocol.encode, Arr(Bulk("PFCOUNT"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5)))
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
}
