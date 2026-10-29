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

final class ListExtPSpec extends ListPSpec {
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
}
