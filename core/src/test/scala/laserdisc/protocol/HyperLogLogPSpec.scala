/*
 * Copyright (c) 2018-2025 LaserDisc
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

abstract class HyperLogLogPSpec extends BaseSpec with HyperLogLogP {

  property("The HyperLogLog protocol using pfadd roundtrips successfully given key and elements") {
    forAll { (k: Key, es: OneOrMoreKeys, b: Boolean) =>
      val protocol = pfadd(k, es)
      assertEquals(protocol.encode, Arr(Bulk("PFADD") :: Bulk(k) :: es.value.map(Bulk(_))))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  property("The HyperLogLog protocol using pfcount roundtrips successfully given keys") {
    forAll { (ks: OneOrMoreKeys, nni: NonNegInt) =>
      val protocol = pfcount(ks)
      assertEquals(protocol.encode, Arr(Bulk("PFCOUNT") :: ks.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The HyperLogLog protocol using pfmerge roundtrips successfully given two or more source keys and a destination key") {
    forAll { (sks: TwoOrMoreKeys, dk: Key) =>
      val protocol = pfmerge(sks, dk)
      assertEquals(protocol.encode, Arr(Bulk("PFMERGE") :: Bulk(dk) :: sks.value.map(Bulk(_))))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }
}
