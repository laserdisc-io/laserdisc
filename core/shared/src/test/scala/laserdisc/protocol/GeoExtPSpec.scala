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

final class GeoExtPSpec extends GeoPSpec {
  import geotypes.*

  property("The Geo extended protocol using geoadd roundtrips successfully given key and position1") {
    forAll { (k: Key, p1: GeoPosition, nni: NonNegInt) =>
      val protocol = geoadd(k, p1)
      assertEquals(protocol.encode, Arr(Bulk("GEOADD") :: Bulk(k) :: List(p1).flatMap(geoPositionToBulkList)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Geo extended protocol using geoadd roundtrips successfully given key and position1, position2") {
    forAll { (k: Key, p1: GeoPosition, p2: GeoPosition, nni: NonNegInt) =>
      val protocol = geoadd(k, p1, p2)
      assertEquals(protocol.encode, Arr(Bulk("GEOADD") :: Bulk(k) :: List(p1, p2).flatMap(geoPositionToBulkList)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Geo extended protocol using geoadd roundtrips successfully given key and position1, position2, position3") {
    forAll { (k: Key, p1: GeoPosition, p2: GeoPosition, p3: GeoPosition, nni: NonNegInt) =>
      val protocol = geoadd(k, p1, p2, p3)
      assertEquals(protocol.encode, Arr(Bulk("GEOADD") :: Bulk(k) :: List(p1, p2, p3).flatMap(geoPositionToBulkList)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Geo extended protocol using geoadd roundtrips successfully given key and position1, position2, position3, position4") {
    forAll { (k: Key, p1: GeoPosition, p2: GeoPosition, p3: GeoPosition, p4: GeoPosition, nni: NonNegInt) =>
      val protocol = geoadd(k, p1, p2, p3, p4)
      assertEquals(protocol.encode, Arr(Bulk("GEOADD") :: Bulk(k) :: List(p1, p2, p3, p4).flatMap(geoPositionToBulkList)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property(
    "The Geo extended protocol using geoadd roundtrips successfully given key and position1, position2, position3, position4, position5"
  ) {
    forAll { (k: Key, p1: GeoPosition, p2: GeoPosition, p3: GeoPosition, p4: GeoPosition, p5: GeoPosition, nni: NonNegInt) =>
      val protocol = geoadd(k, p1, p2, p3, p4, p5)
      assertEquals(protocol.encode, Arr(Bulk("GEOADD") :: Bulk(k) :: List(p1, p2, p3, p4, p5).flatMap(geoPositionToBulkList)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Geo extended protocol using geohash roundtrips successfully given key and member1") {
    forAll { (k: Key, m1: Key, oghs: OneOrMore[Option[GeoHash]]) =>
      val protocol = geohash(k, m1)
      assertEquals(protocol.encode, Arr(Bulk("GEOHASH") :: Bulk(k) :: List(Bulk(m1))))
      assertEquals(protocol.decode(oneOrMoreGeoHashOptionToArr(oghs)), oghs.value)
    }
  }
  property("The Geo extended protocol using geohash roundtrips successfully given key and member1, member2") {
    forAll { (k: Key, m1: Key, m2: Key, oghs: OneOrMore[Option[GeoHash]]) =>
      val protocol = geohash(k, m1, m2)
      assertEquals(protocol.encode, Arr(Bulk("GEOHASH") :: Bulk(k) :: List(Bulk(m1), Bulk(m2))))
      assertEquals(protocol.decode(oneOrMoreGeoHashOptionToArr(oghs)), oghs.value)
    }
  }
  property("The Geo extended protocol using geohash roundtrips successfully given key and member1, member2, member3") {
    forAll { (k: Key, m1: Key, m2: Key, m3: Key, oghs: OneOrMore[Option[GeoHash]]) =>
      val protocol = geohash(k, m1, m2, m3)
      assertEquals(protocol.encode, Arr(Bulk("GEOHASH") :: Bulk(k) :: List(Bulk(m1), Bulk(m2), Bulk(m3))))
      assertEquals(protocol.decode(oneOrMoreGeoHashOptionToArr(oghs)), oghs.value)
    }
  }
  property("The Geo extended protocol using geohash roundtrips successfully given key and member1, member2, member3, member4") {
    forAll { (k: Key, m1: Key, m2: Key, m3: Key, m4: Key, oghs: OneOrMore[Option[GeoHash]]) =>
      val protocol = geohash(k, m1, m2, m3, m4)
      assertEquals(protocol.encode, Arr(Bulk("GEOHASH") :: Bulk(k) :: List(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4))))
      assertEquals(protocol.decode(oneOrMoreGeoHashOptionToArr(oghs)), oghs.value)
    }
  }
  property("The Geo extended protocol using geohash roundtrips successfully given key and member1, member2, member3, member4, member5") {
    forAll { (k: Key, m1: Key, m2: Key, m3: Key, m4: Key, m5: Key, oghs: OneOrMore[Option[GeoHash]]) =>
      val protocol = geohash(k, m1, m2, m3, m4, m5)
      assertEquals(protocol.encode, Arr(Bulk("GEOHASH") :: Bulk(k) :: List(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5))))
      assertEquals(protocol.decode(oneOrMoreGeoHashOptionToArr(oghs)), oghs.value)
    }
  }
  property("The Geo extended protocol using geopos roundtrips successfully given key and member1") {
    forAll { (k: Key, m1: Key, ocs: OneOrMore[Option[GeoCoordinates]]) =>
      val protocol = geopos(k, m1)
      assertEquals(protocol.encode, Arr(Bulk("GEOPOS") :: Bulk(k) :: List(Bulk(m1))))
      assertEquals(protocol.decode(oneOrMoreGeoCoordinatesOptionToArr(ocs)), ocs.value)
    }
  }
  property("The Geo extended protocol using geopos roundtrips successfully given key and member1, member2") {
    forAll { (k: Key, m1: Key, m2: Key, ocs: OneOrMore[Option[GeoCoordinates]]) =>
      val protocol = geopos(k, m1, m2)
      assertEquals(protocol.encode, Arr(Bulk("GEOPOS") :: Bulk(k) :: List(Bulk(m1), Bulk(m2))))
      assertEquals(protocol.decode(oneOrMoreGeoCoordinatesOptionToArr(ocs)), ocs.value)
    }
  }
  property("The Geo extended protocol using geopos roundtrips successfully given key and member1, member2, member3") {
    forAll { (k: Key, m1: Key, m2: Key, m3: Key, ocs: OneOrMore[Option[GeoCoordinates]]) =>
      val protocol = geopos(k, m1, m2, m3)
      assertEquals(protocol.encode, Arr(Bulk("GEOPOS") :: Bulk(k) :: List(Bulk(m1), Bulk(m2), Bulk(m3))))
      assertEquals(protocol.decode(oneOrMoreGeoCoordinatesOptionToArr(ocs)), ocs.value)
    }
  }
  property("The Geo extended protocol using geopos roundtrips successfully given key and member1, member2, member3, member4") {
    forAll { (k: Key, m1: Key, m2: Key, m3: Key, m4: Key, ocs: OneOrMore[Option[GeoCoordinates]]) =>
      val protocol = geopos(k, m1, m2, m3, m4)
      assertEquals(protocol.encode, Arr(Bulk("GEOPOS") :: Bulk(k) :: List(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4))))
      assertEquals(protocol.decode(oneOrMoreGeoCoordinatesOptionToArr(ocs)), ocs.value)
    }
  }
  property("The Geo extended protocol using geopos roundtrips successfully given key and member1, member2, member3, member4, member5") {
    forAll { (k: Key, m1: Key, m2: Key, m3: Key, m4: Key, m5: Key, ocs: OneOrMore[Option[GeoCoordinates]]) =>
      val protocol = geopos(k, m1, m2, m3, m4, m5)
      assertEquals(protocol.encode, Arr(Bulk("GEOPOS") :: Bulk(k) :: List(Bulk(m1), Bulk(m2), Bulk(m3), Bulk(m4), Bulk(m5))))
      assertEquals(protocol.decode(oneOrMoreGeoCoordinatesOptionToArr(ocs)), ocs.value)
    }
  }
}
