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

final class GeoExtPSpec extends GeoPSpec with ExtPArguments {
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

  //format: off
  private val p1 = GeoPosition(k1, Latitude(10.5), Longitude(20.5))
  private val p2 = GeoPosition(k2, Latitude(10.5), Longitude(20.5))
  private val p3 = GeoPosition(k3, Latitude(10.5), Longitude(20.5))
  private val p4 = GeoPosition(k4, Latitude(10.5), Longitude(20.5))
  private val p5 = GeoPosition(k5, Latitude(10.5), Longitude(20.5))
  private val p6 = GeoPosition(k6, Latitude(10.5), Longitude(20.5))
  private val p7 = GeoPosition(k7, Latitude(10.5), Longitude(20.5))
  private val p8 = GeoPosition(k8, Latitude(10.5), Longitude(20.5))
  private val p9 = GeoPosition(k9, Latitude(10.5), Longitude(20.5))
  private val p10 = GeoPosition(k10, Latitude(10.5), Longitude(20.5))
  private val p11 = GeoPosition(k11, Latitude(10.5), Longitude(20.5))
  private val p12 = GeoPosition(k12, Latitude(10.5), Longitude(20.5))
  private val p13 = GeoPosition(k13, Latitude(10.5), Longitude(20.5))
  private val p14 = GeoPosition(k14, Latitude(10.5), Longitude(20.5))
  private val p15 = GeoPosition(k15, Latitude(10.5), Longitude(20.5))
  private val p16 = GeoPosition(k16, Latitude(10.5), Longitude(20.5))
  private val p17 = GeoPosition(k17, Latitude(10.5), Longitude(20.5))
  private val p18 = GeoPosition(k18, Latitude(10.5), Longitude(20.5))
  private val p19 = GeoPosition(k19, Latitude(10.5), Longitude(20.5))
  private val p20 = GeoPosition(k20, Latitude(10.5), Longitude(20.5))
  private val p21 = GeoPosition(k21, Latitude(10.5), Longitude(20.5))

  test("The Geo extended protocol using geoadd sends in order any number of positions") {
    assertEquals(geoadd(k, p1).encode, frame("GEOADD", "k", "20.5", "10.5", "k1"))
    assertEquals(geoadd(k, p1, p2).encode, frame("GEOADD", "k", "20.5", "10.5", "k1", "20.5", "10.5", "k2"))
    assertEquals(geoadd(k, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21).encode, frame("GEOADD", "k", "20.5", "10.5", "k1", "20.5", "10.5", "k2", "20.5", "10.5", "k3", "20.5", "10.5", "k4", "20.5", "10.5", "k5", "20.5", "10.5", "k6", "20.5", "10.5", "k7", "20.5", "10.5", "k8", "20.5", "10.5", "k9", "20.5", "10.5", "k10", "20.5", "10.5", "k11", "20.5", "10.5", "k12", "20.5", "10.5", "k13", "20.5", "10.5", "k14", "20.5", "10.5", "k15", "20.5", "10.5", "k16", "20.5", "10.5", "k17", "20.5", "10.5", "k18", "20.5", "10.5", "k19", "20.5", "10.5", "k20", "20.5", "10.5", "k21"))
  }
  test("The Geo extended protocol using geohash sends in order any number of members") {
    assertEquals(geohash(k, k1).encode, frame("GEOHASH", "k", "k1"))
    assertEquals(geohash(k, k1, k2).encode, frame("GEOHASH", "k", "k1", "k2"))
    assertEquals(geohash(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).encode, frame("GEOHASH", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
  }
  test("The Geo extended protocol using geopos sends in order any number of members") {
    assertEquals(geopos(k, k1).encode, frame("GEOPOS", "k", "k1"))
    assertEquals(geopos(k, k1, k2).encode, frame("GEOPOS", "k", "k1", "k2"))
    assertEquals(geopos(k, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).encode, frame("GEOPOS", "k", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
  }
}
