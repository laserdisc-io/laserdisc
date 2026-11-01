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

abstract class GeoPSpec extends BaseSpec with GeoP {
  import geotypes.*
  import org.scalacheck.{Arbitrary, Gen}
  import org.scalacheck.Arbitrary.arbitrary
  import org.scalacheck.Gen.listOf

  protected implicit final val geoHashShow: Show[GeoHash] = Show.unsafeFromToString

  protected implicit final val geoCoordinatesArb: Arbitrary[GeoCoordinates] = Arbitrary {
    for {
      lat  <- arbitrary[Latitude]
      long <- arbitrary[Longitude]
    } yield GeoCoordinates(lat, long)
  }
  protected implicit final val geoPositionArb: Arbitrary[GeoPosition] = Arbitrary {
    for {
      m    <- arbitrary[Key]
      lat  <- arbitrary[Latitude]
      long <- arbitrary[Longitude]
    } yield GeoPosition(m, lat, long)
  }
  protected implicit final val geoUnitArb: Arbitrary[GeoUnit] = Arbitrary {
    Gen.oneOf(GeoUnit.meters, GeoUnit.kilometers, GeoUnit.miles, GeoUnit.feet)
  }

  protected final val geoPositionToBulkList: GeoPosition => List[Bulk] = { case GeoPosition(m, lat, long) =>
    Bulk(long) :: Bulk(lat) :: Bulk(m) :: Nil
  }
  protected final val nonNegDoubleOptionToBulk: Option[NonNegDouble] => GenBulk                       = _.fold(NullBulk: GenBulk)(Bulk(_))
  protected final val oneOrMoreGeoCoordinatesOptionToArr: OneOrMore[Option[GeoCoordinates]] => GenArr = _.value.foldLeft(NilArr: GenArr) {
    case (NilArr, Some(GeoCoordinates(lat, long))) => Arr(Arr(Bulk(long), Bulk(lat)))
    case (NilArr, None)                            => Arr(NilArr)
    case (Arr(e), Some(GeoCoordinates(lat, long))) => Arr(e :+ Arr(Bulk(long), Bulk(lat)))
    case (Arr(e), None)                            => Arr(e :+ NilArr)
  }
  protected final val oneOrMoreGeoHashOptionToArr: OneOrMore[Option[GeoHash]] => GenArr = _.value.foldLeft(NilArr: GenArr) {
    case (NilArr, Some(gh)) => Arr(Bulk(gh))
    case (NilArr, None)     => Arr(NullBulk)
    case (Arr(e), Some(gh)) => Arr(e :+ Bulk(gh))
    case (Arr(e), None)     => Arr(e :+ NullBulk)
  }

  private implicit final val geoKeyAndCoordArb: Arbitrary[GeoKeyAndCoord] = Arbitrary {
    for {
      k <- arbitrary[Key]
      c <- arbitrary[GeoCoordinates]
    } yield GeoKeyAndCoord(k, c)
  }
  private implicit final val geoKeyAndDistArb: Arbitrary[GeoKeyAndDist] = Arbitrary {
    for {
      k <- arbitrary[Key]
      d <- arbitrary[NonNegDouble]
    } yield GeoKeyAndDist(k, d)
  }
  private implicit final val geoKeyAndHashArb: Arbitrary[GeoKeyAndHash] = Arbitrary {
    for {
      k <- arbitrary[Key]
      l <- arbitrary[NonNegLong]
    } yield GeoKeyAndHash(k, l)
  }
  private implicit final val geoKeyCoordAndDistArb: Arbitrary[GeoKeyCoordAndDist] = Arbitrary {
    for {
      k <- arbitrary[Key]
      c <- arbitrary[GeoCoordinates]
      d <- arbitrary[NonNegDouble]
    } yield GeoKeyCoordAndDist(k, c, d)
  }
  private implicit final val geoKeyCoordAndHashArb: Arbitrary[GeoKeyCoordAndHash] = Arbitrary {
    for {
      k <- arbitrary[Key]
      c <- arbitrary[GeoCoordinates]
      l <- arbitrary[NonNegLong]
    } yield GeoKeyCoordAndHash(k, c, l)
  }
  private implicit final val geoKeyDistAndHashArb: Arbitrary[GeoKeyDistAndHash] = Arbitrary {
    for {
      k <- arbitrary[Key]
      d <- arbitrary[NonNegDouble]
      l <- arbitrary[NonNegLong]
    } yield GeoKeyDistAndHash(k, d, l)
  }
  private implicit final val geoKeyCoordDistAndHashArb: Arbitrary[GeoKeyCoordDistAndHash] = Arbitrary {
    for {
      k <- arbitrary[Key]
      c <- arbitrary[GeoCoordinates]
      d <- arbitrary[NonNegDouble]
      l <- arbitrary[NonNegLong]
    } yield GeoKeyCoordDistAndHash(k, c, d, l)
  }
  private implicit final val geoRadiusModeArb: Arbitrary[(GeoRadiusMode, List[?])] = Arbitrary {
    Gen.oneOf(
      listOf(geoKeyAndCoordArb.arbitrary).map(GeoRadiusMode.coordinates -> _),
      listOf(geoKeyAndDistArb.arbitrary).map(GeoRadiusMode.distance -> _),
      listOf(geoKeyAndHashArb.arbitrary).map(GeoRadiusMode.hash -> _),
      listOf(geoKeyCoordAndDistArb.arbitrary).map(GeoRadiusMode.coordinatesAndDistance -> _),
      listOf(geoKeyCoordAndHashArb.arbitrary).map(GeoRadiusMode.coordinatesAndHash -> _),
      listOf(geoKeyDistAndHashArb.arbitrary).map(GeoRadiusMode.distanceAndHash -> _),
      listOf(geoKeyCoordDistAndHashArb.arbitrary).map(GeoRadiusMode.all -> _)
    )
  }
  private implicit final val geoStoreModeArb: Arbitrary[GeoStoreMode] = Arbitrary {
    Gen.oneOf(
      arbitrary[Key].map(GeoStoreHash(_)),
      arbitrary[Key].map(GeoStoreDistance(_)),
      arbitrary[Key].flatMap(hk => arbitrary[Key].map(GeoStoreBoth(hk, _)))
    )
  }

  private final val listToArr: List[?] => Arr = l =>
    Arr(l.collect {
      case GeoKeyAndCoord(k, GeoCoordinates(lat, long))               => Arr(Bulk(k), Arr(Bulk(long), Bulk(lat)))
      case GeoKeyAndDist(k, d)                                        => Arr(Bulk(k), Bulk(d))
      case GeoKeyAndHash(k, h)                                        => Arr(Bulk(k), Num(h.value))
      case GeoKeyCoordAndDist(k, GeoCoordinates(lat, long), d)        => Arr(Bulk(k), Bulk(d), Arr(Bulk(long), Bulk(lat)))
      case GeoKeyCoordAndHash(k, GeoCoordinates(lat, long), h)        => Arr(Bulk(k), Num(h.value), Arr(Bulk(long), Bulk(lat)))
      case GeoKeyDistAndHash(k, d, h)                                 => Arr(Bulk(k), Bulk(d), Num(h.value))
      case GeoKeyCoordDistAndHash(k, GeoCoordinates(lat, long), d, h) => Arr(Bulk(k), Bulk(d), Num(h.value), Arr(Bulk(long), Bulk(lat)))
    })

  property("The Geo protocol using geoadd roundtrips successfully given key and positions") {
    forAll { (k: Key, ps: OneOrMore[GeoPosition], nni: NonNegInt) =>
      val protocol = geoadd(k, ps)
      assertEquals(protocol.encode, Arr(Bulk("GEOADD") :: Bulk(k) :: ps.value.flatMap(geoPositionToBulkList)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Geo protocol using geodist roundtrips successfully given key and members") {
    forAll { (k: Key, m1: Key, m2: Key, onnd: Option[NonNegDouble]) =>
      val protocol = geodist(k, m1, m2)
      assertEquals(protocol.encode, Arr(Bulk("GEODIST"), Bulk(k), Bulk(m1), Bulk(m2)))
      assertEquals(protocol.decode(nonNegDoubleOptionToBulk(onnd)), onnd)
    }
  }

  property("The Geo protocol using geodist roundtrips successfully given key, members and unit") {
    forAll { (k: Key, m1: Key, m2: Key, u: GeoUnit, onnd: Option[NonNegDouble]) =>
      val protocol = geodist(k, m1, m2, u)
      assertEquals(protocol.encode, Arr(Bulk("GEODIST"), Bulk(k), Bulk(m1), Bulk(m2), Bulk(u)))
      assertEquals(protocol.decode(nonNegDoubleOptionToBulk(onnd)), onnd)
    }
  }

  property("The Geo protocol using geohash roundtrips successfully given key and members") {
    forAll { (k: Key, ms: OneOrMoreKeys, oghs: OneOrMore[Option[GeoHash]]) =>
      val protocol = geohash(k, ms)
      assertEquals(protocol.encode, Arr(Bulk("GEOHASH") :: Bulk(k) :: ms.value.map(m => Bulk(m))))
      assertEquals(protocol.decode(oneOrMoreGeoHashOptionToArr(oghs)), oghs.value)
    }
  }

  property("The Geo protocol using geopos roundtrips successfully given key and members") {
    forAll { (k: Key, ms: OneOrMoreKeys, ocs: OneOrMore[Option[GeoCoordinates]]) =>
      val protocol = geopos(k, ms)
      assertEquals(protocol.encode, Arr(Bulk("GEOPOS") :: Bulk(k) :: ms.value.map(m => Bulk(m))))
      assertEquals(protocol.decode(oneOrMoreGeoCoordinatesOptionToArr(ocs)), ocs.value)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, coordinates, radius and unit") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, ms: List[Key]) =>
      val protocol = georadius(k, c, r, u)
      assertEquals(protocol.encode, Arr(Bulk("GEORADIUS"), Bulk(k), Bulk(c.longitude), Bulk(c.latitude), Bulk(r), Bulk(u)))
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, coordinates, radius, unit and limit") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, ms: List[Key]) =>
      val protocol = georadius(k, c, r, u, l)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS"),
          Bulk(k),
          Bulk(c.longitude),
          Bulk(c.latitude),
          Bulk(r),
          Bulk(u),
          Bulk("COUNT"),
          Bulk(l)
        )
      )
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, coordinates, radius, unit and direction") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, ms: List[Key]) =>
      val protocol = georadius(k, c, r, u, d)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS"),
          Bulk(k),
          Bulk(c.longitude),
          Bulk(c.latitude),
          Bulk(r),
          Bulk(u),
          Bulk(d)
        )
      )
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, coordinates, radius, unit, limit and direction") {
    forAll { (input: (Key, GeoCoordinates, NonNegDouble, GeoUnit, PosInt, Direction), ms: List[Key]) =>
      val (k, c, r, u, l, d) = input
      val protocol           = georadius(k, c, r, u, l, d)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS"),
          Bulk(k),
          Bulk(c.longitude),
          Bulk(c.latitude),
          Bulk(r),
          Bulk(u),
          Bulk("COUNT"),
          Bulk(l),
          Bulk(d)
        )
      )
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, coordinates, radius, unit and radius mode") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = georadius(k, c, r, u, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS") ::
            Bulk(k) ::
            Bulk(c.longitude) ::
            Bulk(c.latitude) ::
            Bulk(r) ::
            Bulk(u) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, coordinates, radius, unit, limit and radius mode") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = georadius(k, c, r, u, l, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS") ::
            Bulk(k) ::
            Bulk(c.longitude) ::
            Bulk(c.latitude) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk("COUNT") ::
            Bulk(l) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, coordinates, radius, unit, direction and radius mode") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = georadius(k, c, r, u, d, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS") ::
            Bulk(k) ::
            Bulk(c.longitude) ::
            Bulk(c.latitude) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk(d) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property(
    "The Geo protocol using georadius roundtrips successfully given key, coordinates, radius, unit, limit, direction and radius mode"
  ) {
    forAll { (input: (Key, GeoCoordinates, NonNegDouble, GeoUnit, PosInt, Direction), rmAndRes: (GeoRadiusMode, List[?])) =>
      val (k, c, r, u, l, d) = input
      val (rm, res)          = rmAndRes
      implicit val ev        = rm.r
      val protocol           = georadius(k, c, r, u, l, d, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS") ::
            Bulk(k) ::
            Bulk(c.longitude) ::
            Bulk(c.latitude) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk("COUNT") ::
            Bulk(l) ::
            Bulk(d) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, coordinates, radius, unit and store mode") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, sm: GeoStoreMode, nni: NonNegInt) =>
      val protocol = georadius(k, c, r, u, sm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS") ::
            Bulk(k) ::
            Bulk(c.longitude) ::
            Bulk(c.latitude) ::
            Bulk(r) ::
            Bulk(u) ::
            sm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, coordinates, radius, unit, limit and store mode") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, sm: GeoStoreMode) =>
      forAll { (nni: NonNegInt) =>
        val protocol = georadius(k, c, r, u, l, sm)
        assertEquals(
          protocol.encode,
          Arr(
            Bulk("GEORADIUS") ::
              Bulk(k) ::
              Bulk(c.longitude) ::
              Bulk(c.latitude) ::
              Bulk(r) ::
              Bulk(u) ::
              Bulk("COUNT") ::
              Bulk(l) ::
              sm.params.map(Bulk(_))
          )
        )
        assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
      }
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, coordinates, radius, unit, direction and store mode") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, sm: GeoStoreMode) =>
      forAll { (nni: NonNegInt) =>
        val protocol = georadius(k, c, r, u, d, sm)
        assertEquals(
          protocol.encode,
          Arr(
            Bulk("GEORADIUS") ::
              Bulk(k) ::
              Bulk(c.longitude) ::
              Bulk(c.latitude) ::
              Bulk(r) ::
              Bulk(u) ::
              Bulk(d) ::
              sm.params.map(Bulk(_))
          )
        )
        assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
      }
    }
  }

  property(
    "The Geo protocol using georadius roundtrips successfully given key, coordinates, radius, unit, limit, direction and store mode"
  ) {
    forAll { (input: (Key, GeoCoordinates, NonNegDouble, GeoUnit, PosInt, Direction), sm: GeoStoreMode, nni: NonNegInt) =>
      val (k, c, r, u, l, d) = input
      val protocol           = georadius(k, c, r, u, l, d, sm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS") ::
            Bulk(k) ::
            Bulk(c.longitude) ::
            Bulk(c.latitude) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk("COUNT") ::
            Bulk(l) ::
            Bulk(d) ::
            sm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius and unit") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, ms: List[Key]) =>
      val protocol = georadius(k, m, r, u)
      assertEquals(protocol.encode, Arr(Bulk("GEORADIUSBYMEMBER"), Bulk(k), Bulk(m), Bulk(r), Bulk(u)))
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius, unit and limit") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, l: PosInt, ms: List[Key]) =>
      val protocol = georadius(k, m, r, u, l)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER"),
          Bulk(k),
          Bulk(m),
          Bulk(r),
          Bulk(u),
          Bulk("COUNT"),
          Bulk(l)
        )
      )
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius, unit and direction") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, d: Direction, ms: List[Key]) =>
      val protocol = georadius(k, m, r, u, d)
      assertEquals(protocol.encode, Arr(Bulk("GEORADIUSBYMEMBER"), Bulk(k), Bulk(m), Bulk(r), Bulk(u), Bulk(d)))
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius, unit, limit and direction") {
    forAll { (input: (Key, Key, NonNegDouble, GeoUnit, PosInt, Direction), ms: List[Key]) =>
      val (k, m, r, u, l, d) = input
      val protocol           = georadius(k, m, r, u, l, d)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER"),
          Bulk(k),
          Bulk(m),
          Bulk(r),
          Bulk(u),
          Bulk("COUNT"),
          Bulk(l),
          Bulk(d)
        )
      )
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius, unit and radius mode") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = georadius(k, m, r, u, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER") ::
            Bulk(k) ::
            Bulk(m) ::
            Bulk(r) ::
            Bulk(u) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius, unit, limit and radius mode") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, l: PosInt, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = georadius(k, m, r, u, l, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER") ::
            Bulk(k) ::
            Bulk(m) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk("COUNT") ::
            Bulk(l) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius, unit, direction and radius mode") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, d: Direction, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = georadius(k, m, r, u, d, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER") ::
            Bulk(k) ::
            Bulk(m) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk(d) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius, unit, limit, direction and radius mode") {
    forAll { (input: (Key, Key, NonNegDouble, GeoUnit, PosInt, Direction), rmAndRes: (GeoRadiusMode, List[?])) =>
      val (k, m, r, u, l, d) = input
      val (rm, res)          = rmAndRes
      implicit val ev        = rm.r
      val protocol           = georadius(k, m, r, u, l, d, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER") ::
            Bulk(k) ::
            Bulk(m) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk("COUNT") ::
            Bulk(l) ::
            Bulk(d) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius, unit and store mode") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, sm: GeoStoreMode, nni: NonNegInt) =>
      val protocol = georadius(k, m, r, u, sm)
      assertEquals(protocol.encode, Arr(Bulk("GEORADIUSBYMEMBER") :: Bulk(k) :: Bulk(m) :: Bulk(r) :: Bulk(u) :: sm.params.map(Bulk(_))))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius, unit, limit and store mode") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, l: PosInt, sm: GeoStoreMode) =>
      forAll { (nni: NonNegInt) =>
        val protocol = georadius(k, m, r, u, l, sm)
        assertEquals(
          protocol.encode,
          Arr(
            Bulk("GEORADIUSBYMEMBER") ::
              Bulk(k) ::
              Bulk(m) ::
              Bulk(r) ::
              Bulk(u) ::
              Bulk("COUNT") ::
              Bulk(l) ::
              sm.params.map(Bulk(_))
          )
        )
        assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
      }
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius, unit, direction and store mode") {
    forAll { (input: (Key, Key, NonNegDouble, GeoUnit, Direction, GeoStoreMode), nni: NonNegInt) =>
      val (k, m, r, u, d, sm) = input
      val protocol            = georadius(k, m, r, u, d, sm)
      assertEquals(
        protocol.encode,
        Arr(Bulk("GEORADIUSBYMEMBER") :: Bulk(k) :: Bulk(m) :: Bulk(r) :: Bulk(u) :: Bulk(d) :: sm.params.map(Bulk(_)))
      )
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Geo protocol using georadius roundtrips successfully given key, member, radius, unit, limit, direction and store mode") {
    forAll { (input: (Key, Key, NonNegDouble, GeoUnit, PosInt, Direction), sm: GeoStoreMode, nni: NonNegInt) =>
      val (k, m, r, u, l, d) = input
      val protocol           = georadius(k, m, r, u, l, d, sm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER") ::
            Bulk(k) ::
            Bulk(m) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk("COUNT") ::
            Bulk(l) ::
            Bulk(d) ::
            sm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, coordinates, radius and unit") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, ms: List[Key]) =>
      val protocol = ro.georadius(k, c, r, u)
      assertEquals(protocol.encode, Arr(Bulk("GEORADIUS_RO"), Bulk(k), Bulk(c.longitude), Bulk(c.latitude), Bulk(r), Bulk(u)))
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, coordinates, radius, unit and limit") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, ms: List[Key]) =>
      val protocol = ro.georadius(k, c, r, u, l)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS_RO"),
          Bulk(k),
          Bulk(c.longitude),
          Bulk(c.latitude),
          Bulk(r),
          Bulk(u),
          Bulk("COUNT"),
          Bulk(l)
        )
      )
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, coordinates, radius, unit and direction") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, ms: List[Key]) =>
      val protocol = ro.georadius(k, c, r, u, d)
      assertEquals(protocol.encode, Arr(Bulk("GEORADIUS_RO"), Bulk(k), Bulk(c.longitude), Bulk(c.latitude), Bulk(r), Bulk(u), Bulk(d)))
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, coordinates, radius, unit, limit and direction") {
    forAll { (input: (Key, GeoCoordinates, NonNegDouble, GeoUnit, PosInt, Direction), ms: List[Key]) =>
      val (k, c, r, u, l, d) = input
      val protocol           = ro.georadius(k, c, r, u, l, d)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS_RO"),
          Bulk(k),
          Bulk(c.longitude),
          Bulk(c.latitude),
          Bulk(r),
          Bulk(u),
          Bulk("COUNT"),
          Bulk(l),
          Bulk(d)
        )
      )
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, coordinates, radius, unit and radius mode") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = ro.georadius(k, c, r, u, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS_RO") ::
            Bulk(k) ::
            Bulk(c.longitude) ::
            Bulk(c.latitude) ::
            Bulk(r) ::
            Bulk(u) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, coordinates, radius, unit, limit and radius mode") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = ro.georadius(k, c, r, u, l, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS_RO") ::
            Bulk(k) ::
            Bulk(c.longitude) ::
            Bulk(c.latitude) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk("COUNT") ::
            Bulk(l) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, coordinates, radius, unit, direction and radius mode") {
    forAll { (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = ro.georadius(k, c, r, u, d, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS_RO") ::
            Bulk(k) ::
            Bulk(c.longitude) ::
            Bulk(c.latitude) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk(d) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property(
    "The Geo protocol using ro.georadius roundtrips successfully given key, coordinates, radius, unit, limit, direction and radius mode"
  ) {
    forAll { (input: (Key, GeoCoordinates, NonNegDouble, GeoUnit, PosInt, Direction), rmAndRes: (GeoRadiusMode, List[?])) =>
      val (k, c, r, u, l, d) = input
      val (rm, res)          = rmAndRes
      implicit val ev        = rm.r
      val protocol           = ro.georadius(k, c, r, u, l, d, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUS_RO") ::
            Bulk(k) ::
            Bulk(c.longitude) ::
            Bulk(c.latitude) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk("COUNT") ::
            Bulk(l) ::
            Bulk(d) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, member, radius and unit") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, ms: List[Key]) =>
      val protocol = ro.georadius(k, m, r, u)
      assertEquals(protocol.encode, Arr(Bulk("GEORADIUSBYMEMBER_RO"), Bulk(k), Bulk(m), Bulk(r), Bulk(u)))
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, member, radius, unit and limit") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, l: PosInt, ms: List[Key]) =>
      val protocol = ro.georadius(k, m, r, u, l)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER_RO"),
          Bulk(k),
          Bulk(m),
          Bulk(r),
          Bulk(u),
          Bulk("COUNT"),
          Bulk(l)
        )
      )
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, member, radius, unit and direction") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, d: Direction, ms: List[Key]) =>
      val protocol = ro.georadius(k, m, r, u, d)
      assertEquals(protocol.encode, Arr(Bulk("GEORADIUSBYMEMBER_RO"), Bulk(k), Bulk(m), Bulk(r), Bulk(u), Bulk(d)))
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, member, radius, unit, limit and direction") {
    forAll { (input: (Key, Key, NonNegDouble, GeoUnit, PosInt, Direction), ms: List[Key]) =>
      val (k, m, r, u, l, d) = input
      val protocol           = ro.georadius(k, m, r, u, l, d)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER_RO"),
          Bulk(k),
          Bulk(m),
          Bulk(r),
          Bulk(u),
          Bulk("COUNT"),
          Bulk(l),
          Bulk(d)
        )
      )
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, member, radius, unit and radius mode") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = ro.georadius(k, m, r, u, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER_RO") ::
            Bulk(k) ::
            Bulk(m) ::
            Bulk(r) ::
            Bulk(u) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, member, radius, unit, limit and radius mode") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, l: PosInt, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = ro.georadius(k, m, r, u, l, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER_RO") ::
            Bulk(k) ::
            Bulk(m) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk("COUNT") ::
            Bulk(l) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using ro.georadius roundtrips successfully given key, member, radius, unit, direction and radius mode") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, d: Direction, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = ro.georadius(k, m, r, u, d, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER_RO") ::
            Bulk(k) ::
            Bulk(m) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk(d) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property(
    "The Geo protocol using ro.georadius roundtrips successfully given key, member, radius, unit, limit, direction and radius mode"
  ) {
    forAll { (input: (Key, Key, NonNegDouble, GeoUnit, PosInt, Direction), rmAndRes: (GeoRadiusMode, List[?])) =>
      val (k, m, r, u, l, d) = input
      val (rm, res)          = rmAndRes
      implicit val ev        = rm.r
      val protocol           = ro.georadius(k, m, r, u, l, d, rm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEORADIUSBYMEMBER_RO") ::
            Bulk(k) ::
            Bulk(m) ::
            Bulk(r) ::
            Bulk(u) ::
            Bulk("COUNT") ::
            Bulk(l) ::
            Bulk(d) ::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using geoadd and geoaddch send NX, XX and CH before the positions") {
    forAll { (k: Key, ps: OneOrMore[GeoPosition], n: NonNegInt) =>
      val positions = ps.value.flatMap(geoPositionToBulkList)
      assertEquals(geoadd(k, GeoFlag.nx, ps).encode, Arr(Bulk("GEOADD") :: Bulk(k) :: Bulk("NX") :: positions))
      assertEquals(geoaddch(k, ps).encode, Arr(Bulk("GEOADD") :: Bulk(k) :: Bulk("CH") :: positions))
      assertEquals(geoaddch(k, GeoFlag.xx, ps).encode, Arr(Bulk("GEOADD") :: Bulk(k) :: Bulk("XX") :: Bulk("CH") :: positions))
      assertEquals(geoaddch(k, GeoFlag.xx, ps).decode(Num(n.value.toLong)), n)
    }
  }

  property("The Geo protocol using geosearch roundtrips successfully given key, a radius around a member, direction and count") {
    forAll { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, d: Direction, c: PosInt, ms: List[Key]) =>
      val protocol = geosearch(k, GeoArea.radius(GeoCenter.member(m), r, u), d, GeoCount.any(c))
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEOSEARCH"),
          Bulk(k),
          Bulk("FROMMEMBER"),
          Bulk(m),
          Bulk("BYRADIUS"),
          Bulk(r),
          Bulk(u),
          Bulk(d),
          Bulk("COUNT"),
          Bulk(c),
          Bulk("ANY")
        )
      )
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }

  property("The Geo protocol using geosearch roundtrips successfully given key, a box around coordinates and radius mode") {
    forAll { (k: Key, co: GeoCoordinates, w: NonNegDouble, h: NonNegDouble, u: GeoUnit, rmAndRes: (GeoRadiusMode, List[?])) =>
      val (rm, res)   = rmAndRes
      implicit val ev = rm.r
      val protocol    = geosearch(k, GeoArea.box(GeoCenter.coordinates(co), w, h, u), rm)
      assertEquals(
        protocol.encode,
        Arr(
          List(
            Bulk("GEOSEARCH"),
            Bulk(k),
            Bulk("FROMLONLAT"),
            Bulk(co.longitude),
            Bulk(co.latitude),
            Bulk("BYBOX"),
            Bulk(w),
            Bulk(h),
            Bulk(u)
          ) :::
            rm.params.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(listToArr(res)), res)
    }
  }

  property("The Geo protocol using geosearchstore roundtrips successfully given source, destination, area, count and STOREDIST") {
    forAll { (s: Key, d: Key, m: Key, r: NonNegDouble, u: GeoUnit, c: PosInt, n: NonNegInt) =>
      val protocol = geosearchstore(s, d, GeoArea.radius(GeoCenter.member(m), r, u), GeoCount.closest(c), GeoStoreDistances)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("GEOSEARCHSTORE"),
          Bulk(d),
          Bulk(s),
          Bulk("FROMMEMBER"),
          Bulk(m),
          Bulk("BYRADIUS"),
          Bulk(r),
          Bulk(u),
          Bulk("COUNT"),
          Bulk(c),
          Bulk("STOREDIST")
        )
      )
      assertEquals(protocol.decode(Num(n.value.toLong)), n)
    }
  }

  property("The Geo protocol using geosearch roundtrips successfully given key and polygon") {
    forAll { (k: Key, a: GeoCoordinates, b: GeoCoordinates, c: GeoCoordinates, more: List[GeoCoordinates], ms: List[Key]) =>
      val protocol = geosearch(k, GeoPolygon(a, b, c, more*))
      val vertices = (a :: b :: c :: more).flatMap(v => List(Bulk(v.longitude), Bulk(v.latitude)))
      assertEquals(protocol.encode, Arr(Bulk("GEOSEARCH") :: Bulk(k) :: Bulk("BYPOLYGON") :: Bulk(3 + more.size) :: vertices))
      assertEquals(protocol.decode(Arr(ms.map(Bulk(_)))), ms)
    }
  }
}
