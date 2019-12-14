package laserdisc
package protocol

final class GeoPSpec extends GeoExtPSpec {
  import geotypes._
  import org.scalacheck.{Arbitrary, Gen}
  import org.scalacheck.Arbitrary.arbitrary
  import org.scalacheck.Gen.listOf

  private[this] implicit final val geoKeyAndCoordArb: Arbitrary[GeoKeyAndCoord] = Arbitrary {
    for {
      k <- arbitrary[Key]
      c <- arbitrary[GeoCoordinates]
    } yield GeoKeyAndCoord(k, c)
  }
  private[this] implicit final val geoKeyAndDistArb: Arbitrary[GeoKeyAndDist] = Arbitrary {
    for {
      k <- arbitrary[Key]
      d <- arbitrary[NonNegDouble]
    } yield GeoKeyAndDist(k, d)
  }
  private[this] implicit final val geoKeyAndHashArb: Arbitrary[GeoKeyAndHash] = Arbitrary {
    for {
      k <- arbitrary[Key]
      l <- arbitrary[NonNegLong]
    } yield GeoKeyAndHash(k, l)
  }
  private[this] implicit final val geoKeyCoordAndDistArb: Arbitrary[GeoKeyCoordAndDist] = Arbitrary {
    for {
      k <- arbitrary[Key]
      c <- arbitrary[GeoCoordinates]
      d <- arbitrary[NonNegDouble]
    } yield GeoKeyCoordAndDist(k, c, d)
  }
  private[this] implicit final val geoKeyCoordAndHashArb: Arbitrary[GeoKeyCoordAndHash] = Arbitrary {
    for {
      k <- arbitrary[Key]
      c <- arbitrary[GeoCoordinates]
      l <- arbitrary[NonNegLong]
    } yield GeoKeyCoordAndHash(k, c, l)
  }
  private[this] implicit final val geoKeyDistAndHashArb: Arbitrary[GeoKeyDistAndHash] = Arbitrary {
    for {
      k <- arbitrary[Key]
      d <- arbitrary[NonNegDouble]
      l <- arbitrary[NonNegLong]
    } yield GeoKeyDistAndHash(k, d, l)
  }
  private[this] implicit final val geoKeyCoordDistAndHashArb: Arbitrary[GeoKeyCoordDistAndHash] = Arbitrary {
    for {
      k <- arbitrary[Key]
      c <- arbitrary[GeoCoordinates]
      d <- arbitrary[NonNegDouble]
      l <- arbitrary[NonNegLong]
    } yield GeoKeyCoordDistAndHash(k, c, d, l)
  }
  private[this] implicit final val geoRadiusModeArb: Arbitrary[(GeoRadiusMode, List[_])] = Arbitrary {
    Gen.oneOf(
      listOf(geoKeyAndCoordArb.arbitrary).map(GeoRadiusMode.coordinates                -> _),
      listOf(geoKeyAndDistArb.arbitrary).map(GeoRadiusMode.distance                    -> _),
      listOf(geoKeyAndHashArb.arbitrary).map(GeoRadiusMode.hash                        -> _),
      listOf(geoKeyCoordAndDistArb.arbitrary).map(GeoRadiusMode.coordinatesAndDistance -> _),
      listOf(geoKeyCoordAndHashArb.arbitrary).map(GeoRadiusMode.coordinatesAndHash     -> _),
      listOf(geoKeyDistAndHashArb.arbitrary).map(GeoRadiusMode.distanceAndHash         -> _),
      listOf(geoKeyCoordDistAndHashArb.arbitrary).map(GeoRadiusMode.all                -> _)
    )
  }
  private[this] implicit final val geoStoreModeArb: Arbitrary[GeoStoreMode] = Arbitrary {
    Gen.oneOf(
      arbitrary[Key].map(GeoStoreHash(_)),
      arbitrary[Key].map(GeoStoreDistance(_)),
      arbitrary[Key].flatMap(hk => arbitrary[Key].map(GeoStoreBoth(hk, _)))
    )
  }

  private[this] final val listToArr: List[_] => Arr = l =>
    Arr(l.collect {
      case GeoKeyAndCoord(k, GeoCoordinates(lat, long))               => Arr(Bulk(k), Arr(Bulk(long), Bulk(lat)))
      case GeoKeyAndDist(k, d)                                        => Arr(Bulk(k), Bulk(d))
      case GeoKeyAndHash(k, h)                                        => Arr(Bulk(k), Num(h.value))
      case GeoKeyCoordAndDist(k, GeoCoordinates(lat, long), d)        => Arr(Bulk(k), Bulk(d), Arr(Bulk(long), Bulk(lat)))
      case GeoKeyCoordAndHash(k, GeoCoordinates(lat, long), h)        => Arr(Bulk(k), Num(h.value), Arr(Bulk(long), Bulk(lat)))
      case GeoKeyDistAndHash(k, d, h)                                 => Arr(Bulk(k), Bulk(d), Num(h.value))
      case GeoKeyCoordDistAndHash(k, GeoCoordinates(lat, long), d, h) => Arr(Bulk(k), Bulk(d), Num(h.value), Arr(Bulk(long), Bulk(lat)))
    })

  "The Geo protocol" when {
    "using geoadd" should {
      "roundtrip successfully" when {
        "given key and positions" in forAll("key", "positions", "added") { (k: Key, ps: OneOrMore[GeoPosition], nni: NonNegInt) =>
          val protocol = geoadd(k, ps)

          protocol.encode shouldBe Arr(Bulk("GEOADD") :: Bulk(k) :: ps.value.flatMap(geoPositionToBulkList))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using geodist" should {
      "roundtrip successfully" when {
        "given key and members" in {
          forAll("key", "member 1", "member 2", "maybe distance") { (k: Key, m1: Key, m2: Key, onnd: Option[NonNegDouble]) =>
            val protocol = geodist(k, m1, m2)

            protocol.encode shouldBe Arr(Bulk("GEODIST"), Bulk(k), Bulk(m1), Bulk(m2))
            protocol.decode(nonNegDoubleOptionToBulk(onnd)) onRight (_ shouldBe onnd)
          }
        }
        "given key, members and unit" in {
          forAll("key", "member 1", "member 2", "unit", "maybe distance") {
            (k: Key, m1: Key, m2: Key, u: GeoUnit, onnd: Option[NonNegDouble]) =>
              val protocol = geodist(k, m1, m2, u)

              protocol.encode shouldBe Arr(Bulk("GEODIST"), Bulk(k), Bulk(m1), Bulk(m2), Bulk(u))
              protocol.decode(nonNegDoubleOptionToBulk(onnd)) onRight (_ shouldBe onnd)
          }
        }
      }
    }

    "using geohash" should {
      "roundtrip successfully" when {
        "given key and members" in {
          forAll("key", "members", "geo hashes") { (k: Key, ms: OneOrMoreKeys, oghs: OneOrMore[Option[GeoHash]]) =>
            val protocol = geohash(k, ms)

            protocol.encode shouldBe Arr(Bulk("GEOHASH") :: Bulk(k) :: ms.value.map(m => Bulk(m)))
            protocol.decode(oneOrMoreGeoHashOptionToArr(oghs)) onRight (_ shouldBe oghs.value)
          }
        }
      }
    }

    "using geopos" should {
      "roundtrip successfully" when {
        "given key and members" in {
          forAll("key", "members", "coordinates") { (k: Key, ms: OneOrMoreKeys, ocs: OneOrMore[Option[GeoCoordinates]]) =>
            val protocol = geopos(k, ms)

            protocol.encode shouldBe Arr(Bulk("GEOPOS") :: Bulk(k) :: ms.value.map(m => Bulk(m)))
            protocol.decode(oneOrMoreGeoCoordinatesOptionToArr(ocs)) onRight (_ shouldBe ocs.value)
          }
        }
      }
    }

    "using georadius" should {
      "roundtrip successfully" when {
        "given key, coordinates, radius and unit" in {
          forAll("key", "coordinates", "radius", "unit", "members") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, ms: List[Key]) =>
              val protocol = georadius(k, c, r, u)

              protocol.encode shouldBe Arr(Bulk("GEORADIUS"), Bulk(k), Bulk(c.longitude), Bulk(c.latitude), Bulk(r), Bulk(u))
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, coordinates, radius, unit and limit" in {
          forAll("key", "coordinates", "radius", "unit", "limit", "members") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, ms: List[Key]) =>
              val protocol = georadius(k, c, r, u, l)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUS"),
                Bulk(k),
                Bulk(c.longitude),
                Bulk(c.latitude),
                Bulk(r),
                Bulk(u),
                Bulk("COUNT"),
                Bulk(l)
              )
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, coordinates, radius, unit and direction" in {
          forAll("key", "coordinates", "radius", "unit", "direction", "members") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, ms: List[Key]) =>
              val protocol = georadius(k, c, r, u, d)

              protocol.encode shouldBe Arr(Bulk("GEORADIUS"), Bulk(k), Bulk(c.longitude), Bulk(c.latitude), Bulk(r), Bulk(u), Bulk(d))
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, coordinates, radius, unit, limit and direction" in {
          forAll("key, coordinates, radius, unit, limit, direction", "members") {
            (input: (Key, GeoCoordinates, NonNegDouble, GeoUnit, PosInt, Direction), ms: List[Key]) =>
              val (k, c, r, u, l, d) = input
              val protocol           = georadius(k, c, r, u, l, d)

              protocol.encode shouldBe Arr(
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
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, coordinates, radius, unit and radius mode" in {
          forAll("key", "coordinates", "radius", "unit", "radius mode & result") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = georadius(k, c, r, u, rm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUS") ::
                  Bulk(k) ::
                  Bulk(c.longitude) ::
                  Bulk(c.latitude) ::
                  Bulk(r) ::
                  Bulk(u) ::
                  rm.params.map(Bulk(_))
              )
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, coordinates, radius, unit, limit and radius mode" in {
          forAll("key", "coordinates", "radius", "unit", "limit", "radius mode & result") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = georadius(k, c, r, u, l, rm)

              protocol.encode shouldBe Arr(
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
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, coordinates, radius, unit, direction and radius mode" in {
          forAll("key", "coordinates", "radius", "unit", "direction", "radius mode & result") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = georadius(k, c, r, u, d, rm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUS") ::
                  Bulk(k) ::
                  Bulk(c.longitude) ::
                  Bulk(c.latitude) ::
                  Bulk(r) ::
                  Bulk(u) ::
                  Bulk(d) ::
                  rm.params.map(Bulk(_))
              )
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, coordinates, radius, unit, limit, direction and radius mode" in {
          forAll("key, coordinates, radius, unit, limit, direction", "radius mode & result") {
            (input: (Key, GeoCoordinates, NonNegDouble, GeoUnit, PosInt, Direction), rmAndRes: (GeoRadiusMode, List[_])) =>
              val (k, c, r, u, l, d) = input
              val (rm, res)          = rmAndRes
              implicit val ev        = rm.r
              val protocol           = georadius(k, c, r, u, l, d, rm)

              protocol.encode shouldBe Arr(
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
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, coordinates, radius, unit and store mode" in {
          forAll("key", "coordinates", "radius", "unit", "store mode", "stored") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, sm: GeoStoreMode, nni: NonNegInt) =>
              val protocol = georadius(k, c, r, u, sm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUS") :: Bulk(k) :: Bulk(c.longitude) :: Bulk(c.latitude) :: Bulk(r) :: Bulk(u) :: sm.params.map(Bulk(_))
              )
              protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
          }
        }
        "given key, coordinates, radius, unit, limit and store mode" in {
          forAll("key", "coordinates", "radius", "unit", "limit", "store mode") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, sm: GeoStoreMode) =>
              forAll("stored") { nni: NonNegInt =>
                val protocol = georadius(k, c, r, u, l, sm)

                protocol.encode shouldBe Arr(
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
                protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
              }
          }
        }
        "given key, coordinates, radius, unit, direction and store mode" in {
          forAll("key", "coordinates", "radius", "unit", "direction", "store mode") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, sm: GeoStoreMode) =>
              forAll("stored") { nni: NonNegInt =>
                val protocol = georadius(k, c, r, u, d, sm)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS") ::
                    Bulk(k) ::
                    Bulk(c.longitude) ::
                    Bulk(c.latitude) ::
                    Bulk(r) ::
                    Bulk(u) ::
                    Bulk(d) ::
                    sm.params.map(Bulk(_))
                )
                protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
              }
          }
        }
        "given key, coordinates, radius, unit, limit, direction and store mode" in {
          forAll("key, coordinates, radius, unit, limit, direction", "store mode", "stored") {
            (input: (Key, GeoCoordinates, NonNegDouble, GeoUnit, PosInt, Direction), sm: GeoStoreMode, nni: NonNegInt) =>
              val (k, c, r, u, l, d) = input
              val protocol           = georadius(k, c, r, u, l, d, sm)

              protocol.encode shouldBe Arr(
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
              protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
          }
        }

        "given key, member, radius and unit" in {
          forAll("key", "member", "radius", "unit", "members") { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, ms: List[Key]) =>
            val protocol = georadius(k, m, r, u)

            protocol.encode shouldBe Arr(Bulk("GEORADIUSBYMEMBER"), Bulk(k), Bulk(m), Bulk(r), Bulk(u))
            protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, member, radius, unit and limit" in {
          forAll("key", "coordinates", "radius", "unit", "limit", "members") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, l: PosInt, ms: List[Key]) =>
              val protocol = georadius(k, m, r, u, l)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER"),
                Bulk(k),
                Bulk(m),
                Bulk(r),
                Bulk(u),
                Bulk("COUNT"),
                Bulk(l)
              )
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, member, radius, unit and direction" in {
          forAll("key", "member", "radius", "unit", "direction", "members") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, d: Direction, ms: List[Key]) =>
              val protocol = georadius(k, m, r, u, d)

              protocol.encode shouldBe Arr(Bulk("GEORADIUSBYMEMBER"), Bulk(k), Bulk(m), Bulk(r), Bulk(u), Bulk(d))
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, member, radius, unit, limit and direction" in {
          forAll("key, member, radius, unit, limit, direction", "members") {
            (input: (Key, Key, NonNegDouble, GeoUnit, PosInt, Direction), ms: List[Key]) =>
              val (k, m, r, u, l, d) = input
              val protocol           = georadius(k, m, r, u, l, d)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER"),
                Bulk(k),
                Bulk(m),
                Bulk(r),
                Bulk(u),
                Bulk("COUNT"),
                Bulk(l),
                Bulk(d)
              )
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, member, radius, unit and radius mode" in {
          forAll("key", "member", "radius", "unit", "radius mode & result") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = georadius(k, m, r, u, rm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER") ::
                  Bulk(k) ::
                  Bulk(m) ::
                  Bulk(r) ::
                  Bulk(u) ::
                  rm.params.map(Bulk(_))
              )
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, member, radius, unit, limit and radius mode" in {
          forAll("key", "member", "radius", "unit", "limit", "radius mode & result") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, l: PosInt, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = georadius(k, m, r, u, l, rm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER") ::
                  Bulk(k) ::
                  Bulk(m) ::
                  Bulk(r) ::
                  Bulk(u) ::
                  Bulk("COUNT") ::
                  Bulk(l) ::
                  rm.params.map(Bulk(_))
              )
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, member, radius, unit, direction and radius mode" in {
          forAll("key", "member", "radius", "unit", "direction", "radius mode & result") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, d: Direction, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = georadius(k, m, r, u, d, rm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER") ::
                  Bulk(k) ::
                  Bulk(m) ::
                  Bulk(r) ::
                  Bulk(u) ::
                  Bulk(d) ::
                  rm.params.map(Bulk(_))
              )
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, member, radius, unit, limit, direction and radius mode" in {
          forAll("key, member, radius, unit, limit, direction", "radius mode & result") {
            (input: (Key, Key, NonNegDouble, GeoUnit, PosInt, Direction), rmAndRes: (GeoRadiusMode, List[_])) =>
              val (k, m, r, u, l, d) = input
              val (rm, res)          = rmAndRes
              implicit val ev        = rm.r
              val protocol           = georadius(k, m, r, u, l, d, rm)

              protocol.encode shouldBe Arr(
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
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, member, radius, unit and store mode" in {
          forAll("key", "member", "radius", "unit", "store mode", "stored") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, sm: GeoStoreMode, nni: NonNegInt) =>
              val protocol = georadius(k, m, r, u, sm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER") :: Bulk(k) :: Bulk(m) :: Bulk(r) :: Bulk(u) :: sm.params.map(Bulk(_))
              )
              protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
          }
        }
        "given key, member, radius, unit, limit and store mode" in {
          forAll("key", "member", "radius", "unit", "limit", "store mode") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, l: PosInt, sm: GeoStoreMode) =>
              forAll("stored") { nni: NonNegInt =>
                val protocol = georadius(k, m, r, u, l, sm)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUSBYMEMBER") ::
                    Bulk(k) ::
                    Bulk(m) ::
                    Bulk(r) ::
                    Bulk(u) ::
                    Bulk("COUNT") ::
                    Bulk(l) ::
                    sm.params.map(Bulk(_))
                )
                protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
              }
          }
        }
        "given key, member, radius, unit, direction and store mode" in {
          forAll("key, member, radius, unit, direction, store mode", "stored") {
            (input: (Key, Key, NonNegDouble, GeoUnit, Direction, GeoStoreMode), nni: NonNegInt) =>
              val (k, m, r, u, d, sm) = input
              val protocol            = georadius(k, m, r, u, d, sm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER") :: Bulk(k) :: Bulk(m) :: Bulk(r) :: Bulk(u) :: Bulk(d) :: sm.params.map(Bulk(_))
              )
              protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
          }
        }
        "given key, member, radius, unit, limit, direction and store mode" in {
          forAll("key, member, radius, unit, limit, direction", "store mode", "stored") {
            (input: (Key, Key, NonNegDouble, GeoUnit, PosInt, Direction), sm: GeoStoreMode, nni: NonNegInt) =>
              val (k, m, r, u, l, d) = input
              val protocol           = georadius(k, m, r, u, l, d, sm)

              protocol.encode shouldBe Arr(
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
              protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
          }
        }
      }
    }

    "using ro.georadius" should {
      "roundtrip successfully" when {
        "given key, coordinates, radius and unit" in {
          forAll("key", "coordinates", "radius", "unit", "members") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, ms: List[Key]) =>
              val protocol = ro.georadius(k, c, r, u)

              protocol.encode shouldBe Arr(Bulk("GEORADIUS_RO"), Bulk(k), Bulk(c.longitude), Bulk(c.latitude), Bulk(r), Bulk(u))
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, coordinates, radius, unit and limit" in {
          forAll("key", "coordinates", "radius", "unit", "limit", "members") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, ms: List[Key]) =>
              val protocol = ro.georadius(k, c, r, u, l)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUS_RO"),
                Bulk(k),
                Bulk(c.longitude),
                Bulk(c.latitude),
                Bulk(r),
                Bulk(u),
                Bulk("COUNT"),
                Bulk(l)
              )
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, coordinates, radius, unit and direction" in {
          forAll("key", "coordinates", "radius", "unit", "direction", "members") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, ms: List[Key]) =>
              val protocol = ro.georadius(k, c, r, u, d)

              protocol.encode shouldBe Arr(Bulk("GEORADIUS_RO"), Bulk(k), Bulk(c.longitude), Bulk(c.latitude), Bulk(r), Bulk(u), Bulk(d))
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, coordinates, radius, unit, limit and direction" in {
          forAll("key, coordinates, radius, unit, limit, direction", "members") {
            (input: (Key, GeoCoordinates, NonNegDouble, GeoUnit, PosInt, Direction), ms: List[Key]) =>
              val (k, c, r, u, l, d) = input
              val protocol           = ro.georadius(k, c, r, u, l, d)

              protocol.encode shouldBe Arr(
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
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, coordinates, radius, unit and radius mode" in {
          forAll("key", "coordinates", "radius", "unit", "radius mode & result") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = ro.georadius(k, c, r, u, rm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUS_RO") ::
                  Bulk(k) ::
                  Bulk(c.longitude) ::
                  Bulk(c.latitude) ::
                  Bulk(r) ::
                  Bulk(u) ::
                  rm.params.map(Bulk(_))
              )
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, coordinates, radius, unit, limit and radius mode" in {
          forAll("key", "coordinates", "radius", "unit", "limit", "radius mode & result") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = ro.georadius(k, c, r, u, l, rm)

              protocol.encode shouldBe Arr(
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
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, coordinates, radius, unit, direction and radius mode" in {
          forAll("key", "coordinates", "radius", "unit", "direction", "radius mode & result") {
            (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = ro.georadius(k, c, r, u, d, rm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUS_RO") ::
                  Bulk(k) ::
                  Bulk(c.longitude) ::
                  Bulk(c.latitude) ::
                  Bulk(r) ::
                  Bulk(u) ::
                  Bulk(d) ::
                  rm.params.map(Bulk(_))
              )
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, coordinates, radius, unit, limit, direction and radius mode" in {
          forAll("key, coordinates, radius, unit, limit, direction", "radius mode & result") {
            (input: (Key, GeoCoordinates, NonNegDouble, GeoUnit, PosInt, Direction), rmAndRes: (GeoRadiusMode, List[_])) =>
              val (k, c, r, u, l, d) = input
              val (rm, res)          = rmAndRes
              implicit val ev        = rm.r
              val protocol           = ro.georadius(k, c, r, u, l, d, rm)

              protocol.encode shouldBe Arr(
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
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }

        "given key, member, radius and unit" in {
          forAll("key", "member", "radius", "unit", "members") { (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, ms: List[Key]) =>
            val protocol = ro.georadius(k, m, r, u)

            protocol.encode shouldBe Arr(Bulk("GEORADIUSBYMEMBER_RO"), Bulk(k), Bulk(m), Bulk(r), Bulk(u))
            protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, member, radius, unit and limit" in {
          forAll("key", "coordinates", "radius", "unit", "limit", "members") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, l: PosInt, ms: List[Key]) =>
              val protocol = ro.georadius(k, m, r, u, l)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER_RO"),
                Bulk(k),
                Bulk(m),
                Bulk(r),
                Bulk(u),
                Bulk("COUNT"),
                Bulk(l)
              )
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, member, radius, unit and direction" in {
          forAll("key", "member", "radius", "unit", "direction", "members") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, d: Direction, ms: List[Key]) =>
              val protocol = ro.georadius(k, m, r, u, d)

              protocol.encode shouldBe Arr(Bulk("GEORADIUSBYMEMBER_RO"), Bulk(k), Bulk(m), Bulk(r), Bulk(u), Bulk(d))
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, member, radius, unit, limit and direction" in {
          forAll("key, member, radius, unit, limit, direction", "members") {
            (input: (Key, Key, NonNegDouble, GeoUnit, PosInt, Direction), ms: List[Key]) =>
              val (k, m, r, u, l, d) = input
              val protocol           = ro.georadius(k, m, r, u, l, d)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER_RO"),
                Bulk(k),
                Bulk(m),
                Bulk(r),
                Bulk(u),
                Bulk("COUNT"),
                Bulk(l),
                Bulk(d)
              )
              protocol.decode(Arr(ms.map(Bulk(_)))) onRight (_ shouldBe ms)
          }
        }
        "given key, member, radius, unit and radius mode" in {
          forAll("key", "member", "radius", "unit", "radius mode & result") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = ro.georadius(k, m, r, u, rm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER_RO") ::
                  Bulk(k) ::
                  Bulk(m) ::
                  Bulk(r) ::
                  Bulk(u) ::
                  rm.params.map(Bulk(_))
              )
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, member, radius, unit, limit and radius mode" in {
          forAll("key", "member", "radius", "unit", "limit", "radius mode & result") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, l: PosInt, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = ro.georadius(k, m, r, u, l, rm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER_RO") ::
                  Bulk(k) ::
                  Bulk(m) ::
                  Bulk(r) ::
                  Bulk(u) ::
                  Bulk("COUNT") ::
                  Bulk(l) ::
                  rm.params.map(Bulk(_))
              )
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, member, radius, unit, direction and radius mode" in {
          forAll("key", "member", "radius", "unit", "direction", "radius mode & result") {
            (k: Key, m: Key, r: NonNegDouble, u: GeoUnit, d: Direction, rmAndRes: (GeoRadiusMode, List[_])) =>
              val (rm, res)   = rmAndRes
              implicit val ev = rm.r
              val protocol    = ro.georadius(k, m, r, u, d, rm)

              protocol.encode shouldBe Arr(
                Bulk("GEORADIUSBYMEMBER_RO") ::
                  Bulk(k) ::
                  Bulk(m) ::
                  Bulk(r) ::
                  Bulk(u) ::
                  Bulk(d) ::
                  rm.params.map(Bulk(_))
              )
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
        "given key, member, radius, unit, limit, direction and radius mode" in {
          forAll("key, member, radius, unit, limit, direction", "radius mode & result") {
            (input: (Key, Key, NonNegDouble, GeoUnit, PosInt, Direction), rmAndRes: (GeoRadiusMode, List[_])) =>
              val (k, m, r, u, l, d) = input
              val (rm, res)          = rmAndRes
              implicit val ev        = rm.r
              val protocol           = ro.georadius(k, m, r, u, l, d, rm)

              protocol.encode shouldBe Arr(
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
              protocol.decode(listToArr(res)) onRight (_ shouldBe res)
          }
        }
      }
    }
  }
}
