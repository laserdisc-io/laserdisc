package laserdisc
package protocol

final class GeoPSpec extends GeoExtPSpec {
  import geotypes._
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary

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

  "The Geo protocol" when {

    "using geoadd" should {

      "roundtrip successfully" when {
        "given key and positions" in forAll("key", "positions", "added") { (k: Key, ps: OneOrMore[GeoPosition], nni: NonNegInt) =>
          val protocol = geoadd(k, ps)

          protocol.encode shouldBe Arr(Bulk("GEOADD") :: Bulk(k) :: ps.value.flatMap(geoPosition2BulkList))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
      }

    }

    "using geodist" should {

      "roundtrip successfully" when {
        "given key and members" in {
          forAll("key", "member 1", "member 2", "maybe distance") { (k: Key, m1: Key, m2: Key, onnd: Option[NonNegDouble]) =>
            val protocol = geodist(k, m1, m2)

            protocol.encode shouldBe Arr(Bulk("GEODIST"), Bulk(k), Bulk(m1), Bulk(m2))
            protocol.decode(nonNegDoubleOptionToBulk(onnd)).right.value shouldBe onnd
          }
        }
        "given key, members and unit" in {
          forAll("key", "member 1", "member 2", "unit", "maybe distance") {
            (k: Key, m1: Key, m2: Key, u: GeoUnit, onnd: Option[NonNegDouble]) =>
              val protocol = geodist(k, m1, m2, u)

              protocol.encode shouldBe Arr(Bulk("GEODIST"), Bulk(k), Bulk(m1), Bulk(m2), Bulk(u))
              protocol.decode(nonNegDoubleOptionToBulk(onnd)).right.value shouldBe onnd
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
            protocol.decode(oneOrMoreGeoHashOptionToArr(oghs)).right.value shouldBe oghs.value
          }
        }
      }

    }

    "using geopos" should {

      "roundtrip successfully" when {
        "given key and members" in {
          forAll("key", "members", "coordinatws") { (k: Key, ms: OneOrMoreKeys, ocs: OneOrMore[Option[GeoCoordinates]]) =>
            val protocol = geopos(k, ms)

            protocol.encode shouldBe Arr(Bulk("GEOPOS") :: Bulk(k) :: ms.value.map(m => Bulk(m)))
            protocol.decode(oneOrMoreGeoCoordinatesOptionToArr(ocs)).right.value shouldBe ocs.value
          }
        }
      }

      "using georadius" should {

        "roundtrip successfully" when {
          "given key, coordinates, radius and unit" in {
            forAll("key", "coordinates", "radius", "unit", "members") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, ks: List[Key]) =>
                val protocol = georadius(k, c, r, u)

                protocol.encode shouldBe Arr(Bulk("GEORADIUS"), Bulk(k), Bulk(c.longitude), Bulk(c.latitude), Bulk(r), Bulk(u))
                protocol.decode(Arr(ks.map(Bulk(_)))).right.value shouldBe ks
            }
          }
          "given key, coordinates, radius, unit and limit" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "members") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, ks: List[Key]) =>
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
                protocol.decode(Arr(ks.map(Bulk(_)))).right.value shouldBe ks
            }
          }
          "given key, coordinates, radius, unit and direction" in {
            forAll("key", "coordinates", "radius", "unit", "direction", "members") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, ks: List[Key]) =>
                val protocol = georadius(k, c, r, u, d)

                protocol.encode shouldBe Arr(Bulk("GEORADIUS"), Bulk(k), Bulk(c.longitude), Bulk(c.latitude), Bulk(r), Bulk(u), Bulk(d))
                protocol.decode(Arr(ks.map(Bulk(_)))).right.value shouldBe ks
            }
          }
          "given key, coordinates, radius, unit, limit and direction" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "direction") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, d: Direction) =>
                forAll("members") { ks: List[Key] =>
                  val protocol = georadius(k, c, r, u, l, d)

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
                  protocol.decode(Arr(ks.map(Bulk(_)))).right.value shouldBe ks
                }
            }
          }
          "given key, coordinates, radius, unit and WITHCOORD" in {
            forAll("key", "coordinates", "radius", "unit", "keys and coordinates") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, kcs: List[GeoKeyAndCoord]) =>
                val protocol = georadius(k, c, r, u, GeoRadiusMode.coordinates)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("WITHCOORD")
                )
                protocol
                  .decode(Arr(kcs.map { case GeoKeyAndCoord(k, GeoCoordinates(lat, long)) => Arr(Bulk(k), Arr(Bulk(long), Bulk(lat))) }))
                  .right
                  .value shouldBe kcs
            }
          }
          "given key, coordinates, radius, unit and WITHDIST" in {
            forAll("key", "coordinates", "radius", "unit", "keys and distance") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, kds: List[GeoKeyAndDist]) =>
                val protocol = georadius(k, c, r, u, GeoRadiusMode.distance)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("WITHDIST")
                )
                protocol.decode(Arr(kds.map { case GeoKeyAndDist(k, d) => Arr(Bulk(k), Bulk(d)) })).right.value shouldBe kds
            }
          }
          "given key, coordinates, radius, unit and WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "keys and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, khs: List[GeoKeyAndHash]) =>
                val protocol = georadius(k, c, r, u, GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("WITHHASH")
                )
                protocol.decode(Arr(khs.map { case GeoKeyAndHash(k, h) => Arr(Bulk(k), Num(h.value)) })).right.value shouldBe khs
            }
          }
          "given key, coordinates, radius, unit and WITHCOORD & WITHDIST" in {
            forAll("key", "coordinates", "radius", "unit", "keys, coordinates and distance") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, kcds: List[GeoKeyCoordAndDist]) =>
                val protocol = georadius(k, c, r, u, GeoRadiusMode.coordinates & GeoRadiusMode.distance)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("WITHCOORD"),
                  Bulk("WITHDIST")
                )
                protocol
                  .decode(Arr(kcds.map {
                    case GeoKeyCoordAndDist(k, GeoCoordinates(lat, long), d) => Arr(Bulk(k), Bulk(d), Arr(Bulk(long), Bulk(lat)))
                  }))
                  .right
                  .value shouldBe kcds
            }
          }
          "given key, coordinates, radius, unit and WITHCOORD & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "keys, coordinates and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, kchs: List[GeoKeyCoordAndHash]) =>
                val protocol = georadius(k, c, r, u, GeoRadiusMode.coordinates & GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("WITHCOORD"),
                  Bulk("WITHHASH")
                )
                protocol
                  .decode(Arr(kchs.map {
                    case GeoKeyCoordAndHash(k, GeoCoordinates(lat, long), h) => Arr(Bulk(k), Num(h.value), Arr(Bulk(long), Bulk(lat)))
                  }))
                  .right
                  .value shouldBe kchs
            }
          }
          "given key, coordinates, radius, unit and WITHDIST & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "keys, distance and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, kdhs: List[GeoKeyDistAndHash]) =>
                val protocol = georadius(k, c, r, u, GeoRadiusMode.distance & GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("WITHDIST"),
                  Bulk("WITHHASH")
                )
                protocol
                  .decode(Arr(kdhs.map { case GeoKeyDistAndHash(k, d, h) => Arr(Bulk(k), Bulk(d), Num(h.value)) }))
                  .right
                  .value shouldBe kdhs
            }
          }
          "given key, coordinates, radius, unit and WITHCOORD & WITHDIST & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "keys, coordinates, distance and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, kcdhs: List[GeoKeyCoordDistAndHash]) =>
                val protocol = georadius(k, c, r, u, GeoRadiusMode.coordinates & GeoRadiusMode.distance & GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("WITHCOORD"),
                  Bulk("WITHDIST"),
                  Bulk("WITHHASH")
                )
                protocol
                  .decode(Arr(kcdhs.map {
                    case GeoKeyCoordDistAndHash(k, GeoCoordinates(lat, long), d, h) =>
                      Arr(Bulk(k), Bulk(d), Num(h.value), Arr(Bulk(long), Bulk(lat)))
                  }))
                  .right
                  .value shouldBe kcdhs
            }
          }
          "given key, coordinates, radius, unit, limit and WITHCOORD" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "keys and coordinates") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, kcs: List[GeoKeyAndCoord]) =>
                val protocol = georadius(k, c, r, u, l, GeoRadiusMode.coordinates)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("COUNT"),
                  Bulk(l),
                  Bulk("WITHCOORD")
                )
                protocol
                  .decode(Arr(kcs.map { case GeoKeyAndCoord(k, GeoCoordinates(lat, long)) => Arr(Bulk(k), Arr(Bulk(long), Bulk(lat))) }))
                  .right
                  .value shouldBe kcs
            }
          }
          "given key, coordinates, radius, unit, limit and WITHDIST" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "keys and distance") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, kds: List[GeoKeyAndDist]) =>
                val protocol = georadius(k, c, r, u, l, GeoRadiusMode.distance)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("COUNT"),
                  Bulk(l),
                  Bulk("WITHDIST")
                )
                protocol.decode(Arr(kds.map { case GeoKeyAndDist(k, d) => Arr(Bulk(k), Bulk(d)) })).right.value shouldBe kds
            }
          }
          "given key, coordinates, radius, unit, limit and WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "keys and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, khs: List[GeoKeyAndHash]) =>
                val protocol = georadius(k, c, r, u, l, GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("COUNT"),
                  Bulk(l),
                  Bulk("WITHHASH")
                )
                protocol.decode(Arr(khs.map { case GeoKeyAndHash(k, h) => Arr(Bulk(k), Num(h.value)) })).right.value shouldBe khs
            }
          }
          "given key, coordinates, radius, unit, limit and WITHCOORD & WITHDIST" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "keys, coordinates and distance") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, kcds: List[GeoKeyCoordAndDist]) =>
                val protocol = georadius(k, c, r, u, l, GeoRadiusMode.coordinates & GeoRadiusMode.distance)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("COUNT"),
                  Bulk(l),
                  Bulk("WITHCOORD"),
                  Bulk("WITHDIST")
                )
                protocol
                  .decode(Arr(kcds.map {
                    case GeoKeyCoordAndDist(k, GeoCoordinates(lat, long), d) => Arr(Bulk(k), Bulk(d), Arr(Bulk(long), Bulk(lat)))
                  }))
                  .right
                  .value shouldBe kcds
            }
          }
          "given key, coordinates, radius, unit, limit and WITHCOORD & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "keys, coordinates and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, kchs: List[GeoKeyCoordAndHash]) =>
                val protocol = georadius(k, c, r, u, l, GeoRadiusMode.coordinates & GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("COUNT"),
                  Bulk(l),
                  Bulk("WITHCOORD"),
                  Bulk("WITHHASH")
                )
                protocol
                  .decode(Arr(kchs.map {
                    case GeoKeyCoordAndHash(k, GeoCoordinates(lat, long), h) => Arr(Bulk(k), Num(h.value), Arr(Bulk(long), Bulk(lat)))
                  }))
                  .right
                  .value shouldBe kchs
            }
          }
          "given key, coordinates, radius, unit, limit and WITHDIST & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "keys, distance and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, kdhs: List[GeoKeyDistAndHash]) =>
                val protocol = georadius(k, c, r, u, l, GeoRadiusMode.distance & GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("COUNT"),
                  Bulk(l),
                  Bulk("WITHDIST"),
                  Bulk("WITHHASH")
                )
                protocol
                  .decode(Arr(kdhs.map { case GeoKeyDistAndHash(k, d, h) => Arr(Bulk(k), Bulk(d), Num(h.value)) }))
                  .right
                  .value shouldBe kdhs
            }
          }
          "given key, coordinates, radius, unit, limit and WITHCOORD & WITHDIST & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "keys, coordinates, distance and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, kcdhs: List[GeoKeyCoordDistAndHash]) =>
                val protocol = georadius(k, c, r, u, l, GeoRadiusMode.coordinates & GeoRadiusMode.distance & GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk("COUNT"),
                  Bulk(l),
                  Bulk("WITHCOORD"),
                  Bulk("WITHDIST"),
                  Bulk("WITHHASH")
                )
                protocol
                  .decode(Arr(kcdhs.map {
                    case GeoKeyCoordDistAndHash(k, GeoCoordinates(lat, long), d, h) =>
                      Arr(Bulk(k), Bulk(d), Num(h.value), Arr(Bulk(long), Bulk(lat)))
                  }))
                  .right
                  .value shouldBe kcdhs
            }
          }
          "given key, coordinates, radius, unit, direction and WITHCOORD" in {
            forAll("key", "coordinates", "radius", "unit", "direction", "keys and coordinates") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, kcs: List[GeoKeyAndCoord]) =>
                val protocol = georadius(k, c, r, u, d, GeoRadiusMode.coordinates)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk(d),
                  Bulk("WITHCOORD")
                )
                protocol
                  .decode(Arr(kcs.map { case GeoKeyAndCoord(k, GeoCoordinates(lat, long)) => Arr(Bulk(k), Arr(Bulk(long), Bulk(lat))) }))
                  .right
                  .value shouldBe kcs
            }
          }
          "given key, coordinates, radius, unit, direction and WITHDIST" in {
            forAll("key", "coordinates", "radius", "unit", "direction", "keys and distance") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, kds: List[GeoKeyAndDist]) =>
                val protocol = georadius(k, c, r, u, d, GeoRadiusMode.distance)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk(d),
                  Bulk("WITHDIST")
                )
                protocol.decode(Arr(kds.map { case GeoKeyAndDist(k, d) => Arr(Bulk(k), Bulk(d)) })).right.value shouldBe kds
            }
          }
          "given key, coordinates, radius, unit, direction and WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "direction", "keys and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, khs: List[GeoKeyAndHash]) =>
                val protocol = georadius(k, c, r, u, d, GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk(d),
                  Bulk("WITHHASH")
                )
                protocol.decode(Arr(khs.map { case GeoKeyAndHash(k, h) => Arr(Bulk(k), Num(h.value)) })).right.value shouldBe khs
            }
          }
          "given key, coordinates, radius, unit, direction and WITHCOORD & WITHDIST" in {
            forAll("key", "coordinates", "radius", "unit", "direction", "keys, coordinates and distance") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, kcds: List[GeoKeyCoordAndDist]) =>
                val protocol = georadius(k, c, r, u, d, GeoRadiusMode.coordinates & GeoRadiusMode.distance)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk(d),
                  Bulk("WITHCOORD"),
                  Bulk("WITHDIST")
                )
                protocol
                  .decode(Arr(kcds.map {
                    case GeoKeyCoordAndDist(k, GeoCoordinates(lat, long), d) => Arr(Bulk(k), Bulk(d), Arr(Bulk(long), Bulk(lat)))
                  }))
                  .right
                  .value shouldBe kcds
            }
          }
          "given key, coordinates, radius, unit, direction and WITHCOORD & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "direction", "keys, coordinates and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, kchs: List[GeoKeyCoordAndHash]) =>
                val protocol = georadius(k, c, r, u, d, GeoRadiusMode.coordinates & GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk(d),
                  Bulk("WITHCOORD"),
                  Bulk("WITHHASH")
                )
                protocol
                  .decode(Arr(kchs.map {
                    case GeoKeyCoordAndHash(k, GeoCoordinates(lat, long), h) => Arr(Bulk(k), Num(h.value), Arr(Bulk(long), Bulk(lat)))
                  }))
                  .right
                  .value shouldBe kchs
            }
          }
          "given key, coordinates, radius, unit, direction and WITHDIST & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "direction", "keys, distance and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, kdhs: List[GeoKeyDistAndHash]) =>
                val protocol = georadius(k, c, r, u, d, GeoRadiusMode.distance & GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk(d),
                  Bulk("WITHDIST"),
                  Bulk("WITHHASH")
                )
                protocol
                  .decode(Arr(kdhs.map { case GeoKeyDistAndHash(k, d, h) => Arr(Bulk(k), Bulk(d), Num(h.value)) }))
                  .right
                  .value shouldBe kdhs
            }
          }
          "given key, coordinates, radius, unit, direction and WITHCOORD & WITHDIST & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "direction", "keys, coordinates, distance and hash") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, d: Direction, kcdhs: List[GeoKeyCoordDistAndHash]) =>
                val protocol = georadius(k, c, r, u, d, GeoRadiusMode.coordinates & GeoRadiusMode.distance & GeoRadiusMode.hash)

                protocol.encode shouldBe Arr(
                  Bulk("GEORADIUS"),
                  Bulk(k),
                  Bulk(c.longitude),
                  Bulk(c.latitude),
                  Bulk(r),
                  Bulk(u),
                  Bulk(d),
                  Bulk("WITHCOORD"),
                  Bulk("WITHDIST"),
                  Bulk("WITHHASH")
                )
                protocol
                  .decode(Arr(kcdhs.map {
                    case GeoKeyCoordDistAndHash(k, GeoCoordinates(lat, long), d, h) =>
                      Arr(Bulk(k), Bulk(d), Num(h.value), Arr(Bulk(long), Bulk(lat)))
                  }))
                  .right
                  .value shouldBe kcdhs
            }
          }
          "given key, coordinates, radius, unit, limit, direction and WITHCOORD" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "direction") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, d: Direction) =>
                forAll("keys and coordinates") { kcs: List[GeoKeyAndCoord] =>
                  val protocol = georadius(k, c, r, u, l, d, GeoRadiusMode.coordinates)

                  protocol.encode shouldBe Arr(
                    Bulk("GEORADIUS"),
                    Bulk(k),
                    Bulk(c.longitude),
                    Bulk(c.latitude),
                    Bulk(r),
                    Bulk(u),
                    Bulk("COUNT"),
                    Bulk(l),
                    Bulk(d),
                    Bulk("WITHCOORD")
                  )
                  protocol
                    .decode(Arr(kcs.map { case GeoKeyAndCoord(k, GeoCoordinates(lat, long)) => Arr(Bulk(k), Arr(Bulk(long), Bulk(lat))) }))
                    .right
                    .value shouldBe kcs
                }
            }
          }
          "given key, coordinates, radius, unit, limit, direction and WITHDIST" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "direction") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, d: Direction) =>
                forAll("keys and distance") { kds: List[GeoKeyAndDist] =>
                  val protocol = georadius(k, c, r, u, l, d, GeoRadiusMode.distance)

                  protocol.encode shouldBe Arr(
                    Bulk("GEORADIUS"),
                    Bulk(k),
                    Bulk(c.longitude),
                    Bulk(c.latitude),
                    Bulk(r),
                    Bulk(u),
                    Bulk("COUNT"),
                    Bulk(l),
                    Bulk(d),
                    Bulk("WITHDIST")
                  )
                  protocol.decode(Arr(kds.map { case GeoKeyAndDist(k, d) => Arr(Bulk(k), Bulk(d)) })).right.value shouldBe kds
                }
            }
          }
          "given key, coordinates, radius, unit, limit, direction and WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "direction") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, d: Direction) =>
                forAll("keys and hash") { khs: List[GeoKeyAndHash] =>
                  val protocol = georadius(k, c, r, u, l, d, GeoRadiusMode.hash)

                  protocol.encode shouldBe Arr(
                    Bulk("GEORADIUS"),
                    Bulk(k),
                    Bulk(c.longitude),
                    Bulk(c.latitude),
                    Bulk(r),
                    Bulk(u),
                    Bulk("COUNT"),
                    Bulk(l),
                    Bulk(d),
                    Bulk("WITHHASH")
                  )
                  protocol.decode(Arr(khs.map { case GeoKeyAndHash(k, h) => Arr(Bulk(k), Num(h.value)) })).right.value shouldBe khs
                }
            }
          }
          "given key, coordinates, radius, unit, limit, direction and WITHCOORD & WITHDIST" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "direction") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, d: Direction) =>
                forAll("keys, coordinates and distance") { kcds: List[GeoKeyCoordAndDist] =>
                  val protocol = georadius(k, c, r, u, l, d, GeoRadiusMode.coordinates & GeoRadiusMode.distance)

                  protocol.encode shouldBe Arr(
                    Bulk("GEORADIUS"),
                    Bulk(k),
                    Bulk(c.longitude),
                    Bulk(c.latitude),
                    Bulk(r),
                    Bulk(u),
                    Bulk("COUNT"),
                    Bulk(l),
                    Bulk(d),
                    Bulk("WITHCOORD"),
                    Bulk("WITHDIST")
                  )
                  protocol
                    .decode(Arr(kcds.map {
                      case GeoKeyCoordAndDist(k, GeoCoordinates(lat, long), d) => Arr(Bulk(k), Bulk(d), Arr(Bulk(long), Bulk(lat)))
                    }))
                    .right
                    .value shouldBe kcds
                }
            }
          }
          "given key, coordinates, radius, unit, limit, direction and WITHCOORD & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "direction") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, d: Direction) =>
                forAll("keys, coordinates and hash") { kchs: List[GeoKeyCoordAndHash] =>
                  val protocol = georadius(k, c, r, u, l, d, GeoRadiusMode.coordinates & GeoRadiusMode.hash)

                  protocol.encode shouldBe Arr(
                    Bulk("GEORADIUS"),
                    Bulk(k),
                    Bulk(c.longitude),
                    Bulk(c.latitude),
                    Bulk(r),
                    Bulk(u),
                    Bulk("COUNT"),
                    Bulk(l),
                    Bulk(d),
                    Bulk("WITHCOORD"),
                    Bulk("WITHHASH")
                  )
                  protocol
                    .decode(Arr(kchs.map {
                      case GeoKeyCoordAndHash(k, GeoCoordinates(lat, long), h) => Arr(Bulk(k), Num(h.value), Arr(Bulk(long), Bulk(lat)))
                    }))
                    .right
                    .value shouldBe kchs
                }
            }
          }
          "given key, coordinates, radius, unit, limit, direction and WITHDIST & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "direction") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, d: Direction) =>
                forAll("keys, distance and hash") { kdhs: List[GeoKeyDistAndHash] =>
                  val protocol = georadius(k, c, r, u, l, d, GeoRadiusMode.distance & GeoRadiusMode.hash)

                  protocol.encode shouldBe Arr(
                    Bulk("GEORADIUS"),
                    Bulk(k),
                    Bulk(c.longitude),
                    Bulk(c.latitude),
                    Bulk(r),
                    Bulk(u),
                    Bulk("COUNT"),
                    Bulk(l),
                    Bulk(d),
                    Bulk("WITHDIST"),
                    Bulk("WITHHASH")
                  )
                  protocol
                    .decode(Arr(kdhs.map { case GeoKeyDistAndHash(k, d, h) => Arr(Bulk(k), Bulk(d), Num(h.value)) }))
                    .right
                    .value shouldBe kdhs
                }
            }
          }
          "given key, coordinates, radius, unit, limit, direction and WITHCOORD & WITHDIST & WITHHASH" in {
            forAll("key", "coordinates", "radius", "unit", "limit", "direction") {
              (k: Key, c: GeoCoordinates, r: NonNegDouble, u: GeoUnit, l: PosInt, d: Direction) =>
                forAll("keys, coordinates, distance and hash") { kcdhs: List[GeoKeyCoordDistAndHash] =>
                  val protocol = georadius(k, c, r, u, l, d, GeoRadiusMode.coordinates & GeoRadiusMode.distance & GeoRadiusMode.hash)

                  protocol.encode shouldBe Arr(
                    Bulk("GEORADIUS"),
                    Bulk(k),
                    Bulk(c.longitude),
                    Bulk(c.latitude),
                    Bulk(r),
                    Bulk(u),
                    Bulk("COUNT"),
                    Bulk(l),
                    Bulk(d),
                    Bulk("WITHCOORD"),
                    Bulk("WITHDIST"),
                    Bulk("WITHHASH")
                  )
                  protocol
                    .decode(Arr(kcdhs.map {
                      case GeoKeyCoordDistAndHash(k, GeoCoordinates(lat, long), d, h) =>
                        Arr(Bulk(k), Bulk(d), Num(h.value), Arr(Bulk(long), Bulk(lat)))
                    }))
                    .right
                    .value shouldBe kcdhs
                }
            }
          }
        }
      }
    }
  }
}
