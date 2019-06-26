package laserdisc
package protocol

final class GeoPSpec extends GeoExtPSpec {
  import geos._

  "The Geo protocol" when {

    "using geoadd" should {

      "roundtrip successfully" when {
        "given key and positions" in {
          forAll("key", "positions", "return value") { (k: Key, ps: OneOrMore[Position], nni: NonNegInt) =>
            val protocol = geoadd(k, ps)

            protocol.encode shouldBe Arr(Bulk("GEOADD") :: Bulk(k) :: ps.value.flatMap(position2BulkList))
            protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
          }
        }
      }

    }

    "using geodist" should {

      "roundtrip successfully" when {
        "given key and members" in {
          forAll("key", "member1", "member2", "return value") { (k: Key, m1: Key, m2: Key, onnd: Option[NonNegDouble]) =>
            val protocol = geodist(k, m1, m2)

            protocol.encode shouldBe Arr(Bulk("GEODIST"), Bulk(k), Bulk(m1), Bulk(m2))
            protocol.decode(nonNegDoubleOptionToBulk(onnd)).right.value shouldBe onnd
          }
        }
        "given key, members and unit" in {
          forAll("key", "member1", "member2", "unit", "return value") { (k: Key, m1: Key, m2: Key, u: Unit, onnd: Option[NonNegDouble]) =>
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
          forAll("key", "members", "return value") { (k: Key, ms: OneOrMoreKeys, oghs: OneOrMore[Option[GeoHash]]) =>
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
          forAll("key", "members", "return value") { (k: Key, ms: OneOrMoreKeys, ocs: OneOrMore[Option[Coordinates]]) =>
            val protocol = geopos(k, ms)

            protocol.encode shouldBe Arr(Bulk("GEOPOS") :: Bulk(k) :: ms.value.map(m => Bulk(m)))
            protocol.decode(oneOrMoreCoordinatesOptionToArr(ocs)).right.value shouldBe ocs.value
          }
        }
      }

      "using georadius" should {

        "roundtrip successfully" when {
          "given key and members" in {
            forAll("key", "member1", "member2", "return value") { (k: Key, m1: Key, m2: Key, onnd: Option[NonNegDouble]) =>
              val protocol = geodist(k, m1, m2)

              protocol.encode shouldBe Arr(Bulk("GEODIST"), Bulk(k), Bulk(m1), Bulk(m2))
              protocol.decode(nonNegDoubleOptionToBulk(onnd)).right.value shouldBe onnd
            }
          }
          "given key, members and unit" in {
            forAll("key", "member1", "member2", "unit", "return value") { (k: Key, m1: Key, m2: Key, u: Unit, onnd: Option[NonNegDouble]) =>
              val protocol = geodist(k, m1, m2, u)

              protocol.encode shouldBe Arr(Bulk("GEODIST"), Bulk(k), Bulk(m1), Bulk(m2), Bulk(u))
              protocol.decode(nonNegDoubleOptionToBulk(onnd)).right.value shouldBe onnd
            }
          }
        }
      }
    }
  }
}
