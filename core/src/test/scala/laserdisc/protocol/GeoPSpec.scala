package laserdisc
package protocol

final class GeoPSpec extends GeoExtraPSpec {
  import geo._
  import geos._
  import RESP._
  import show._

  "A GeoBaseP" when {

    "using geoadd" should {

      "fail to compile" when {
        "given empty key" in {
          """geoadd(Key(""), OneOrMore.unsafeFrom(List(Position("a", 0d, 0d))))""" shouldNot compile
        }
        "given non literal to refine" in {
          """geoadd(Key("a"), OneOrMore(List(Position("a", 0d, 0d))))""" shouldNot compile
        }
      }

      "fail at runtime" when {
        "given empty list of positions to safely refine" in {
          OneOrMore.from(List.empty[Position]).right.map(geoadd(Key("a"), _)).left.value shouldBe "Predicate isEmpty(List()) did not fail."
        }
        "given empty list of positions to unsafely refine" in {
          the[IllegalArgumentException].thrownBy {
            geoadd(Key("a"), OneOrMore.unsafeFrom(List.empty[Position]))
          }.getMessage shouldBe "Predicate isEmpty(List()) did not fail."
        }
      }

      "compile successfully" when {
        "given non empty key and non empty list of positions" in {
          forAll("key", "positions", "return value") { (k: Key, ps: OneOrMore[Position], nni: NonNegInt) =>
            val protocol = geoadd(k, ps)

            protocol.encode shouldBe arr(bulk("GEOADD") :: bulk(k.show) :: ps.value.flatMap(position2BulkList))
            protocol.decode(num(nni.value.toLong)).right.value shouldBe nni
          }
        }
      }

    }

    "using geodist" should {

      "fail to compile" when {
        "given empty key" in {
          """geodist(Key(""), Key("m1"), Key("m2"))""" shouldNot compile
        }
        "given empty member1" in {
          """geodist(Key("a"), "", Key("m2"))""" shouldNot compile
        }
        "given empty member2" in {
          """geodist(Key("a"), Key("m1"), "")""" shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and members" in {
          forAll("key", "member1", "member2", "return value") { (k: Key, m1: Key, m2: Key, onnd: Option[NonNegDouble]) =>
            val protocol = geodist(k, m1, m2)

            protocol.encode shouldBe arr(bulk("GEODIST"), bulk(k.show), bulk(m1.show), bulk(m2.show))
            protocol.decode(nonNegDoubleOptionToBulk(onnd)).right.value shouldBe onnd
          }
        }
        "given non empty key, members and unit" in {
          forAll("key", "member1", "member2", "unit", "return value") { (k: Key, m1: Key, m2: Key, u: Unit, onnd: Option[NonNegDouble]) =>
            val protocol = geodist(k, m1, m2, u)

            protocol.encode shouldBe arr(bulk("GEODIST"), bulk(k.show), bulk(m1.show), bulk(m2.show), bulk(u.show))
            protocol.decode(nonNegDoubleOptionToBulk(onnd)).right.value shouldBe onnd
          }
        }
      }
    }

    "using geohash" should {

      "fail to compile" when {
        "given empty key" in {
          """geohash("", OneOrMoreKeys.unsafeFrom(List(Key("m"))))""" shouldNot compile
        }
        "given non literal to refine" in {
          """geohash("a", OneOrMoreKeys(List(Key("m"))))""" shouldNot compile
        }
      }

      "fail at runtime" when {
        "given empty list of members to safely refine" in {
          OneOrMoreKeys.from(List.empty).right.map(geohash(Key("a"), _)).left.value shouldBe "Predicate isEmpty(List()) did not fail."
        }
        "given empty list of members to unsafely refine" in {
          the[IllegalArgumentException].thrownBy {
            geohash(Key("a"), OneOrMoreKeys.unsafeFrom(List.empty))
          }.getMessage shouldBe "Predicate isEmpty(List()) did not fail."
        }
      }

      "compile successfully" when {
        "given non empty key and non empty list of members" in {
          forAll("key", "members", "return value") { (k: Key, ms: OneOrMoreKeys, oghs: OneOrMore[Option[GeoHash]]) =>
            val protocol = geohash(k, ms)

            protocol.encode shouldBe arr(bulk("GEOHASH") :: bulk(k.show) :: ms.value.map(m => bulk(m.show)))
            protocol.decode(oneOrMoreGeoHashOptionToArr(oghs)).right.value shouldBe oghs.value
          }
        }
      }

    }

    "using geopos" should {

      "fail to compile" when {
        "given empty key" in {
          """geopos("", OneOrMoreKeys.unsafeFrom(List(Key("m"))))""" shouldNot compile
        }
        "given non literal to refine" in {
          """geopos("a", OneOrMoreKeys(List(Key("m"))))""" shouldNot compile
        }
      }

      "fail at runtime" when {
        "given empty list of members to safely refine" in {
          OneOrMoreKeys.from(List.empty).right.map(geopos(Key("a"), _)).left.value shouldBe "Predicate isEmpty(List()) did not fail."
        }
        "given empty list of members to unsafely refine" in {
          the[IllegalArgumentException].thrownBy {
            geopos(Key("a"), OneOrMoreKeys.unsafeFrom(List.empty))
          }.getMessage shouldBe "Predicate isEmpty(List()) did not fail."
        }
      }

      "compile successfully" when {
        "given non empty key and non empty list of members" in {
          forAll("key", "members", "return value") { (k: Key, ms: OneOrMoreKeys, ocs: OneOrMore[Option[Coordinates]]) =>
            val protocol = geopos(k, ms)

            protocol.encode shouldBe arr(bulk("GEOPOS") :: bulk(k.show) :: ms.value.map(m => bulk(m.show)))
            protocol.decode(oneOrMoreCoordinatesOptionToArr(ocs)).right.value shouldBe ocs.value
          }
        }
      }

      "using georadius" should {

        "fail to compile" when {
          "given empty key" in {
            """georadius("", Coordinates(0.0d, 0.0d), ValidDouble(0.0d), unit.meters)""" shouldNot compile
          }
          "given invalid radius" in {
            """georadius(Key("a"), Coordinates(0.0d, 0.0d), Key("m2"))""" shouldNot compile
          }
          "given empty member2" in {
            """georadius(Key("a"), Key("m1"), "")""" shouldNot compile
          }
        }

        "compile successfully" when {
          "given non empty key and members" in {
            forAll("key", "member1", "member2", "return value") { (k: Key, m1: Key, m2: Key, onnd: Option[NonNegDouble]) =>
              val protocol = geodist(k, m1, m2)

              protocol.encode shouldBe arr(bulk("GEODIST"), bulk(k.show), bulk(m1.show), bulk(m2.show))
              protocol.decode(nonNegDoubleOptionToBulk(onnd)).right.value shouldBe onnd
            }
          }
          "given non empty key, members and unit" in {
            forAll("key", "member1", "member2", "unit", "return value") { (k: Key, m1: Key, m2: Key, u: Unit, onnd: Option[NonNegDouble]) =>
              val protocol = geodist(k, m1, m2, u)

              protocol.encode shouldBe arr(bulk("GEODIST"), bulk(k.show), bulk(m1.show), bulk(m2.show), bulk(u.show))
              protocol.decode(nonNegDoubleOptionToBulk(onnd)).right.value shouldBe onnd
            }
          }
        }
      }
    }
  }
}
