package laserdisc
package protocol

final class GeoPSpec extends GeoExtraPSpec {
  import auto._
  import geo._
  import geos._
  import RESP._
  import show._

  "A GeoBaseP" when {

    "using geoadd" should {

      "fail to compile" when {
        "given empty key" in {
          """geoadd("", OneOrMore.unsafeFrom(List(Position("a", 0d, 0d))))""" shouldNot compile
        }
        "given non literal to refine" in {
          """geoadd("a", OneOrMore(List(Position("a", 0d, 0d))))""" shouldNot compile
        }
      }

      "fail at runtime" when {
        "given empty list of positions to safely refine" in {
          OneOrMore.from(List.empty[Position]).right.map(geoadd("a", _)).left.value shouldBe "Predicate isEmpty(List()) did not fail."
        }
        "given empty list of positions to unsafely refine" in {
          the[IllegalArgumentException].thrownBy {
            geoadd("a", OneOrMore.unsafeFrom(List.empty[Position]))
          }.getMessage shouldBe "Predicate isEmpty(List()) did not fail."
        }
      }

      "compile successfully" when {
        "given non empty key and non empty list of positions" in {
          forAll("key", "positions", "return value") { (k: Key, ps: OneOrMore[Position], nni: NonNegInt) =>
            val protocol = geoadd(k, ps)

            protocol.encode shouldBe arr(bulk("GEOADD") :: bulk(k.show) :: ps.flatMap(position2BulkList))
            protocol.decode(num(nni.toLong)).right.value shouldBe nni
          }
        }
      }

    }

    "using geodist" should {

      "fail to compile" when {
        "given empty key" in {
          """geodist("", Key("m1"), Key("m2"))""" shouldNot compile
        }
        "given empty member1" in {
          """geodist(Key("a"), "", Key("m2"))""" shouldNot compile
        }
        "given empty member2" in {
          """geodist(Key("a"), Key("m1"), "")""" shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and members" in forAll("key", "member1", "member2", "return value") { (k: Key, m1: Key, m2: Key, ovd: Option[ValidDouble]) =>
          val protocol = geodist(k, m1, m2)

          protocol.encode shouldBe arr(bulk("GEODIST"), bulk(k.show), bulk(m1.show), bulk(m2.show))
          protocol.decode(ovd.fold(nullBulk: GenBulk)(vd => bulk(vd.show))).right.value shouldBe ovd
        }
        "given non empty key, members and unit" in forAll("key", "member1", "member2", "unit", "return value") { (k: Key, m1: Key, m2: Key, u: Unit, ovd: Option[ValidDouble]) =>
          val protocol = geodist(k, m1, m2, u)

          protocol.encode shouldBe arr(bulk("GEODIST"), bulk(k.show), bulk(m1.show), bulk(m2.show), bulk(u.show))
          protocol.decode(ovd.fold(nullBulk: GenBulk)(vd => bulk(vd.show))).right.value shouldBe ovd
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
          OneOrMoreKeys.from(List.empty).right.map(geohash("a", _)).left.value shouldBe "Predicate isEmpty(List()) did not fail."
        }
        "given empty list of members to unsafely refine" in {
          the[IllegalArgumentException].thrownBy {
            geohash("a", OneOrMoreKeys.unsafeFrom(List.empty))
          }.getMessage shouldBe "Predicate isEmpty(List()) did not fail."
        }
      }

      "compile successfully" when {
        "given non empty key and non empty list of members" in {
          forAll("key", "members", "return value") { (k: Key, ms: OneOrMoreKeys, oghs: OneOrMore[Option[GeoHash]]) =>
            val protocol = geohash(k, ms)

            protocol.encode shouldBe arr(bulk("GEOHASH") :: bulk(k.show) :: ms.map(m => bulk(m.show)))
            protocol.decode(oneOrMoreGeoHashOptionToArr(oghs)).right.value shouldBe oghs.value
          }
        }
      }

    }
  }
}
