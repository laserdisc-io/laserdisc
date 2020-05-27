package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class GeoHashSuite extends BaseSpec {
  test("GeoHash fails to compile given a non conformant String (length < 11)") {
    assertNoDiff(
      compileErrors("""GeoHash("abcdefghij")"""),
      """|error: Predicate failed: "abcdefghij".matches("[a-z0-9]{11}").
         |GeoHash("abcdefghij")
         |       ^
         |""".stripMargin
    )
  }

  test("GeoHash fails to compile given a non conformant String (length > 11)") {
    assertNoDiff(
      compileErrors("""GeoHash("abcdefghijkl")"""),
      """|error: Predicate failed: "abcdefghijkl".matches("[a-z0-9]{11}").
         |GeoHash("abcdefghijkl")
         |       ^
         |""".stripMargin
    )
  }

  test("GeoHash fails to compile given a non conformant String (uppercase)") {
    assertNoDiff(
      compileErrors("""GeoHash("abCdefghijk")"""),
      """|error: Predicate failed: "abCdefghijk".matches("[a-z0-9]{11}").
         |GeoHash("abCdefghijk")
         |       ^
         |""".stripMargin
    )
  }

  test("GeoHash fails to compile given a non conformant String (invalid chars)") {
    assertNoDiff(
      compileErrors("""GeoHash("abcd&fghijk")"""),
      """|error: Predicate failed: "abcd&fghijk".matches("[a-z0-9]{11}").
         |GeoHash("abcd&fghijk")
         |       ^
         |""".stripMargin
    )
  }

  property("GeoHash fails at runtime provided non literal cases of non conformant Strings") {
    forAll(strings.filterNot(geoHashIsValid)) { s =>
      intercept[IllegalArgumentException](GeoHash.unsafeFrom(s))
    }
  }

  test("GeoHash compiles given conformant String") {
    GeoHash("abcd3fgh1jk")
  }

  property("GeoHash refines correctly provided non literal cases of conformant Strings") {
    forAll(geoHashGen) { s =>
      GeoHash.from(s) onRight (_.value == s)
    }
  }
}
