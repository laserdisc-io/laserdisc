package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class LongitudeSuite extends BaseSpec {
  test("Longitude fails to compile given out of range Double (< -180.0D)") {
    assertNoDiff(
      compileErrors("Longitude(-180.00000001D)"),
      """|error: Left predicate of (!(-180.00000001 < -180.0) && !(-180.00000001 > 180.0)) failed: Predicate (-180.00000001 < -180.0) did not fail.
         |Longitude(-180.00000001D)
         |         ^
         |""".stripMargin
    )
  }

  test("Longitude fails to compile given out of range Double (> 180.0D)") {
    assertNoDiff(
      compileErrors("Longitude(180.00000001D)"),
      """|error: Right predicate of (!(180.00000001 < -180.0) && !(180.00000001 > 180.0)) failed: Predicate (180.00000001 > 180.0) did not fail.
         |Longitude(180.00000001D)
         |         ^
         |""".stripMargin
    )
  }

  property("Longitude fails at runtime provided non literal cases of out of range Doubles (d < -180.0D | d > 180.0D)") {
    forAll(doubles.filterNot(longitudeIsValid)) { d =>
      intercept[IllegalArgumentException](Longitude.unsafeFrom(d))
    }
  }

  test("Longitude compiles given edge cases (-180.0D)") {
    Longitude(-180.0d)
  }

  test("Longitude compiles given edge cases (180.0D)") {
    Longitude(180.0d)
  }

  property("Longitude refines correctly provided non literal cases of in range Doubles (-180.0D <= d <= 180.0D)") {
    forAll(longitudeGen) { l =>
      Longitude.from(l) onRight (_.value == l)
    }
  }
}
