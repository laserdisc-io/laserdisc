package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class LatitudeSuite extends BaseSpec {
  test("Latitude fails to compile given out of range Double (< -85.05112878D)") {
    assertNoDiff(
      compileErrors("Latitude(-85.05112879D)"),
      """|error: Left predicate of (!(-85.05112879 < -85.05112878) && !(-85.05112879 > 85.05112878)) failed: Predicate (-85.05112879 < -85.05112878) did not fail.
         |Latitude(-85.05112879D)
         |        ^
         |""".stripMargin
    )
  }

  test("Latitude fails to compile given out of range Double (> 85.05112878D)") {
    assertNoDiff(
      compileErrors("Latitude(85.05112879D)"),
      """|error: Right predicate of (!(85.05112879 < -85.05112878) && !(85.05112879 > 85.05112878)) failed: Predicate (85.05112879 > 85.05112878) did not fail.
         |Latitude(85.05112879D)
         |        ^
         |""".stripMargin
    )
  }

  property("Latitude fails at runtime provided non literal cases of out of range Doubles (d < -85.05112878D | d > 85.05112878D)") {
    forAll(doubles.filterNot(latitudeIsValid)) { d =>
      intercept[IllegalArgumentException](Latitude.unsafeFrom(d))
    }
  }

  test("Key compiles given edge cases (-85.05112878D)") {
    Latitude(-85.05112878d)
  }

  test("Key compiles given edge cases (85.05112878D)") {
    Latitude(85.05112878d)
  }

  property("Latitude refines correctly provided non literal cases of in range Doubles (-85.05112878D <= d <= 85.05112878D)") {
    forAll(latitudeGen) { l =>
      Latitude.from(l) onRight (_.value == l)
    }
  }
}
