package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class NonNegDoubleSuite extends BaseSpec {
  test("NonNegDouble fails to compile given out of range Double (< 0.0D)") {
    assertNoDiff(
      compileErrors("NonNegDouble(-0.00000001D)"),
      """|error: Right predicate of ((-1.0E-8 != NaN) && !(-1.0E-8 < 0.0)) failed: Predicate (-1.0E-8 < 0.0) did not fail.
         |NonNegDouble(-0.00000001D)
         |            ^
         |""".stripMargin
    )
  }

  test("NonNegDouble fails to compile given NaN Double") {
    assertNoDiff(
      compileErrors("NonNegDouble(Double.NaN)"),
      """|error: Left predicate of ((NaN != NaN) && !(NaN < 0.0)) failed: Predicate failed: (NaN != NaN).
         |NonNegDouble(Double.NaN)
         |            ^
         |""".stripMargin
    )
  }

  property("NonNegDouble fails at runtime provided non literal cases of out of range Doubles (d < 0.0D)") {
    forAll(doubles.filterNot(nonNegDoubleIsValid)) { d =>
      intercept[IllegalArgumentException](NonNegDouble.unsafeFrom(d))
    }
  }

  test("NonNegDouble compiles given edge cases (0.0D)") {
    NonNegDouble(0.0d)
  }

  property("NonNegDouble refines correctly provided non literal cases of in range Doubles (d >= 0.0D)") {
    forAll(nonNegDoubleGen) { d =>
      NonNegDouble.from(d) onRight (_.value == d)
    }
  }
}
