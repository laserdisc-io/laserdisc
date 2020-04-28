package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class NonZeroDoubleSuite extends BaseSpec {
  test("NonZeroDouble fails to compile given 0.0D") {
    assertNoDiff(
      compileErrors("NonZeroDouble(0.0D)"),
      """|error: Right predicate of ((0.0 != NaN) && !(0.0 == 0.0)) failed: Predicate (0.0 == 0.0) did not fail.
         |NonZeroDouble(0.0D)
         |             ^
         |""".stripMargin
    )
  }

  test("NonZeroDouble fails to compile given NaN") {
    assertNoDiff(
      compileErrors("NonZeroDouble(Double.NaN)"),
      """|error: Left predicate of ((NaN != NaN) && !(NaN == 0.0)) failed: Predicate failed: (NaN != NaN).
         |NonZeroDouble(Double.NaN)
         |             ^
         |""".stripMargin
    )
  }

  property("NonZeroDouble refines correctly provided non literal cases of valid Doubles (d != 0.0D)") {
    forAll(nonZeroDoubleGen) { d =>
      NonZeroDouble.from(d) onRight (_.value == d)
    }
  }
}
