package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class ValidDoubleSuite extends BaseSpec {
  test("ValidDouble fails to compile given Double.NaN") {
    assertNoDiff(
      compileErrors("ValidDouble(Double.NaN)"),
      """|error: Predicate failed: (NaN != NaN).
         |ValidDouble(Double.NaN)
         |           ^
         |""".stripMargin
    )
  }

  test("ValidDouble compiles given edge cases (-1.7976931348623157E308) -> can't use Double.MinValue as not a literal") {
    ValidDouble(-1.7976931348623157e308)
  }

  test("ValidDouble compiles given edge cases (Double.MaxValue)") {
    ValidDouble(Double.MaxValue)
  }

  property("ValidDouble refines correctly provided non literal cases of valid Doubles (d != Double.NaN)") {
    forAll(doubles.filter(validDoubleIsValid)) { d =>
      ValidDouble.from(d) onRight (_.value == d)
    }
  }
}
