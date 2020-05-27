package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class NonNegIntSuite extends BaseSpec {
  test("NonNegInt fails to compile given out of range Ints (< 0)") {
    assertNoDiff(
      compileErrors("NonNegInt(-1)"),
      """|error: Predicate (-1 < 0) did not fail.
         |NonNegInt(-1)
         |         ^
         |""".stripMargin
    )
  }

  property("NonNegInt fails at runtime provided non literal cases of out of range Ints (i < 0)") {
    forAll(ints.filterNot(nonNegIntIsValid)) { i =>
      intercept[IllegalArgumentException](NonNegInt.unsafeFrom(i))
    }
  }

  test("NonNegInt compiles given edge cases (0)") {
    NonNegInt(0)
  }

  property("NonNegInt refines correctly provided non literal cases of in range Ints (i > 0)") {
    forAll(nonNegIntGen) { i =>
      NonNegInt.from(i) onRight (_.value == i)
    }
  }
}
