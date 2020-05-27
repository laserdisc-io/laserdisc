package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class NonZeroIntSuite extends BaseSpec {
  test("NonZeroInt fails to compile given 0") {
    assertNoDiff(
      compileErrors("NonZeroInt(0)"),
      """|error: Predicate (0 == 0) did not fail.
         |NonZeroInt(0)
         |          ^
         |""".stripMargin
    )
  }

  property("NonZeroInt refines correctly provided non literal cases of valid Ints (i != 0)") {
    forAll(nonZeroIntGen) { i =>
      NonZeroInt.from(i) onRight (_.value == i)
    }
  }
}
