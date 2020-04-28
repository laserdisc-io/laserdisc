package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class NonZeroLongSuite extends BaseSpec {
  test("NonZeroLong fails to compile given 0L") {
    assertNoDiff(
      compileErrors("NonZeroLong(0L)"),
      """|error: Predicate (0 == 0) did not fail.
         |NonZeroLong(0L)
         |           ^
         |""".stripMargin
    )
  }

  property("NonZeroLong refines correctly provided non literal cases of valid Longs (l != 0L)") {
    forAll(nonZeroLongGen) { l =>
      NonZeroLong.from(l) onRight (_.value == l)
    }
  }
}
