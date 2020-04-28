package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class NonNegLongSuite extends BaseSpec {
  test("NonNegLong fails to compile given out of range Longs (< 0L)") {
    assertNoDiff(
      compileErrors("NonNegLong(-1L)"),
      """|error: Predicate (-1 < 0) did not fail.
         |NonNegLong(-1L)
         |          ^
         |""".stripMargin
    )
  }

  property("NonNegLong fails at runtime provided non literal cases of out of range Longs (l < 0L)") {
    forAll(longs.filterNot(nonNegLongIsValid)) { l =>
      intercept[IllegalArgumentException](NonNegLong.unsafeFrom(l))
    }
  }

  test("NonNegLong compiles given edge cases (0L)") {
    NonNegLong(0L)
  }

  property("NonNegLong refines correctly provided non literal cases of in range Longs (l > 0L)") {
    forAll(nonNegLongGen) { l =>
      NonNegLong.from(l) onRight (_.value == l)
    }
  }
}
