package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class StringLengthSuite extends BaseSpec {
  test("StringLength fails to compile given out of range Long (< 0L)") {
    assertNoDiff(
      compileErrors("StringLength(-1L)"),
      """|error: Left predicate of (!(-1 < 0) && !(-1 > 4294967295)) failed: Predicate (-1 < 0) did not fail.
         |StringLength(-1L)
         |            ^
         |""".stripMargin
    )
  }

  test("StringLength fails to compile given out of range Long (> 4294967295L)") {
    assertNoDiff(
      compileErrors("StringLength(4294967296L)"),
      """|error: Right predicate of (!(4294967296 < 0) && !(4294967296 > 4294967295)) failed: Predicate (4294967296 > 4294967295) did not fail.
         |StringLength(4294967296L)
         |            ^
         |""".stripMargin
    )
  }

  property("StringLength fails at runtime provided non literal cases of out of range Longs (l < 0L | l > 4294967295L)") {
    forAll(longs.filterNot(stringLengthIsValid)) { l =>
      intercept[IllegalArgumentException](StringLength.unsafeFrom(l))
    }
  }

  test("StringLength compiles given edge cases (0L)") {
    StringLength(0L)
  }

  test("StringLength compiles given edge cases (4294967295L)") {
    StringLength(4294967295L)
  }

  property("StringLength refines correctly provided non literal cases of in range Longs (0L <= l <= 4294967295L)") {
    forAll(stringLengthGen) { l =>
      StringLength.from(l) onRight (_.value == l)
    }
  }
}
