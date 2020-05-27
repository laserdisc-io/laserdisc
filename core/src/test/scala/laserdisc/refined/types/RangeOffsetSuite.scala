package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class RangeOffsetSuite extends BaseSpec {
  test("RangeOffset fails to compile given out of range Int (< 0)") {
    assertNoDiff(
      compileErrors("RangeOffset(-1)"),
      """|error: Left predicate of (!(-1 < 0) && !(-1 > 536870911)) failed: Predicate (-1 < 0) did not fail.
         |RangeOffset(-1)
         |           ^
         |""".stripMargin
    )
  }

  test("RangeOffset fails to compile given out of range Int (> 536870911)") {
    assertNoDiff(
      compileErrors("RangeOffset(536870912)"),
      """|error: Right predicate of (!(536870912 < 0) && !(536870912 > 536870911)) failed: Predicate (536870912 > 536870911) did not fail.
         |RangeOffset(536870912)
         |           ^
         |""".stripMargin
    )
  }

  property("RangeOffset fails at runtime provided non literal cases of out of range Ints (i < 0 | i > 536870911)") {
    forAll(ints.filterNot(rangeOffsetIsValid)) { i =>
      intercept[IllegalArgumentException](RangeOffset.unsafeFrom(i))
    }
  }

  test("RangeOffset compiles given edge cases (0)") {
    RangeOffset(0)
  }

  test("RangeOffset compiles given edge cases (536870911)") {
    RangeOffset(536870911)
  }

  property("RangeOffset refines correctly provided non literal cases of in range Ints (0 <= i <= 536870911)") {
    forAll(rangeOffsetGen) { i =>
      RangeOffset.from(i) onRight (_.value == i)
    }
  }
}
