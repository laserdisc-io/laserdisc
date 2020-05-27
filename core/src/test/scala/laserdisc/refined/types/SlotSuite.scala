package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class SlotSuite extends BaseSpec {
  test("Slot fails to compile given out of range Int (< 0)") {
    assertNoDiff(
      compileErrors("Slot(-1)"),
      """|error: Left predicate of (!(-1 < 0) && !(-1 > 16383)) failed: Predicate (-1 < 0) did not fail.
         |Slot(-1)
         |    ^
         |""".stripMargin
    )
  }

  test("Slot fails to compile given out of range Int (> 16383)") {
    assertNoDiff(
      compileErrors("Slot(16384)"),
      """|error: Right predicate of (!(16384 < 0) && !(16384 > 16383)) failed: Predicate (16384 > 16383) did not fail.
         |Slot(16384)
         |    ^
         |""".stripMargin
    )
  }

  property("Slot fails at runtime provided non literal cases of out of range Ints (i < 0 | i > 16383)") {
    forAll(ints.filterNot(slotIsValid)) { i =>
      intercept[IllegalArgumentException](Slot.unsafeFrom(i))
    }
  }

  test("NonNegLong compiles given edge cases (0)") {
    Slot(0)
  }

  test("NonNegLong compiles given edge cases (16383)") {
    Slot(16383)
  }
}
