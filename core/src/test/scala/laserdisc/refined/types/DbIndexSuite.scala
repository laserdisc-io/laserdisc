package laserdisc
package refined.types

import org.scalacheck.Prop._

final class DbIndexSuite extends BaseSpec {
  test("DbIndex fail to compile given out of range Int (< 0)") {
    assertNoDiff(
      compileErrors("DbIndex(-1)"),
      """|error: Left predicate of (!(-1 < 0) && !(-1 > 15)) failed: Predicate (-1 < 0) did not fail.
         |DbIndex(-1)
         |       ^
         |""".stripMargin
    )
  }

  test("DbIndex fail to compile given out of range Int (> 15)") {
    assertNoDiff(
      compileErrors("DbIndex(16)"),
      """|error: Right predicate of (!(16 < 0) && !(16 > 15)) failed: Predicate (16 > 15) did not fail.
         |DbIndex(16)
         |       ^
         |""".stripMargin
    )
  }

  property("DbIndex fails at runtime provided non literal cases of out of range Ints") {
    forAll(ints.filterNot(dbIndexIsValid)) { i =>
      intercept[IllegalArgumentException](DbIndex.unsafeFrom(i))
    }
  }

  test("DbIndex compiles given in range Int") {
    DbIndex(0)
  }

  property("DbIndex refines correctly provided non literal cases of in range Ints") {
    forAll(dbIndexGen) { i =>
      DbIndex.from(i) onRight (_.value == i)
    }
  }
}
