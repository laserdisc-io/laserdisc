package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class TwoOrMoreKeysSuite extends BaseSpec {
  test("TwoOrMoreKeys fails to compile given non literal empty List") {
    assertNoDiff(
      compileErrors("TwoOrMoreKeys(List.empty)"),
      """|error: compile-time refinement only works with literals
         |TwoOrMoreKeys(List.empty)
         |             ^
         |""".stripMargin
    )
  }

  test("TwoOrMoreKeys fails to compile given non literal single element List") {
    assertNoDiff(
      compileErrors("""TwoOrMoreKeys(List(Key("a")))"""),
      """|error: compile-time refinement only works with literals
         |TwoOrMoreKeys(List(Key("a")))
         |             ^
         |""".stripMargin
    )
  }

  test("TwoOrMoreKeys fails to compile given non literal List of two elements") {
    assertNoDiff(
      compileErrors("""TwoOrMoreKeys(List(Key("a"), Key("b")))"""),
      """|error: compile-time refinement only works with literals
         |TwoOrMoreKeys(List(Key("a"), Key("b")))
         |             ^
         |""".stripMargin
    )
  }

  property("TwoOrMoreKeys fails at runtime provided empty List") {
    intercept[IllegalArgumentException](TwoOrMoreKeys.unsafeFrom(List.empty))
  }

  property("TwoOrMoreKeys fails at runtime provided single element List") {
    intercept[IllegalArgumentException](TwoOrMoreKeys.unsafeFrom(List(Key("a"))))
  }

  property("TwoOrMoreKeys refines correctly provided non literal cases of Lists of length > 1") {
    forAll(keyLists.filter(_.size > 1)) { ks =>
      TwoOrMoreKeys.from(ks) onRight (_.value == ks)
    }
  }
}
