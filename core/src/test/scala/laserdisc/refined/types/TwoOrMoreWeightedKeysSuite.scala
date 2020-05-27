package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class TwoOrMoreWeightedKeysSuite extends BaseSpec {
  test("TwoOrMoreWeightedKeys fails to compile given non literal empty List") {
    assertNoDiff(
      compileErrors("TwoOrMoreWeightedKeys(List.empty)"),
      """|error: compile-time refinement only works with literals
         |TwoOrMoreWeightedKeys(List.empty)
         |                     ^
         |""".stripMargin
    )
  }

  test("TwoOrMoreWeightedKeys fails to compile given non literal single element List") {
    assertNoDiff(
      compileErrors("""TwoOrMoreWeightedKeys(List(Key("a") -> ValidDouble(42.0D)))"""),
      """|error: compile-time refinement only works with literals
         |TwoOrMoreWeightedKeys(List(Key("a") -> ValidDouble(42.0D)))
         |                     ^
         |""".stripMargin
    )
  }

  test("TwoOrMoreWeightedKeys fails to compile given non literal List of two elements") {
    assertNoDiff(
      compileErrors("""TwoOrMoreWeightedKeys(List(Key("a") -> ValidDouble(42.0D), Key("b") -> ValidDouble(23.0D)))"""),
      """|error: compile-time refinement only works with literals
         |TwoOrMoreWeightedKeys(List(Key("a") -> ValidDouble(42.0D), Key("b") -> ValidDouble(23.0D)))
         |                     ^
         |""".stripMargin
    )
  }

  test("TwoOrMoreWeightedKeys fails at runtime provided empty List") {
    intercept[IllegalArgumentException](TwoOrMoreWeightedKeys.unsafeFrom(List.empty))
  }

  test("TwoOrMoreWeightedKeys fails at runtime provided single element List") {
    intercept[IllegalArgumentException](TwoOrMoreWeightedKeys.unsafeFrom(List(Key("a") -> ValidDouble(42.0d))))
  }

  property("TwoOrMoreWeightedKeys refines correctly provided non literal cases of Lists of length > 1") {
    forAll(wightedKeyLists.filter(_.size > 1)) { ks =>
      TwoOrMoreWeightedKeys.from(ks) onRight (_.value == ks)
    }
  }
}
