package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class OneOrMoreKeysSuite extends BaseSpec {
  test("OneOrMoreKeys fails to compile given non literal empty List") {
    assertNoDiff(
      compileErrors("OneOrMoreKeys(List.empty)"),
      """|error: compile-time refinement only works with literals
         |OneOrMoreKeys(List.empty)
         |             ^
         |""".stripMargin
    )
  }

  test("OneOrMoreKeys fails to compile given non literal non empty List") {
    assertNoDiff(
      compileErrors("""OneOrMoreKeys(List(Key("a")))"""),
      """|error: compile-time refinement only works with literals
         |OneOrMoreKeys(List(Key("a")))
         |             ^
         |""".stripMargin
    )
  }

  test("OneOrMoreKeys fails at runtime provided empty List") {
    intercept[IllegalArgumentException](OneOrMoreKeys.unsafeFrom(List.empty))
  }

  property("OneOrMoreKeys refines correctly provided non literal cases of non empty Lists (length > 0)") {
    forAll(keyLists.filter(_.nonEmpty)) { ks =>
      OneOrMoreKeys.from(ks) onRight (_.value == ks)
    }
  }
}
