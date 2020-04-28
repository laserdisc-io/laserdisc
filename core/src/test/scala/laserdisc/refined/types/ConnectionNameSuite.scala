package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class ConnectionNameSuite extends BaseSpec {
  test("ConnectionName fails to compile given an empty string") {
    assertNoDiff(
      compileErrors("""ConnectionName("")"""),
      """|error: Left predicate of (!isEmpty() && ()) failed: Predicate isEmpty() did not fail.
         |ConnectionName("")
         |              ^
         |""".stripMargin
    )
  }

  test("ConnectionName fails to compile given a space") {
    assertNoDiff(
      compileErrors("""ConnectionName(" ")"""),
      """|error: Right predicate of (!isEmpty( ) && ((!isWhitespace(' ') && !isControl(' ')))) failed: Predicate failed: ((!isWhitespace(' ') && !isControl(' '))).
         |ConnectionName(" ")
         |              ^
         |""".stripMargin
    )
  }

  property("ConnectionName fails at runtime provided non literal cases of strings that contain spaces") {
    forAll(stringsWithSpacesGen.filterNot(connectionNameIsValid)) { s =>
      intercept[IllegalArgumentException](ConnectionName.unsafeFrom(s))
    }
  }

  test("ConnectionName compiles given non empty String with no spaces") {
    ConnectionName("a")
  }

  property("ConnectionName refines correctly provided non literal cases of non empty strings with no spaces") {
    forAll(connectionNameGen) { s =>
      ConnectionName.from(s) onRight (_.value == s)
    }
  }
}
