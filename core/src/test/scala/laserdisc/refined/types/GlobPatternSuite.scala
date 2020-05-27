package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class GlobPatternSuite extends BaseSpec {
  test("GlobPattern fails to compile given an empty String") {
    assertNoDiff(
      compileErrors("""GlobPattern("")"""),
      """|error: Predicate failed: "".matches("(\[?[\w\*\?]+\]?)+").
         |GlobPattern("")
         |           ^
         |""".stripMargin
    )
  }

  test("GlobPattern fails to compile given a non conformant String") {
    assertNoDiff(
      compileErrors("""GlobPattern("!")"""),
      """|error: Predicate failed: "!".matches("(\[?[\w\*\?]+\]?)+").
         |GlobPattern("!")
         |           ^
         |""".stripMargin
    )
  }

  property("GlobPattern fails at runtime provided non literal cases of non conformant Strings") {
    forAll(strings.filterNot(globPatternIsValid)) { s =>
      intercept[IllegalArgumentException](GlobPattern.unsafeFrom(s))
    }
  }

  test("GlobPattern compiles given conformant String") {
    GlobPattern("abc*fg?1jk")
    GlobPattern("a[bc*]fg?1jk")
  }

  property("GlobPattern refines correctly provided non literal cases of conformant Strings") {
    forAll(globPatternGen) { s =>
      GlobPattern.from(s) onRight (_.value == s)
    }
  }
}
