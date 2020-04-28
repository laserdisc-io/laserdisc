package laserdisc
package refined.types

final class KeySuite extends BaseSpec {
  test("Key fails to compile given an empty String") {
    assertNoDiff(
      compileErrors("""Key("")"""),
      """|error: Left predicate of (!isEmpty() && ()) failed: Predicate isEmpty() did not fail.
         |Key("")
         |   ^
         |""".stripMargin
    )
  }

  test("Key compiles given a non empty String") {
    Key("a")
  }
}
