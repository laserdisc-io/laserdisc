package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class NodeIdSuite extends BaseSpec {
  test("NodeId fails to compile given a non conformant String (length < 40)") {
    assertNoDiff(
      compileErrors("""NodeId("0123456789abcdef0123456789abcdef0123456")"""),
      """|error: Predicate failed: "0123456789abcdef0123456789abcdef0123456".matches("[0-9a-f]{40}").
         |NodeId("0123456789abcdef0123456789abcdef0123456")
         |      ^
         |""".stripMargin
    )
  }

  test("NodeId fails to compile given a non conformant String (length > 40)") {
    assertNoDiff(
      compileErrors("""NodeId("0123456789abcdef0123456789abcdef012345678")"""),
      """|error: Predicate failed: "0123456789abcdef0123456789abcdef012345678".matches("[0-9a-f]{40}").
         |NodeId("0123456789abcdef0123456789abcdef012345678")
         |      ^
         |""".stripMargin
    )
  }

  test("NodeId fails to compile given a non conformant String (uppercase)") {
    assertNoDiff(
      compileErrors("""NodeId("0123456789abcdEf0123456789abcdef01234567")"""),
      """|error: Predicate failed: "0123456789abcdEf0123456789abcdef01234567".matches("[0-9a-f]{40}").
         |NodeId("0123456789abcdEf0123456789abcdef01234567")
         |      ^
         |""".stripMargin
    )
  }

  test("NodeId fails to compile given a non conformant String (invalid chars)") {
    assertNoDiff(
      compileErrors("""NodeId("0123456789abcd&f0123456789abcdef01234567&fghijk")"""),
      """|error: Predicate failed: "0123456789abcd&f0123456789abcdef01234567&fghijk".matches("[0-9a-f]{40}").
         |NodeId("0123456789abcd&f0123456789abcdef01234567&fghijk")
         |      ^
         |""".stripMargin
    )
  }

  property("NodeId fails at runtime provided non literal cases of non conformant Strings") {
    forAll(strings.filterNot(nodeIdIsValid)) { d =>
      intercept[IllegalArgumentException](NodeId.unsafeFrom(d))
    }
  }

  test("NodeId compiles given conformant String") {
    NodeId("0123456789abcdef0123456789abcdef01234567")
  }

  property("NodeId refines correctly provided non literal cases of conformant Strings") {
    forAll(nodeIdGen) { i =>
      NodeId.from(i) onRight (_.value == i)
    }
  }
}
