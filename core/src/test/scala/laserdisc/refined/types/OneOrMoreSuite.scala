package laserdisc
package refined.types

import org.scalacheck.Prop.forAll

final class OneOrMoreSuite extends BaseSpec {
  property("OneOrMore fails at runtime provided empty List") {
    intercept[IllegalArgumentException](OneOrMore.unsafeFrom(List.empty[Int]))
  }

  property("OneOrMore refines correctly provided non literal cases of non empty Lists (length > 0)") {
    forAll(intLists.filter(_.nonEmpty)) { l =>
      OneOrMore.from(l) onRight (_.value == l)
    }
  }
}
