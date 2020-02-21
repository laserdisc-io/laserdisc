package laserdisc
package interop

import laserdisc.interop.circe._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class CirceSpecJvm extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks with EitherTestSyntax {
  "handling an invalid json" should {
    "fail to decode" in {
      Read[Bulk, Bar].read(Bulk("{")) onLeft (_.message shouldBe "ParsingFailure: exhausted input")
    }
  }
}
