package laserdisc
package interop

import laserdisc.interop.circe._
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

final class CirceSpecJvm extends WordSpec with Matchers with ScalaCheckPropertyChecks with EitherTestSyntax {
  "handling an invalid json" should {
    "fail to decode" in {
      Read[Bulk, Bar].read(Bulk("{")) onLeft (_.message shouldBe "ParsingFailure: exhausted input")
    }
  }
}
