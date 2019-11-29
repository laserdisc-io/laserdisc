package laserdisc
package interop

import laserdisc.interop.circe._
import org.scalatest.{Matchers, OptionValues, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

final class CirceSpecJs extends WordSpec with Matchers with ScalaCheckPropertyChecks with OptionValues with EitherTestSyntax {

  "handling an invalid json" should {
    "fail to decode" in {
      Read[Bulk, Bar].read(Bulk("{")) onLeft (_.message shouldBe "ParsingFailure: SyntaxError: Unexpected end of JSON input")
    }
  }
}
