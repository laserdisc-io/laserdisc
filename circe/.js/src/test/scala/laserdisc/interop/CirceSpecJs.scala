package laserdisc
package interop

import laserdisc.interop.circe._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class CirceSpecJs extends AnyWordSpec with Matchers with EitherTestSyntax {
  "handling an invalid json" should {
    "fail to decode" in {
      Read[Bulk, Bar].read(Bulk("{")) onLeft (_.message shouldBe "ParsingFailure: SyntaxError: Unexpected end of JSON input")
    }
  }
}
