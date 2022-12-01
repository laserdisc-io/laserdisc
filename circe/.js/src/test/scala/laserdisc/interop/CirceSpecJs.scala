package laserdisc
package interop

import laserdisc.interop.circe._
import munit.FunSuite

final class CirceSpecJs extends FunSuite with EitherTestSyntax {
  test("handling an invalid json fails to decode") {
    Read[Bulk, Bar].read(Bulk("{")) onLeft { e =>
      assert(e.message.startsWith("ParsingFailure: SyntaxError:"))
    }
  }
}
