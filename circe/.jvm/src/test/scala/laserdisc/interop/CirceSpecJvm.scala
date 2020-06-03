package laserdisc
package interop

import laserdisc.interop.circe._
import munit.FunSuite

final class CirceSpecJvm extends FunSuite with EitherTestSyntax {
  test("handling an invalid json fails to decode") {
    Read[Bulk, Bar].read(Bulk("{")) onLeft (e => assertEquals(e.message, "ParsingFailure: exhausted input"))
  }
}
