package laserdisc
package interop

import io.circe._
import cats.syntax.either._
import cats.syntax.show._
import io.circe.syntax._

object circe {
  implicit final def encoderShow[A: Encoder]: Show[A] = Show.instance(_.asJson.noSpaces)
  implicit final def decoderRead[A: Decoder]: Bulk ==> A = Read.instance {
    case Bulk(s) => parser.decode(s).leftMap(err => RESPDecErr(err.show))
  }
}
