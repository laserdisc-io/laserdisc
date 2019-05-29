package laserdisc
package interop

import io.circe._
import io.circe.syntax._

object circe {
  implicit final def encoderShow[A: Encoder]: Show[A] = Show.instance(_.asJson.noSpaces)
  implicit final def decoderRead[A: Decoder]: Bulk ==> A = Read.instance {
    case Bulk(s) =>
      parser.decode(s) match {
        case Right(a) => Some(a)
        case _        => None
      }
  }
}
