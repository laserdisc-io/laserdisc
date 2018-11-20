package laserdisc
package interop

import io.circe._
import io.circe.syntax._
import laserdisc.protocol.NonNullBulkString

object circe {
  implicit final def encoderShow[A: Encoder]: Show[A] = Show.instance(_.asJson.noSpaces)
  implicit final def decoderRead[A: Decoder]: Read[NonNullBulkString, A] = Read.instance {
    case NonNullBulkString(s) =>
      parser.decode(s) match {
        case Right(a) => Some(a)
        case _        => None
      }
  }
}
