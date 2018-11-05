package laserdisc
package interop

import io.circe._
import io.circe.syntax._

object circe {
  implicit final def encoderShow[A: Encoder]: Show[A]         = Show.instance(_.asJson.noSpaces)
  implicit final def decoderRead[A: Decoder]: Read[String, A] = Read.instance((s: String) => parser.decode(s).toOption)
}
