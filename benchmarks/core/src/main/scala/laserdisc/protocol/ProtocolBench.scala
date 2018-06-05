package laserdisc.protocol

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import eu.timepit.refined.auto._
import laserdisc.Maybe
import laserdisc.protocol.RESP._
import shapeless._

@State(Scope.Benchmark)
class ProtocolBench {

  private final val protocol = Protocol("CUSTOM", _: Int :: String :: Long :: Double :: HNil).as[SimpleString, OK]

  private final val request  = 0 :: "a" :: 1L :: 2.0d :: HNil
  private final val response = str("OK")

  @Benchmark def encode(): RESP      = protocol(request).encode
  @Benchmark def decode(): Maybe[OK] = protocol(request).decode(response)
}
