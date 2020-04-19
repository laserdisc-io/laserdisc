package laserdisc
package protocol

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import shapeless._

@State(Scope.Benchmark)
class ProtocolBenchStr {
  private final val protocol = Protocol("CUSTOM", _: Int :: String :: Long :: Double :: HNil).as[Str, OK]

  private final val request  = 0 :: "a" :: 1L :: 2.0d :: HNil
  private final val response = Str("OK")

  @Benchmark def encode(): RESP      = protocol(request).encode
  @Benchmark def decode(): Maybe[OK] = protocol(request).decode(response)
}
