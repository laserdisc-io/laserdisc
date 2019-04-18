package laserdisc
package protocol

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import shapeless._

@State(Scope.Benchmark)
class RESPParamWriteBench {

  private final val respParamWrite = RESPParamWrite[Int :: String :: Long :: Double :: HNil]

  private final val value = 0 :: "a" :: 1L :: 2.0d :: HNil

  @Benchmark def write(): Seq[GenBulk] = respParamWrite.write(value)
}
