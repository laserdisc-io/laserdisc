package laserdisc.protocol

import java.nio.charset.StandardCharsets.UTF_8

import laserdisc.protocol.RESP._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scodec.bits.BitVector
import scodec.codecs.utf8
import scodec.{Attempt, Codec}

@State(Scope.Benchmark)
class RESPBench {

  private final val codec = Codec[RESP]

  private final val chars = 2000

  private final val ok              = "OK"
  private final val okRedis         = s"+$ok\r\n"
  private final val rtProblem       = "runtime problem"
  private final val rtProblemRedis  = s"-$rtProblem\r\n"
  private final val fortyTwo        = 42L
  private final val fortyTwoRedis   = s":$fortyTwo\r\n"
  private final val longString      = new String(Array.fill(chars)('a'))
  private final val longStringRedis = s"$$$chars\r\n$longString\r\n"
  private final val longStringI =
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

  private final val simpleString     = str(ok)
  private final val simpleStringBits = BitVector(okRedis.getBytes(UTF_8))
  private final val error            = err(rtProblem)
  private final val errorBits        = BitVector(rtProblemRedis.getBytes(UTF_8))
  private final val integer          = int(fortyTwo)
  private final val integerBits      = BitVector(fortyTwoRedis.getBytes(UTF_8))
  private final val bulkString       = bulk(longString)
  private final val bulkStringBits   = BitVector(longStringRedis.getBytes(UTF_8))
  private final val longStringBits   = BitVector(longString.getBytes(UTF_8))
  private final val longStringIBits  = BitVector(longStringI.getBytes(UTF_8))

  @Benchmark def baseline_utf8_encode: Attempt[BitVector]  = utf8.encode(longString)
  @Benchmark def baseline_utf8_decode: Attempt[String]     = utf8.decodeValue(longStringBits)
  @Benchmark def baseline_utf8_encodeI: Attempt[BitVector] = utf8.encode(longStringI)
  @Benchmark def baseline_utf8_decodeI: Attempt[String]    = utf8.decodeValue(longStringIBits)
  @Benchmark def simpleString_encode: Attempt[BitVector]   = codec.encode(simpleString)
  @Benchmark def simpleString_decode: Attempt[RESP]        = codec.decodeValue(simpleStringBits)
  @Benchmark def error_encode: Attempt[BitVector]          = codec.encode(error)
  @Benchmark def error_decode: Attempt[RESP]               = codec.decodeValue(errorBits)
  @Benchmark def integer_encode: Attempt[BitVector]        = codec.encode(integer)
  @Benchmark def integer_decode: Attempt[RESP]             = codec.decodeValue(integerBits)
  @Benchmark def bulkString_encode: Attempt[BitVector]     = codec.encode(bulkString)
  @Benchmark def bulkString_decode: Attempt[RESP]          = codec.decodeValue(bulkStringBits)
}
