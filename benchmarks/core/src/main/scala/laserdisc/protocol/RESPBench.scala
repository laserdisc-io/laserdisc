package laserdisc
package protocol

import java.nio.charset.StandardCharsets.UTF_8

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scodec.bits.BitVector
import scodec.codecs.utf8
import scodec.{Attempt, Codec}

@State(Scope.Benchmark)
class RESPBench {

  private final val codec = Codec[RESP]

  private final val chars = 2000

  private final val ok              = "OK"
  private final val okRedis         = s"+$ok$CRLF"
  private final val rtProblem       = "runtime problem"
  private final val rtProblemRedis  = s"-$rtProblem$CRLF"
  private final val fortyTwo        = 42L
  private final val fortyTwoRedis   = s":$fortyTwo$CRLF"
  private final val longString      = new String(Array.fill(chars)('a'))
  private final val longStringRedis = s"$$$chars$CRLF$longString$CRLF"
  private final val longStringI =
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

  private final val str             = Str(ok)
  private final val strBits         = BitVector(okRedis.getBytes(UTF_8))
  private final val err             = Err(rtProblem)
  private final val errBits         = BitVector(rtProblemRedis.getBytes(UTF_8))
  private final val num             = Num(fortyTwo)
  private final val numBits         = BitVector(fortyTwoRedis.getBytes(UTF_8))
  private final val bulk            = Bulk(longString)
  private final val bulkBits        = BitVector(longStringRedis.getBytes(UTF_8))
  private final val longStringBits  = BitVector(longString.getBytes(UTF_8))
  private final val longStringIBits = BitVector(longStringI.getBytes(UTF_8))

  @Benchmark def baseline_utf8_encode: Attempt[BitVector]  = utf8.encode(longString)
  @Benchmark def baseline_utf8_decode: Attempt[String]     = utf8.decodeValue(longStringBits)
  @Benchmark def baseline_utf8_encodeI: Attempt[BitVector] = utf8.encode(longStringI)
  @Benchmark def baseline_utf8_decodeI: Attempt[String]    = utf8.decodeValue(longStringIBits)
  @Benchmark def str_encode: Attempt[BitVector]            = codec.encode(str)
  @Benchmark def str_decode: Attempt[RESP]                 = codec.decodeValue(strBits)
  @Benchmark def err_encode: Attempt[BitVector]            = codec.encode(err)
  @Benchmark def err_decode: Attempt[RESP]                 = codec.decodeValue(errBits)
  @Benchmark def num_encode: Attempt[BitVector]            = codec.encode(num)
  @Benchmark def num_decode: Attempt[RESP]                 = codec.decodeValue(numBits)
  @Benchmark def bulk_encode: Attempt[BitVector]           = codec.encode(bulk)
  @Benchmark def bulk_decode: Attempt[RESP]                = codec.decodeValue(bulkBits)
}
