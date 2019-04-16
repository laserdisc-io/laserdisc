package laserdisc
package protocol

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

import org.openjdk.jmh.annotations._
import scodec.Attempt
import scodec.bits.BitVector

@State(Scope.Benchmark)
@Warmup(time = 1)
@Measurement(time = 1)
class UTF8EncodingBench {

  private final val defaultUtf8 = scodec.codecs.utf8
  private final val lenientUtf8 = new LenientStringCodec(UTF_8)

  private final val ok          = "OK"
  private final val okBytes     = ok.getBytes(UTF_8)
  private final val okBitVector = BitVector.view(okBytes)

  private final val longString          = new String(Array.fill(2000)('a'))
  private final val longStringBytes     = longString.getBytes(UTF_8)
  private final val longStringBitVector = BitVector.view(longStringBytes)

  @Benchmark def decode_ok_baseline_charset: String      = UTF_8.decode(ByteBuffer.wrap(okBytes)).toString
  @Benchmark def decode_ok_baseline_string: String       = new String(okBytes, UTF_8)
  @Benchmark def decode_ok_default_utf8: Attempt[String] = defaultUtf8.decodeValue(okBitVector)
  @Benchmark def decode_ok_lenient_utf8: Attempt[String] = lenientUtf8.decodeValue(okBitVector)

  @Benchmark def decode_long_string_baseline_charset: String      = UTF_8.decode(ByteBuffer.wrap(longStringBytes)).toString
  @Benchmark def decode_long_string_baseline_string: String       = new String(longStringBytes, UTF_8)
  @Benchmark def decode_long_string_default_utf8: Attempt[String] = defaultUtf8.decodeValue(longStringBitVector)
  @Benchmark def decode_long_string_lenient_utf8: Attempt[String] = lenientUtf8.decodeValue(longStringBitVector)

  @Benchmark def encode_ok_baseline_charset: ByteBuffer       = UTF_8.encode(ok)
  @Benchmark def encode_ok_baseline_string: scala.Array[Byte] = ok.getBytes(UTF_8)
  @Benchmark def encode_ok_default_utf8: Attempt[BitVector]   = defaultUtf8.encode(ok)
  @Benchmark def encode_ok_lenient_utf8: Attempt[BitVector]   = lenientUtf8.encode(ok)

  @Benchmark def encode_long_string_baseline_charset: ByteBuffer       = UTF_8.encode(longString)
  @Benchmark def encode_long_string_baseline_string: scala.Array[Byte] = longString.getBytes(UTF_8)
  @Benchmark def encode_long_string_default_utf8: Attempt[BitVector]   = defaultUtf8.encode(longString)
  @Benchmark def encode_long_string_lenient_utf8: Attempt[BitVector]   = lenientUtf8.encode(longString)
}
