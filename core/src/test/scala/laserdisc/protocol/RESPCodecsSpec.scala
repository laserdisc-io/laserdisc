package laserdisc
package protocol

import java.nio.charset.StandardCharsets.UTF_8

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalatest.EitherValues
import scodec.bits.BitVector
import scodec.{Codec, Err => SErr}

object RESPCodecsSpec extends EitherValues {
  private[this] final object functions {
    private[this] final val attemptDecode = (bits: BitVector) => Codec[RESP].decodeValue(bits)
    private[this] final val requireEncode = (resp: RESP) => Codec[RESP].encode(resp).require
    private[this] final val stringToBytes = (s: String) => s.getBytes(UTF_8)

    final val stringToBytesLength = stringToBytes andThen (bytes => bytes.length)

    final val stringToRESPAttempt = {
      val bitVectorFromString = (s: String) => BitVector(stringToBytes(s))
      bitVectorFromString andThen attemptDecode
    }

    final val respToString: RESP => String = {
      val stringify = (bits: BitVector) => bits.bytes.decodeUtf8.fold(_.getMessage, identity)
      requireEncode andThen stringify
    }

    final val respSeqToString = (xs: Seq[RESP]) => xs.map(respToString).mkString

    final val roundTripAttempt = requireEncode andThen attemptDecode
  }

  private implicit final class RichChar(private val underlying: Char) extends AnyVal {
    def toHex: String = BitVector.fromByte(underlying.toByte).toHex
  }

  private implicit final class RichString(private val underlying: String) extends AnyVal {
    def RESP: SErr | RESP = functions.stringToRESPAttempt(underlying).toEither
    def bytesLength: Int  = functions.stringToBytesLength(underlying)
  }

  private implicit final class RichRESP(private val underlying: RESP) extends AnyVal {
    def wireFormat: String = functions.respToString(underlying)
    def roundTrip: RESP    = functions.roundTripAttempt(underlying).require
  }

  private implicit final class RichSeqRESP(private val underlying: Seq[RESP]) extends AnyVal {
    def wireFormat: String = functions.respSeqToString(underlying)
  }
}

final class RESPCodecsSpec extends BaseSpec {
  import RESPCodecsSpec._

  private[this] val smallNumGen: Gen[Int] = chooseNum(0, 20)
  private[this] val invalidProtocolGen: Gen[Char] = {
    val exclusions = List('+', '-', ':', '$', '*')
    choose[Char](0, 127).suchThat(!exclusions.contains(_))
  } :| "invalid protocol discriminator"
  private[this] val stringGen: Gen[String] = listOf(utf8BMPCharGen).map(_.mkString) :| "string"
  private[this] val strGen: Gen[Str]       = stringGen.map(Str.apply) :| "simple string RESP"
  private[this] val errGen: Gen[Err]       = stringGen.map(Err.apply) :| "error RESP"
  private[this] val numGen: Gen[Num]       = arbitrary[Long].map(Num.apply) :| "integer RESP"
  private[this] val genBulkGen: Gen[GenBulk] = option(stringGen).map {
    case None    => NullBulk
    case Some(s) => Bulk(s)
  } :| "bulk string RESP"
  private[this] def genArrGen: Gen[GenArr] =
    smallNumGen.flatMap { size =>
      option(listOfN(size, respGen)).map {
        case None    => NilArr
        case Some(v) => Arr(v)
      }
    } :| "array RESP"
  private[this] def respGen: Gen[RESP] =
    lzy {
      frequency(2 -> strGen, 1 -> errGen, 2 -> numGen, 4 -> genBulkGen, 1 -> genArrGen)
    } :| "RESP"
  private[this] def respListGen: Gen[List[RESP]] = smallNumGen.flatMap(listOfN(_, respGen)) :| "list of RESPs"

  private[this] implicit final val stringArb: Arbitrary[String]        = Arbitrary(stringGen)
  private[this] implicit final val invalidProtocolArb: Arbitrary[Char] = Arbitrary(invalidProtocolGen)
  private[this] implicit final val strArb: Arbitrary[Str]              = Arbitrary(strGen)
  private[this] implicit final val errArb: Arbitrary[Err]              = Arbitrary(errGen)
  private[this] implicit final val numArb: Arbitrary[Num]              = Arbitrary(numGen)
  private[this] implicit final val genBulkArb: Arbitrary[GenBulk]      = Arbitrary(genBulkGen)
  private[this] implicit final val genArrArb: Arbitrary[GenArr]        = Arbitrary(genArrGen)
  private[this] implicit final val respListArb: Arbitrary[List[RESP]]  = Arbitrary(respListGen)

  "A RESP codec" when {
    "handling unknown protocol type" should {
      "fail with correct error message" in forAll { c: Char =>
        s"$c".RESP.left.value.messageWithContext shouldBe s"unidentified RESP type (Hex: ${c.toHex})"
      }
    }

    "handling simple strings" should {
      "decode them correctly" in forAll { s: String =>
        s"+$s$CRLF".RESP onRight (_ shouldBe Str(s))
      }
      "encode them correctly" in forAll { s: Str =>
        s.wireFormat shouldBe s"+${s.value}$CRLF"
      }
      "roundtrip with no errors" in forAll { s: Str =>
        s.roundTrip shouldBe s
      }
    }

    "handling errors" should {
      "decode them correctly" in forAll { s: String =>
        s"-$s$CRLF".RESP onRight (_ shouldBe Err(s))
      }
      "encode them correctly" in forAll { e: Err =>
        e.wireFormat shouldBe s"-${e.message}$CRLF"
      }
      "roundtrip with no errors" in forAll { e: Err =>
        e.roundTrip shouldBe e
      }
    }

    "handling integers" should {
      "decode them correctly" in forAll { l: Long =>
        s":$l$CRLF".RESP onRight (_ shouldBe Num(l))
      }
      "encode them correctly" in forAll { n: Num =>
        n.wireFormat shouldBe s":${n.value}$CRLF"
      }
      "roundtrip with no errors" in forAll { n: Num =>
        n.roundTrip shouldBe n
      }
    }

    "handling bulk strings" should {
      "fail with correct error message when decoding size < -1" in {
        s"$$-2${CRLF}bla$CRLF".RESP.left.value.messageWithContext shouldBe "size: failed to decode bulk-string of size -2"
      }
      "decode them correctly" in forAll { os: Option[String] =>
        os match {
          case None    => s"$$-1$CRLF".RESP onRight (_ shouldBe NullBulk)
          case Some(s) => s"$$${s.bytesLength}$CRLF$s$CRLF".RESP onRight (_ shouldBe Bulk(s))
        }
      }
      "encode them correctly" in forAll { b: GenBulk =>
        b match {
          case NullBulk => b.wireFormat shouldBe s"$$-1$CRLF"
          case Bulk(bs) => b.wireFormat shouldBe s"$$${bs.bytesLength}$CRLF$bs$CRLF"
        }
      }
      "roundtrip with no errors" in forAll { b: GenBulk =>
        b.roundTrip shouldBe b
      }
    }

    "handling arrays" should {
      "fail with correct error message when decoding size < -1" in {
        s"*-2${CRLF}bla$CRLF".RESP.left.value.messageWithContext shouldBe "size: failed to decode array of size -2"
      }
      "decode them correctly" in forAll { ors: Option[List[RESP]] =>
        ors match {
          case None     => s"*-1$CRLF".RESP onRight (_ shouldBe NilArr)
          case Some(xs) => s"*${xs.length}$CRLF${xs.wireFormat}".RESP onRight (_ shouldBe Arr(xs))
        }
      }
      "encode them correctly" in forAll { a: GenArr =>
        a match {
          case NilArr  => a.wireFormat shouldBe s"*-1$CRLF"
          case Arr(xs) => a.wireFormat shouldBe s"*${xs.length}$CRLF${xs.wireFormat}"
        }
      }
      "roundtrip with no errors" in forAll { a: GenArr =>
        a.roundTrip shouldBe a
      }
    }
  }
}
