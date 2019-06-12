package laserdisc
package protocol

import java.nio.charset.StandardCharsets.UTF_8

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{chooseNum, listOfN, option, oneOf => genOneOf}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.EitherValues
import scodec.bits.BitVector
import scodec.{Codec, Err => SErr}

object RESPCodecsSpec extends EitherValues {

  private final case class InvalidDiscriminator(c: Char) {
    val toHex: String             = BitVector.fromByte(c.toByte).toHex
    override val toString: String = s"$c"
  }

  private[this] final object functions {
    private[this] final val attemptDecode = (bits: BitVector) => Codec[RESP].decodeValue(bits)
    private[this] final val requireEncode = (resp: RESP) => Codec[RESP].encode(resp).require
    private[this] final val stringToBytes = (s: String) => s.getBytes(UTF_8)

    final val stringToBytesLength = stringToBytes andThen (bytes => bytes.length)

    final val stringToRESPAttempt = {
      val bitVectorFromString = (s: String) => BitVector(stringToBytes(s))
      bitVectorFromString andThen attemptDecode
    }

    final val respToString = {
      val stringify = (bits: BitVector) => bits.bytes.decodeUtf8.right.value
      requireEncode andThen stringify
    }

    final val respSeqToString = (xs: Seq[RESP]) => xs.map(respToString).mkString

    final val roundTripAttempt = requireEncode andThen attemptDecode
  }

  implicit final class RichString(private val underlying: String) extends AnyVal {
    def RESP: SErr | RESP = functions.stringToRESPAttempt(underlying).toEither
    def asRESP: RESP      = RESP.right.value
    def bytesLength: Int  = functions.stringToBytesLength(underlying)
  }

  implicit final class RichRESP(private val underlying: RESP) extends AnyVal {
    def wireFormat: String = functions.respToString(underlying)
    def roundTrip: RESP    = functions.roundTripAttempt(underlying).require
  }

  implicit final class RichSeqRESP(private val underlying: Seq[RESP]) extends AnyVal {
    def wireFormat: String = functions.respSeqToString(underlying)
  }
}

final class RESPCodecsSpec extends BaseSpec {
  import RESP._
  import RESPCodecsSpec._

  private[this] val smallListSize = chooseNum(0, 20)
  private[this] implicit def invalidProtocolDiscriminator: Gen[InvalidDiscriminator] = {
    val exclusions = List('+', '-', ':', '$', '*')
    Gen.choose[Char](0, 127).suchThat(!exclusions.contains(_)).map(InvalidDiscriminator)
  }
  private[this] implicit val _                = Gen.choose(Char.MinValue, Char.MaxValue).filter(Character.isDefined)
  private[this] implicit val genStr: Gen[Str] = arbitrary[String].map(str)
  private[this] implicit val genErr: Gen[Err] = arbitrary[String].map(err)
  private[this] implicit val genNum: Gen[Num] = arbitrary[Long].map(num)
  private[this] implicit val genBulk: Gen[GenBulk] = arbitrary[Option[String]].map {
    case None    => NullBulk
    case Some(s) => bulk(s)
  }
  private[this] implicit def genArr: Gen[GenArr] = smallListSize.flatMap { size =>
    option(listOfN(size, genRESP)).map {
      case None    => NilArr
      case Some(v) => arr(v)
    }
  }
  private[this] implicit def genRESP: Gen[RESP] =
    genOneOf(genStr, genErr, genNum, genBulk, genArr)
  private[this] implicit def genOptionListRESP: Gen[Option[List[RESP]]] = smallListSize.flatMap { size =>
    option(listOfN(size, genRESP))
  }

  private[this] implicit def arb[A](implicit A: Gen[A]): Arbitrary[A] = Arbitrary(A)

  "A RESP codec" when {

    "handling unknown protocol type" should {
      "fail with correct error message" in forAll { invalidDiscriminator: InvalidDiscriminator =>
        s"$invalidDiscriminator".RESP.left.value.messageWithContext shouldBe s"unidentified RESP type (Hex: ${invalidDiscriminator.toHex})"
      }
    }

    "handling simple strings" should {
      "decode them correctly" in forAll { s: String =>
        s"+$s\r\n".asRESP shouldBe str(s)
      }
      "encode them correctly" in forAll { str: Str =>
        str.wireFormat shouldBe s"+${str.value}\r\n"
      }
      "round-trip with no errors" in forAll { str: Str =>
        str.roundTrip shouldBe str
      }
    }

    "handling errors" should {
      "decode them correctly" in forAll { msg: String =>
        s"-$msg\r\n".asRESP shouldBe err(msg)
      }
      "encode them correctly" in forAll { err: Err =>
        err.wireFormat shouldBe s"-${err.message}\r\n"
      }
      "round-trip with no errors" in forAll { err: Err =>
        err.roundTrip shouldBe err
      }
    }

    "handling integers" should {
      "decode them correctly" in forAll { l: Long =>
        s":$l\r\n".asRESP shouldBe num(l)
      }
      "encode them correctly" in forAll { num: Num =>
        num.wireFormat shouldBe s":${num.value}\r\n"
      }
      "round-trip with no errors" in forAll { num: Num =>
        num.roundTrip shouldBe num
      }
    }

    "handling bulk strings" should {
      "fail with correct error message when decoding size < -1" in {
        "$-2\r\nbla\r\n".RESP.left.value.messageWithContext shouldBe "size: failed to decode bulk-string of size -2"
      }
      "decode them correctly" in forAll { maybeString: Option[String] =>
        maybeString match {
          case None    => "$-1\r\n".asRESP shouldBe nullBulk
          case Some(s) => s"$$${s.bytesLength}\r\n$s\r\n".asRESP shouldBe bulk(s)
        }
      }
      "encode them correctly" in forAll { bulk: GenBulk =>
        bulk -> bulk.wireFormat match {
          case (NullBulk, s) => s shouldBe "$-1\r\n"
          case (Bulk(bs), s) => s shouldBe s"$$${bs.bytesLength}\r\n$bs\r\n"
        }
      }
      "round-trip with no errors" in forAll { bulk: GenBulk =>
        bulk.roundTrip shouldBe bulk
      }
    }

    "handling arrays" should {
      "fail with correct error message when decoding size < -1" in {
        "*-2\r\nbla\r\n".RESP.left.value.messageWithContext shouldBe "size: failed to decode array of size -2"
      }
      "decode them correctly" in forAll { maybeRESPList: Option[List[RESP]] =>
        maybeRESPList match {
          case None     => "*-1\r\n".asRESP shouldBe nilArr
          case Some(xs) => s"*${xs.length}\r\n${xs.wireFormat}".asRESP shouldBe arr(xs)
        }
      }
      "encode them correctly" in forAll { arr: GenArr =>
        arr -> arr.wireFormat match {
          case (NilArr, s)  => s shouldBe "*-1\r\n"
          case (Arr(xs), s) => s shouldBe s"*${xs.length}\r\n${xs.wireFormat}"
        }
      }
      "round-trip with no errors" in forAll { arr: GenArr =>
        arr.roundTrip shouldBe arr
      }
    }
  }
}
