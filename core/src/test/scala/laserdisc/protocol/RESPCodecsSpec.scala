package laserdisc.protocol

import java.nio.charset.StandardCharsets.UTF_8

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{chooseNum, listOfN, option, oneOf => genOneOf}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{EitherValues, MustMatchers, WordSpec}
import scodec.bits.BitVector
import scodec.{Codec, Err}

object RESPCodecsSpec extends EitherValues {

  private final case class InvalidDiscriminator(c: Char) {
    val toHex: String             = BitVector.fromByte(c.toByte).toHex
    override val toString: String = s"$c"
  }

  private final object functions {
    private final val attemptDecode = (bits: BitVector) => Codec[RESP].decodeValue(bits)
    private final val requireEncode = (resp: RESP) => Codec[RESP].encode(resp).require
    private final val stringToBytes = (s: String) => s.getBytes(UTF_8)

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
    def RESP: Either[Err, RESP] = functions.stringToRESPAttempt(underlying).toEither
    def asRESP: RESP            = RESP.right.value
    def bytesLength: Int        = functions.stringToBytesLength(underlying)
  }

  implicit final class RichRESP(private val underlying: RESP) extends AnyVal {
    def wireFormat: String = functions.respToString(underlying)
    def roundTrip: RESP    = functions.roundTripAttempt(underlying).require
  }

  implicit final class RichSeqRESP(private val underlying: Seq[RESP]) extends AnyVal {
    def wireFormat: String = functions.respSeqToString(underlying)
  }
}

final class RESPCodecsSpec extends WordSpec with MustMatchers with PropertyChecks with EitherValues {
  import RESPCodecsSpec._
  import RESP._

  private val smallListSize = chooseNum(0, 20)
  private implicit def invalidProtocolDiscriminator: Gen[InvalidDiscriminator] = {
    val exclusions = List('+', '-', ':', '$', '*')
    Gen.choose[Char](0, 127).suchThat(!exclusions.contains(_)).map(InvalidDiscriminator)
  }
  private implicit val _                                  = Gen.choose(Char.MinValue, Char.MaxValue).filter(Character.isDefined)
  private implicit val genSimpleString: Gen[SimpleString] = arbitrary[String].map(str)
  private implicit val genError: Gen[Error]               = arbitrary[String].map(err)
  private implicit val genInteger: Gen[Integer]           = arbitrary[Long].map(int)
  private implicit val genBulkString: Gen[BulkString] = arbitrary[Option[String]].map {
    case None    => NullBulkString
    case Some(s) => bulk(s)
  }
  private implicit def genArray: Gen[Array] = smallListSize.flatMap { size =>
    option(listOfN(size, genRESP)).map {
      case None    => NilArray
      case Some(v) => arr(v)
    }
  }
  private implicit def genRESP: Gen[RESP] =
    genOneOf(genSimpleString, genError, genInteger, genBulkString, genArray)
  private implicit def genOptionListRESP: Gen[Option[List[RESP]]] = smallListSize.flatMap { size =>
    option(listOfN(size, genRESP))
  }

  private implicit def arb[A](implicit A: Gen[A]): Arbitrary[A] = Arbitrary(A)

  "A RESP codec" when {

    "handling unknown protocol type" must {
      "fail with correct error message" in forAll { invalidDiscriminator: InvalidDiscriminator =>
        s"$invalidDiscriminator".RESP.left.value.messageWithContext mustBe s"unidentified RESP type ${invalidDiscriminator.toHex}"
      }
    }

    "handling simple strings" must {
      "decode them correctly" in forAll { s: String =>
        s"+$s\r\n".asRESP mustBe str(s)
      }
      "encode them correctly" in forAll { simpleString: SimpleString =>
        simpleString.wireFormat mustBe s"+${simpleString.value}\r\n"
      }
      "round-trip with no errors" in forAll { simpleString: SimpleString =>
        simpleString.roundTrip mustBe simpleString
      }
    }

    "handling errors" must {
      "decode them correctly" in forAll { msg: String =>
        s"-$msg\r\n".asRESP mustBe err(msg)
      }
      "encode them correctly" in forAll { error: Error =>
        error.wireFormat mustBe s"-${error.message}\r\n"
      }
      "round-trip with no errors" in forAll { error: Error =>
        error.roundTrip mustBe error
      }
    }

    "handling integers" must {
      "decode them correctly" in forAll { l: Long =>
        s":$l\r\n".asRESP mustBe int(l)
      }
      "encode them correctly" in forAll { integer: Integer =>
        integer.wireFormat mustBe s":${integer.value}\r\n"
      }
      "round-trip with no errors" in forAll { integer: Integer =>
        integer.roundTrip mustBe integer
      }
    }

    "handling bulk strings" must {
      "fail with correct error message when decoding size < -1" in {
        "$-2\r\nbla\r\n".RESP.left.value.messageWithContext mustBe "size: failed to decode bulk-string of size -2"
      }
      "decode them correctly" in forAll { maybeString: Option[String] =>
        maybeString match {
          case None    => "$-1\r\n".asRESP mustBe nullBulk
          case Some(s) => s"$$${s.bytesLength}\r\n$s\r\n".asRESP mustBe bulk(s)
        }
      }
      "encode them correctly" in forAll { bulkString: BulkString =>
        bulkString -> bulkString.wireFormat match {
          case (NullBulkString, s)        => s mustBe "$-1\r\n"
          case (NonNullBulkString(bs), s) => s mustBe s"$$${bs.bytesLength}\r\n$bs\r\n"
        }
      }
      "round-trip with no errors" in forAll { bulkString: BulkString =>
        bulkString.roundTrip mustBe bulkString
      }
    }

    "handling arrays" must {
      "fail with correct error message when decoding size < -1" in {
        "*-2\r\nbla\r\n".RESP.left.value.messageWithContext mustBe "size: failed to decode array of size -2"
      }
      "decode them correctly" in forAll { maybeRESPList: Option[List[RESP]] =>
        maybeRESPList match {
          case None     => "*-1\r\n".asRESP mustBe nilArray
          case Some(xs) => s"*${xs.length}\r\n${xs.wireFormat}".asRESP mustBe arr(xs)
        }
      }
      "encode them correctly" in forAll { array: Array =>
        array -> array.wireFormat match {
          case (NilArray, s)        => s mustBe "*-1\r\n"
          case (NonNilArray(xs), s) => s mustBe s"*${xs.length}\r\n${xs.wireFormat}"
        }
      }
      "round-trip with no errors" in forAll { array: Array =>
        array.roundTrip mustBe array
      }
    }
  }
}
