package laserdisc
package protocol

import java.nio.charset.StandardCharsets.UTF_8

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import scodec.bits.BitVector
import scodec.{Codec, Err => SErr}

object RESPCodecsSpec {
  private[this] final object functions {
    private[this] final val attemptDecode = (bits: BitVector) => Codec[RESP].decodeValue(bits)
    private[this] final val requireEncode = (resp: RESP) => Codec[RESP].encode(resp).require
    private[this] final val stringToBytes = (s: String) => s.getBytes(UTF_8)

    final val stringToBytesLength = stringToBytes andThen (bytes => bytes.length)

    final val stringToRESPAttempt = {
      val bitVectorFromString = (s: String) => BitVector(stringToBytes(s))
      bitVectorFromString andThen attemptDecode
    }

    val respToString: RESP => String = {
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

  private[this] implicit val stringArb: Arbitrary[String]        = Arbitrary(stringGen)
  private[this] implicit val invalidProtocolArb: Arbitrary[Char] = Arbitrary(invalidProtocolGen)
  private[this] implicit val strArb: Arbitrary[Str]              = Arbitrary(strGen)
  private[this] implicit val errArb: Arbitrary[Err]              = Arbitrary(errGen)
  private[this] implicit val numArb: Arbitrary[Num]              = Arbitrary(numGen)
  private[this] implicit val genBulkArb: Arbitrary[GenBulk]      = Arbitrary(genBulkGen)
  private[this] implicit val genArrArb: Arbitrary[GenArr]        = Arbitrary(genArrGen)
  private[this] implicit val respListArb: Arbitrary[List[RESP]]  = Arbitrary(respListGen)

  property("A RESP codec handling unknown protocol type fails with correct error message") {
    forAll { c: Char =>
      assertLeftEquals(
        s"$c".RESP leftMap (_.messageWithContext),
        s"unidentified RESP type (Hex: ${c.toHex})"
      )
    }
  }

  property("A RESP codec handling simple strings decodes them correctly") {
    forAll { s: String => assertEquals(s"+$s$CRLF".RESP, Str(s)) }
  }

  property("A RESP codec handling simple strings decodes them correctly") {
    forAll { s: Str => assertEquals(s.wireFormat, s"+${s.value}$CRLF") }
  }

  property("A RESP codec handling simple strings roundtrips with no errors") {
    forAll { s: Str => assertEquals(s.roundTrip, s) }
  }

  property("A RESP codec handling errors decodes them correctly") {
    forAll { s: String => assertEquals(s"-$s$CRLF".RESP, Err(s)) }
  }

  property("A RESP codec handling errors encodes them correctly") {
    forAll { e: Err => assertEquals(e.wireFormat, s"-${e.message}$CRLF") }
  }

  property("A RESP codec handling errors roundtrips with no errors") {
    forAll { e: Err => assertEquals(e.roundTrip, e) }
  }

  property("A RESP codec handling integers decodes them correctly") {
    forAll { l: Long => assertEquals(s":$l$CRLF".RESP, Num(l)) }
  }

  property("A RESP codec handling integers encodes them correctly") {
    forAll { n: Num => assertEquals(n.wireFormat, s":${n.value}$CRLF") }
  }

  property("A RESP codec handling integers roundtrips with no errors") {
    forAll { n: Num => assertEquals(n.roundTrip, n) }
  }

  property("A RESP codec handling bulk strings fails with correct error message when decoding size < -1") {
    assertLeftEquals(
      s"$$-2${CRLF}bla$CRLF".RESP leftMap (_.messageWithContext),
      "size: failed to decode bulk-string of size -2"
    )
  }

  property("A RESP codec handling bulk strings decodes them correctly") {
    forAll { os: Option[String] =>
      os match {
        case None    => assertEquals(s"$$-1$CRLF".RESP, NullBulk)
        case Some(s) => assertEquals(s"$$${s.bytesLength}$CRLF$s$CRLF".RESP, Bulk(s))
      }
    }
  }

  property("A RESP codec handling bulk strings encodes them correctly") {
    forAll { b: GenBulk =>
      b match {
        case NullBulk => assertEquals(b.wireFormat, s"$$-1$CRLF")
        case Bulk(bs) => assertEquals(b.wireFormat, s"$$${bs.bytesLength}$CRLF$bs$CRLF")
      }
    }
  }

  property("A RESP codec handling bulk strings roundtrips with no errors") {
    forAll { b: GenBulk => assertEquals(b.roundTrip, b) }
  }

  property("A RESP codec handling arrays fails with correct error message when decoding size < -1") {
    assertLeftEquals(
      s"*-2${CRLF}bla$CRLF".RESP leftMap (_.messageWithContext),
      "size: failed to decode array of size -2"
    )
  }

  property("A RESP codec handling bulk strings decodes them correctly") {
    forAll { ors: Option[List[RESP]] =>
      ors match {
        case None     => assertEquals(s"*-1$CRLF".RESP, NilArr)
        case Some(xs) => assertEquals(s"*${xs.length}$CRLF${xs.wireFormat}".RESP, Arr(xs))
      }
    }
  }

  property("A RESP codec handling bulk strings encodes them correctly") {
    forAll { a: GenArr =>
      a match {
        case NilArr  => assertEquals(a.wireFormat, s"*-1$CRLF")
        case Arr(xs) => assertEquals(a.wireFormat, s"*${xs.length}$CRLF${xs.wireFormat}")
      }
    }
  }

  property("A RESP codec handling bulk strings roundtrips with no errors") {
    forAll { a: GenArr => assertEquals(a.roundTrip, a) }
  }
}
