package laserdisc
package protocol

import java.nio.charset.StandardCharsets.UTF_8
import java.{lang => j}

import scodec.Decoder.decodeCollect
import scodec.Encoder.encodeSeq
import scodec.Err.{General, MatchingDiscriminatorNotFound}
import scodec.bits.{BitVector, _}
import scodec.codecs.{filtered, fixedSizeBytes}
import scodec.{Attempt, Codec, DecodeResult, Decoder, SizeBound, Err => SErr}

import scala.annotation.tailrec
import shapeless.Generic

/** [[https://redis.io/topics/protocol Redis Protocol Specification]]
  *
  * This sealed trait represents the entire Redis Serialization Protocol algebra
  *
  * Concrete instances of this trait must be created using this trait's companion
  * object's methods, were [[scodec.Codec]]s for each are also defined
  *
  * @see [[RESPBuilders]]
  * @see [[RESPCodecs]]
  */
sealed trait RESP extends AnyRef with Serializable

/**
  * RESP [[https://redis.io/topics/protocol#resp-simple-strings Simple Strings]]
  *
  * @note Sometimes the value "OK" is used to represent a successful
  * acknowledgement/processing of a command.
  *
  * @example
  * {{{
  *   val s: Str = Str("some string")
  * }}}
  *
  * @param value The wrapped string value
  */
final case class Str(value: String) extends RESP
object Str {
  final def apply[A](a: A)(implicit A: Show[A]): Str = new Str(A.show(a))
}

/**
  * RESP [[https://redis.io/topics/protocol#resp-errors Errors]]
  *
  * RESP [[Err]]s are also [[scala.RuntimeException]]s, although
  * __where possible__ they will not contain stacktrace data
  *
  * @example
  * {{{
  *   val e: Err = Err("some error message")
  * }}}
  * @param message The wrapped exception's message
  */
final case class Err(message: String) extends laserdisc.Platform.LaserDiscRuntimeError(message) with RESP

/**
  * RESP [[https://redis.io/topics/protocol#resp-integers Integers]]
  *
  * @note Sometimes the values 0 and 1 are used to represent boolean
  * values. In this case 0 corresponds to False while 1 to True,
  * respectively.
  *
  * @example
  * {{{
  *   val n: Num = Num(42)
  * }}}
  *
  * @param value The wrapped long value
  */
final case class Num(value: Long) extends RESP

/**
  * RESP [[https://redis.io/topics/protocol#resp-bulk-strings Bulk Strings]]
  *
  * There can be 2 cases:
  *  - `null` bulk strings, where the length is -1 and no actual underlying string is present
  *  - actual (non-null) bulk strings, where the length is >= 0
  *
  * @example
  * {{{
  *   val b: Bulk      = Bulk("some string")
  *   val nb: NullBulk = NullBulk
  * }}}
  * @see [[Show]]
  */
sealed trait GenBulk extends RESP
case object NullBulk extends GenBulk

/**
  * This is the special case of a non-null RESP [[GenBulk]]
  *
  * These can be constructed by using the [[RESPBuilders#bulk]]
  * method
  *
  * @param value The wrapped bulk string value
  */
final case class Bulk(value: String) extends GenBulk
object Bulk {
  final def apply[A](a: A)(implicit A: Show[A]): Bulk = new Bulk(A.show(a))

  implicit final val bulkShow: Show[Bulk] = Show.instance(_.value)
}

/**
  * RESP [[https://redis.io/topics/protocol#resp-arrays Arrays]]
  *
  * There can be 2 cases:
  *  - `nil` arrays, where the length is -1 and no array element is present
  *  - actual (non-nil) arrays, where the length is >= 0
  *
  * @note [[[Arr#apply(one:laserdisc\.protocol\.RESP,rest:laserdisc\.protocol\.RESP*)* Arr#apply(one: RESP, rest: RESP*)]]]
  * is an overload which supports the creation of guaranteed non-empty
  * sequences only. This is achieved through the usage of one fixed
  * parameter followed by a var-arg of the same
  * @example
  * {{{
  *   val arr: Arr                = Arr(List(Str("hello"), Str("world")))
  *   val guaranteedNonEmpty: Arr = Arr(Str("hello"), Str("world"))
  *   val empty: Arr              = Arr(List.empty)
  *   val nil: NilArr             = NilArr
  * }}}
  */
sealed trait GenArr extends RESP
case object NilArr  extends GenArr

/**
  * This is the special case of a non-nil RESP [[GenArr]]
  *
  * These can be constructed either by the default case class' apply by resorting to the overloaded
  * [[[Arr#apply(one:laserdisc\.protocol\.RESP,rest:laserdisc\.protocol\.RESP*)* Arr#apply(one: RESP, rest: RESP*)]]]
  * method which expects one parameter to be supplied followed
  * by a (possibly empty) sequence of [[RESP]]s (vararg).
  *
  * @param elements The wrapped array values, as a [[scala.List]] of [[RESP]]
  */
final case class Arr(elements: List[RESP]) extends GenArr {
  override def toString: String = s"Arr(${elements.mkString(COMMA)})"
}
object Arr {
  final def apply(one: RESP, rest: RESP*): Arr = new Arr(one +: rest.toList)
}

private[protocol] final case class Repr[A](decoded: A, bits: BitVector)

sealed trait RESPCodecs extends BitVectorSyntax {
  protected final val utf8Codec         = new LenientStringCodec(UTF_8)
  protected final val BitsInByte        = 8L
  protected final val plus              = hex"2b".bits
  protected final val minus             = hex"2d".bits
  protected final val colon             = hex"3a".bits
  protected final val dollar            = hex"24".bits
  protected final val star              = hex"2a".bits
  protected final val crlf              = hex"0d0a".bits
  protected final val minusOne          = hex"2d31".bits
  protected final val zero              = hex"30".bits
  private[this] final val crlfSize      = crlf.size
  protected final val crlfBytes         = crlf.bytes
  private[this] final val crlfBytesSize = crlfBytes.size

  private[this] final def crlfTerminatedCodec[A](baseCodec: Codec[A], from: Long = 0L): Codec[A] = filtered(
    baseCodec,
    new Codec[BitVector] {
      override final def sizeBound: SizeBound                        = SizeBound.unknown
      override final def encode(bits: BitVector): Attempt[BitVector] = Attempt.successful(bits ++ crlf)
      override final def decode(bits: BitVector): Attempt[DecodeResult[BitVector]] = bits.bytes.indexOfSlice(crlfBytes, from) match {
        case -1 =>
          Attempt.failure(new MatchingDiscriminatorNotFound(s"Does not contain 'CRLF' termination bytes. Content: ${bits.tailToUtf8}"))
        case i => Attempt.successful(DecodeResult(bits.take(i * BitsInByte), bits.drop(i * BitsInByte + crlfSize)))
      }
    }
  )

  private[this] final val crlfTerminatedStringCodec: Codec[String] = crlfTerminatedCodec(utf8Codec)
  private[this] final val crlfTerminatedLongCodec: Codec[Long] = crlfTerminatedStringCodec.narrow(
    s =>
      try Attempt.successful(j.Long.parseLong(s))
      catch { case _: NumberFormatException => Attempt.failure(SErr(s"Expected long but found $s")) },
    _.toString
  )
  private[this] final val strCodec: Codec[Str] = crlfTerminatedStringCodec.xmap[Str](Str.apply, _.value)
  private[this] final val errCodec: Codec[Err] = crlfTerminatedStringCodec.xmap[Err](Err.apply, _.message)
  private[this] final val numCodec: Codec[Num] = crlfTerminatedLongCodec.xmap[Num](Num.apply, _.value)
  private[this] final val bulkCodec: Codec[GenBulk] = new Codec[GenBulk] {
    private[this] final val nullBulkBits = minusOne ++ crlf
    private[this] final val decoder = crlfTerminatedLongCodec.flatMap {
      case -1                => Decoder.point(NullBulk)
      case size if size >= 0 => fixedSizeBytes(size + crlfBytesSize, crlfTerminatedCodec(utf8Codec, size)).map(Bulk.apply)
      case negSize           => Decoder.liftAttempt(Attempt.failure(failDec(negSize)))
    }
    private[this] final def failDec(negSize: Long) = General(s"failed to decode bulk-string of size $negSize", List("size"))
    private[this] final def failEnc(bulk: GenBulk, err: SErr) =
      General(s"failed to encode size of [$bulk]: ${err.messageWithContext}", List("size"))

    override final def sizeBound: SizeBound = SizeBound.unknown
    override final def encode(bulk: GenBulk): Attempt[BitVector] = bulk match {
      case NullBulk => Attempt.successful(nullBulkBits)
      case Bulk(s) =>
        crlfTerminatedStringCodec.encode(s).flatMap { bits =>
          crlfTerminatedLongCodec.encode(bits.size / BitsInByte - crlfBytesSize).mapErr(failEnc(bulk, _)).map(_ ++ bits)
        }
    }
    override final def decode(buffer: BitVector): Attempt[DecodeResult[GenBulk]] = decoder.decode(buffer)
  }
  private[this] final val arrCodec: Codec[GenArr] = new Codec[GenArr] {
    private[this] final val nilArrBits   = minusOne ++ crlf
    private[this] final val emptyArrBits = zero ++ crlf
    private[this] final def checkSize(v: List[RESP], expectedSize: Long): Attempt[List[RESP]] =
      if (v.size == expectedSize) Attempt.successful(v)
      else Attempt.failure(SErr(s"Insufficient number of elements: decoded ${v.size} instead of $expectedSize"))
    private[this] final val decoder = crlfTerminatedLongCodec.flatMap {
      case -1 => Decoder.point(NilArr)
      case 0  => Decoder.point(Arr(List.empty))
      case size if size > 0 =>
        Decoder(decodeCollect[List, RESP](respCodec, Some(size.toInt))(_)).narrow[List[RESP]](checkSize(_, size), identity).map(Arr.apply)
      case negSize => Decoder.liftAttempt(Attempt.failure(failDec(negSize)))
    }
    private[this] final def failDec(negSize: Long) = General(s"failed to decode array of size $negSize", List("size"))
    private[this] final def failEnc(arr: GenArr, err: SErr) =
      General(s"failed to encode size of [$arr]: ${err.messageWithContext}", List("size"))

    override final def sizeBound: SizeBound = SizeBound.unknown
    override final def encode(arr: GenArr): Attempt[BitVector] = arr match {
      case NilArr              => Attempt.successful(nilArrBits)
      case Arr(v) if v.isEmpty => Attempt.successful(emptyArrBits)
      case Arr(v) =>
        crlfTerminatedLongCodec.encode(v.size.toLong).mapErr(failEnc(arr, _)).flatMap(size => encodeSeq(respCodec)(v).map(size ++ _))
    }
    override final def decode(bits: BitVector): Attempt[DecodeResult[GenArr]] = decoder.decode(bits)
  }

  implicit final val respCodec: Codec[RESP] = new Codec[RESP] {
    override final def sizeBound: SizeBound = SizeBound.unknown
    override final def encode(value: RESP): Attempt[BitVector] = value match {
      case str: Str      => strCodec.encode(str).map(plus ++ _)
      case err: Err      => errCodec.encode(err).map(minus ++ _)
      case num: Num      => numCodec.encode(num).map(colon ++ _)
      case bulk: GenBulk => bulkCodec.encode(bulk).map(dollar ++ _)
      case arr: GenArr   => arrCodec.encode(arr).map(star ++ _)
    }
    override final def decode(bits: BitVector): Attempt[DecodeResult[RESP]] =
      bits.consumeThen(BitsInByte)(
        s => Attempt.failure(SErr(s)), {
          case (`plus`, remainder)   => strCodec.decode(remainder)
          case (`minus`, remainder)  => errCodec.decode(remainder)
          case (`colon`, remainder)  => numCodec.decode(remainder)
          case (`dollar`, remainder) => bulkCodec.decode(remainder)
          case (`star`, remainder)   => arrCodec.decode(remainder)
          case (other, _)            => Attempt.failure(SErr(s"unidentified RESP type (Hex: ${other.toHex})"))
        }
      )
    override final def toString: String = "RESP"
  }

  protected final val crlfTerminatedReprOfLongDecoder: Decoder[Repr[Long]] = crlfTerminatedCodec(
    new Codec[Repr[String]] {
      override final def sizeBound: SizeBound                                         = SizeBound.unknown
      override final def encode(bd: Repr[String]): Attempt[BitVector]                 = utf8Codec.encode(bd.decoded)
      override final def decode(bits: BitVector): Attempt[DecodeResult[Repr[String]]] = utf8Codec.decode(bits).map(_.map(Repr(_, bits)))
    }
  ).asDecoder.emap { sRepr =>
    try Attempt.successful(Repr(j.Long.parseLong(sRepr.decoded), sRepr.bits))
    catch { case _: NumberFormatException => Attempt.failure(SErr(s"Expected long but found ${sRepr.decoded}")) }
  }
}

final case class RESPDecErr(message: String) extends laserdisc.Platform.LaserDiscRespProtocolDecodingError(message)

sealed trait RESPCoproduct {
  final val gen = Generic[RESP]

  final type RESPCoproduct = gen.Repr
}

sealed trait RESPFunctions extends EitherSyntax { this: RESPCodecs =>

  import BitVectorDecoding._

  private[this] final val readDiscriminator: BitVector => ((BitVector, BitVector) => String | State) => String | State =
    bits => f => bits.consumeThen(BitsInByte)(_.asLeft, f)

  final val stateOf: BitVector => String | State = bits =>
    readDiscriminator(bits) {
      case (`plus` | `minus` | `colon`, _) =>
        val eomIndex = bits.bytes.indexOfSlice(crlfBytes, from = 0L)
        val size     = eomIndex * BitsInByte + crlf.size

        if (eomIndex == -1) Right(Incomplete)
        else if (size < bits.size) Right(CompleteWithRemainder(bits.take(size), bits.drop(size)))
        else Right(Complete)
      case (`dollar`, payload) =>
        evalWithSizeDecodedFrom(payload) {
          case Left(_) => Incomplete
          case Right(DecodeResult(value, remainder)) =>
            val decoded  = BitsInByte + value.bits.size + crlf.size
            val expected = value.decoded * BitsInByte + crlf.size
            val size     = decoded + expected

            if (value.decoded >= 0 && remainder.size == expected) Complete
            else if (value.decoded >= 0 && remainder.size > expected) CompleteWithRemainder(bits.take(size), bits.drop(size))
            else if (value.decoded == -1 && remainder.isEmpty) Complete
            else if (value.decoded == -1 && remainder.nonEmpty) CompleteWithRemainder(bits.take(decoded), bits.drop(decoded))
            else MissingBits(expected - remainder.size)
        }
      case (`star`, payload) =>
        attemptEvalWithSizeDecodedFrom(payload) {
          case Left(_) => Right(Incomplete)
          case Right(DecodeResult(value, remainder)) =>
            val size = BitsInByte + value.bits.size + crlf.size

            if (value.decoded == -1 && remainder.isEmpty) Right(Complete)
            else if (value.decoded == -1 && remainder.nonEmpty) Right(CompleteWithRemainder(bits.take(size), bits.drop(size)))
            else stateOfArr(value.decoded, remainder, bits.take(size))
        }
      case (other, _) => Left(s"unidentified RESP type when checking the state: ${other.toUtf8} (${other.toHex})")
    }

  @tailrec
  private[this] final def stateOfArr(missing: Long, remainder: BitVector, bits: BitVector): String | State = missing match {
    case 0L =>
      if (remainder.isEmpty) Right(Complete)
      else Right(CompleteWithRemainder(bits, remainder))

    case _ =>
      stateOf(remainder) match {
        case Right(CompleteWithRemainder(c, r)) => stateOfArr(missing - 1, r, bits ++ c)
        case Right(Complete) if missing == 1    => Right(Complete)
        case Right(Complete)                    => Right(Incomplete)
        case Right(incomplete)                  => Right(incomplete)
        case left                               => left
      }
  }

  private[this] final def evalWithSizeDecodedFrom[A](bits: BitVector)(f: (Incomplete | DecodeResult[Repr[Long]]) => A): String | A =
    attemptEvalWithSizeDecodedFrom(bits)(x => Right(f(x)))

  private[this] final def attemptEvalWithSizeDecodedFrom[A](bits: BitVector)(
      f: (Incomplete | DecodeResult[Repr[Long]]) => String | A
  ): String | A =
    crlfTerminatedReprOfLongDecoder
      .decode(bits)
      .fold(
        {
          case MatchingDiscriminatorNotFound(_, _) => f(Left(Incomplete))
          case err                                 => Left(err.message)
        },
        res => f(Right(res))
      )
}

object BitVectorDecoding {
  type Incomplete = Incomplete.type
  type Complete   = Complete.type

  sealed trait State                                                                extends Product with Serializable
  final case class MissingBits(stillToReceive: Long)                                extends State
  final case class CompleteWithRemainder(complete: BitVector, remainder: BitVector) extends State
  final case object Incomplete                                                      extends State
  final case object Complete                                                        extends State
}

object RESP extends RESPCodecs with RESPCoproduct with RESPFunctions
