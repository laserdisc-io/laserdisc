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
sealed trait RESP extends Any with Serializable

/**
  * RESP [[https://redis.io/topics/protocol#resp-simple-strings Simple Strings]]
  *
  * These can be constructed by using the [[RESPBuilders#str]] method
  *
  * @note Sometimes the value "OK" is used to represent a successful
  * acknowledgement/processing of a command.
  *
  * @example
  * {{{
  *   import laserdisc.protocol.RESP._
  *
  *   val s: Str = str("some string")
  * }}}
  *
  * @param value The wrapped string value
  */
final class Str private[protocol] (val value: String) extends RESP {
  override def hashCode(): Int = value.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case other: Str => other.value == value
    case _          => false
  }
  override def toString: String = s"Str($value)"
}
object Str {
  final def unapply(str: Str): Option[String] = Some(str.value)
}

/**
  * RESP [[https://redis.io/topics/protocol#resp-errors Errors]]
  *
  * RESP [[Err]]s are also [[scala.RuntimeException]]s, although
  * __where possible__ they will not contain stacktrace data
  *
  * These can be constructed by using the [[RESPBuilders#err]] method
  *
  * @example
  * {{{
  *   import laserdisc.protocol.RESP._
  *
  *   val e: Err = err("some error message")
  * }}}
  * @param message The wrapped exception's message
  */
final class Err private[protocol] (val message: String) extends laserdisc.Platform.LaserDiscRuntimeError(message) with RESP {
  override def hashCode(): Int = message.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case other: Err => other.message == message
    case _          => false
  }
  override def toString: String = s"Err($message)"
}
object Err {
  final def unapply(err: Err): Option[String] = Some(err.message)
}

/**
  * RESP [[https://redis.io/topics/protocol#resp-integers Integers]]
  *
  * These can be constructed by using the [[RESPBuilders#num]] method
  *
  * @note Sometimes the values 0 and 1 are used to represent boolean
  * values. In this case 0 corresponds to False while 1 to True,
  * respectively.
  *
  * @example
  * {{{
  *   import laserdisc.protocol.RESP._
  *
  *   val n: Num = num(42)
  * }}}
  *
  * @param value The wrapped long value
  */
final class Num private[protocol] (val value: Long) extends RESP {
  override def hashCode(): Int = value.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case other: Num => other.value == value
    case _          => false
  }
  override def toString: String = s"Num($value)"
}
object Num {
  final def unapply(num: Num): Option[Long] = Some(num.value)
}

/**
  * RESP [[https://redis.io/topics/protocol#resp-bulk-strings Bulk Strings]]
  *
  * There can be 2 cases:
  *  - `null` bulk strings, where the length is -1 and no actual underlying string is present
  *  - actual (non-null) bulk strings, where the length is >= 0
  *
  * Non-null [[GenBulk]]s can be constructed using the [[RESPBuilders#bulk]]
  * method
  *
  * @note A forwarder for `null` [[GenBulk]]s is present too and represented
  *       using the `final val`s [[RESPBuilders.nullBulk]]
  * @example
  * {{{
  *   import laserdisc.protocol.RESP._
  *
  *   val b: Bulk      = bulk("some string")
  *   val nb: NullBulk = nullBulk
  * }}}
  * @see [[Show]]
  */
sealed trait GenBulk  extends RESP
sealed trait NullBulk extends GenBulk
case object NullBulk  extends NullBulk

/**
  * This is the special case of a non-null RESP [[GenBulk]]
  *
  * These can be constructed by using the [[RESPBuilders#bulk]]
  * method
  *
  * @param value The wrapped bulk string value
  */
final class Bulk private[protocol] (val value: String) extends GenBulk {
  override def hashCode(): Int = value.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case other: Bulk => other.value == value
    case _           => false
  }
  override def toString: String = s"Bulk($value)"
}
object Bulk {
  final def unapply(bulk: Bulk): Option[String] = Some(bulk.value)

  implicit final val bulkShow: Show[Bulk] = Show.instance(_.value)
}

/**
  * RESP [[https://redis.io/topics/protocol#resp-arrays Arrays]]
  *
  * There can be 2 cases:
  *  - `nil` arrays, where the length is -1 and no array element is present
  *  - actual (non-nil) arrays, where the length is >= 0
  *
  * Non-nil [[GenArr]]s can be constructed using the
  * [[[RESPBuilders#arr(xs:Seq[laserdisc\.protocol\.RESP])* RESPBuilders#arr(xs: Seq[RESP])]]] method.
  *
  * @note [[[RESPBuilders#arr(one:laserdisc\.protocol\.RESP,rest:laserdisc\.protocol\.RESP*)* RESPBuilders#arr(one: RESP, rest: RESP*)]]]
  * is an overload which supports the creation of guaranteed non-empty
  * sequences only. This is achieved through the usage of one fixed
  * parameter followed by a var-arg of the same
  * @note A forwarder for `nil` is present too and represented using
  *       the `final val`s [[RESPBuilders.nilArr]]
  * @example
  * {{{
  *   import laserdisc.protocol.RESP._
  *
  *   val a: Arr                  = arr(Vector(str("hello"), str("world")))
  *   val guaranteedNonEmpty: Arr = arr(str("hello"), str("world"))
  *   val empty: Arr              = arr(Vector.empty)
  *   val nil: NilArr             = nilArr
  * }}}
  */
sealed trait GenArr extends RESP
sealed trait NilArr extends GenArr
case object NilArr  extends NilArr

/**
  * This is the special case of a non-nil RESP [[GenArr]]
  *
  * These can be constructed by using the [[[RESPBuilders#arr(xs:Seq[laserdisc\.protocol\.RESP])* RESPBuilders#arr(xs: Seq[RESP])]]]
  * method
  *
  * __or__
  *
  * by resorting to the overloaded
  * [[[RESPBuilders#arr(one:laserdisc\.protocol\.RESP,rest:laserdisc\.protocol\.RESP*)* RESPBuilders#arr(one: RESP, rest: RESP*)]]]
  * method which expects one parameter to be supplied followed
  * by a (possibly empty) sequence of [[RESP]]s (vararg).
  *
  * @param elements The wrapped array values, as a [[scala.Vector]] of [[RESP]]
  */
final class Arr private[protocol] (val elements: Vector[RESP]) extends GenArr {
  override def hashCode(): Int = elements.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case other: Arr => other.elements == elements
    case _          => false
  }
  override def toString: String = s"Arr(${elements.mkString(",")})"
}
object Arr {
  final def unapply(arr: Arr): Option[Vector[RESP]] = Some(arr.elements)
}

private[protocol] final case class Repr[A](decoded: A, bits: BitVector)

sealed trait RESPBuilders {
  final def str(value: String): Str = new Str(value)

  final def err(message: String): Err = new Err(message)

  final def num(value: Long): Num = new Num(value)

  final val nullBulk: NullBulk    = NullBulk
  final def bulk(s: String): Bulk = new Bulk(s)

  final val nilArr: NilArr                   = NilArr
  final def arr(one: RESP, rest: RESP*): Arr = arr(one +: rest)
  final def arr(xs: Seq[RESP]): Arr          = new Arr(xs.toVector)
}

sealed trait RESPCodecs extends BitVectorSyntax { this: RESPBuilders =>
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
  private[this] final val strCodec: Codec[Str] = crlfTerminatedStringCodec.xmap[Str](str, _.value)
  private[this] final val errCodec: Codec[Err] = crlfTerminatedStringCodec.xmap[Err](err, _.message)
  private[this] final val numCodec: Codec[Num] = crlfTerminatedLongCodec.xmap[Num](num, _.value)
  private[this] final val bulkCodec: Codec[GenBulk] = new Codec[GenBulk] {
    private[this] final val nullBulkBits = minusOne ++ crlf
    private[this] final val decoder = crlfTerminatedLongCodec.flatMap {
      case -1                => Decoder.point(NullBulk)
      case size if size >= 0 => fixedSizeBytes(size + crlfBytesSize, crlfTerminatedCodec(utf8Codec, size)).map(bulk)
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
    private[this] final def checkSize(v: Vector[RESP], expectedSize: Long): Attempt[Vector[RESP]] =
      if (v.size == expectedSize) Attempt.successful(v)
      else Attempt.failure(SErr(s"Insufficient number of elements: decoded ${v.size} instead of $expectedSize"))
    private[this] final val decoder = crlfTerminatedLongCodec.flatMap {
      case -1 => Decoder.point(NilArr)
      case 0  => Decoder.point(arr(Seq.empty))
      case size if size > 0 =>
        Decoder(decodeCollect[Vector, RESP](respCodec, Some(size.toInt))(_)).narrow[Vector[RESP]](checkSize(_, size), identity).map(arr(_))
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
        case Left(e)                            => Left(e)
        case Right(CompleteWithRemainder(c, r)) => stateOfArr(missing - 1, r, bits ++ c)
        case Right(Complete) if missing == 1    => Right(Complete)
        case Right(Complete)                    => Right(Incomplete)
        case Right(incomplete)                  => Right(incomplete)
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

object RESP extends RESPBuilders with RESPCodecs with RESPFunctions
