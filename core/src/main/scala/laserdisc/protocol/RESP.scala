package laserdisc
package protocol

import java.nio.charset.StandardCharsets.UTF_8
import java.{lang => j}

import scodec.Decoder.decodeCollect
import scodec.Encoder.encodeSeq
import scodec.bits.{BitVector, _}
import scodec.codecs.{filtered, fixedSizeBytes}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Err, SizeBound}

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
  *   val simpleString: SimpleString = str("some string")
  * }}}
  *
  * @param value The wrapped string value
  */
final class SimpleString private[protocol] (val value: String) extends RESP {
  override def hashCode(): Int = value.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case other: SimpleString => other.value == value
    case _                   => false
  }
  override def toString: String = s"SimpleString($value)"
}
object SimpleString {
  final def unapply(simpleString: SimpleString): Option[String] = Some(simpleString.value)
}

/**
  * RESP [[https://redis.io/topics/protocol#resp-errors Errors]]
  *
  * RESP [[Error]]s are also [[scala.RuntimeException]]s, although
  * __where possible__ they will not contain stacktrace data
  *
  * These can be constructed by using the [[RESPBuilders#err]] method
  *
  * @example
  * {{{
  *   import laserdisc.protocol.RESP._
  *
  *   val error: Error = err("some error message")
  * }}}
  *
  * @param message The wrapped exception's message
  */
final class Error private[protocol] (val message: String)
    extends laserdisc.Platform.LaserDiscRuntimeError(message)
    with RESP {
  override def hashCode(): Int = message.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case other: Error => other.message == message
    case _            => false
  }
  override def toString: String = s"Error($message)"
}
object Error {
  final def unapply(error: Error): Option[String] = Some(error.message)
}

/**
  * RESP [[https://redis.io/topics/protocol#resp-integers Integers]]
  *
  * These can be constructed by using the [[RESPBuilders#int]] method
  *
  * @note Sometimes the values 0 and 1 are used to represent boolean
  * values. In this case 0 corresponds to False while 1 to True,
  * respectively.
  *
  * @example
  * {{{
  *   import laserdisc.protocol.RESP._
  *
  *   val integer: Integer = int(42)
  * }}}
  *
  * @param value The wrapped long value
  */
final class Integer private[protocol] (val value: Long) extends RESP {
  override def hashCode(): Int = value.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case other: Integer => other.value == value
    case _              => false
  }
  override def toString: String = s"Integer($value)"
}
object Integer {
  final def unapply(integer: Integer): Option[Long] = Some(integer.value)
}

/**
  * RESP [[https://redis.io/topics/protocol#resp-bulk-strings Bulk Strings]]
  *
  * There can be 2 cases:
  *  - `null` bulk strings, where the length is -1 and no actual underlying string is present
  *  - actual (non-null) bulk strings, where the length is >= 0
  *
  * Non-null [[BulkString]]s can be constructed using the [[RESPBuilders#bulk]]
  * method
  *
  * @note A forwarder for `null` [[BulkString]]s is present too and represented
  *       using the `final val`s [[RESPBuilders.nullBulk]]
  *
  * @example
  * {{{
  *   import laserdisc.protocol.RESP._
  *
  *   val nonNullBulkString: NonNullBulkString = bulk("some string")
  *   val nullBulkString: NullBulkString       = nullBulk
  * }}}
  *
  * @see [[Show]]
  */
sealed trait BulkString     extends RESP
sealed trait NullBulkString extends BulkString
case object NullBulkString  extends NullBulkString

/**
  * This is the special case of a non-null RESP [[BulkString]]
  *
  * These can be constructed by using the [[RESPBuilders#bulk]]
  * method
  *
  * @param value The wrapped bulk string value
  */
final class NonNullBulkString private[protocol] (val value: String) extends BulkString {
  override def hashCode(): Int = value.hashCode
  override def equals(obj: Any): Boolean = obj match {
    case other: NonNullBulkString => other.value == value
    case _                        => false
  }
  override def toString: String = s"BulkString($value)"
}
object NonNullBulkString {
  final def unapply(nonNullBulkString: NonNullBulkString): Option[String] = Some(nonNullBulkString.value)

  implicit final val nonNullBulkStringShow: Show[NonNullBulkString] = Show.instance(_.value)
}

/**
  * RESP [[https://redis.io/topics/protocol#resp-arrays Arrays]]
  *
  * There can be 2 cases:
  *  - `nil` arrays, where the length is -1 and no array element is present
  *  - actual (non-nil) arrays, where the length is >= 0
  *
  * Non-nil [[Array]]s can be constructed using the
  * [[[RESPBuilders#arr(xs:Seq[laserdisc\.protocol\.RESP])* RESPBuilders#arr(xs: Seq[RESP])]]] method.
  *
  * @note [[[RESPBuilders#arr(one:laserdisc\.protocol\.RESP,rest:laserdisc\.protocol\.RESP*)* RESPBuilders#arr(one: RESP, rest: RESP*)]]]
  * is an overload which supports the creation of guaranteed non-empty
  * sequences only. This is achieved through the usage of one fixed
  * parameter followed by a var-arg of the same
  *
  * @note A forwarder for `nil` is present too and represented using
  *       the `final val`s [[RESPBuilders.nilArray]]
  *
  * @example
  * {{{
  *   import laserdisc.protocol.RESP._
  *
  *   val nonNilArray: NonNilArray             = arr(Vector(str("hello"), str("world")))
  *   val guaranteedNonEmptyArray: NonNilArray = arr(str("hello"), str("world"))
  *   val emptyArray: NonNilArray              = arr(Vector.empty)
  *   val nilArray: NilArray                   = nilArray
  * }}}
  */
sealed trait Array    extends RESP
sealed trait NilArray extends Array
case object NilArray  extends NilArray

/**
  * This is the special case of a non-nil RESP [[Array]]
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
final class NonNilArray private[protocol] (val elements: Vector[RESP]) extends Array {
  override def hashCode(): Int = elements.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case other: NonNilArray => other.elements == elements
    case _                  => false
  }
  override def toString: String = s"Array(${elements.mkString(",")})"
}
object NonNilArray {
  final def unapply(nonEmptyArray: NonNilArray): Option[Vector[RESP]] = Some(nonEmptyArray.elements)
}

sealed trait RESPBuilders {
  final def str(value: String): SimpleString = new SimpleString(value)

  final def err(message: String): Error = new Error(message)

  final def int(value: Long): Integer = new Integer(value)

  final val nullBulk: NullBulkString           = NullBulkString
  final def bulk(s: String): NonNullBulkString = new NonNullBulkString(s)

  final val nilArray: NilArray                       = NilArray
  final def arr(one: RESP, rest: RESP*): NonNilArray = arr(one +: rest)
  final def arr(xs: Seq[RESP]): NonNilArray          = new NonNilArray(xs.toVector)
}

sealed trait RESPCodecs { this: RESPBuilders =>

  protected final val utf8       = new LenientStringCodec(UTF_8)
  protected final val BitsInByte = 8L
  protected final val plus :: minus :: colon :: dollar :: star :: crlf :: minusOne :: zero :: Nil =
    (hex"2b" :: hex"2d" :: hex"3a" :: hex"24" :: hex"2a" :: hex"0d0a" :: hex"2d31" :: hex"30" :: Nil)
      .map(_.bits)
  private final val crlfSize      = crlf.size
  protected final val crlfBytes   = crlf.bytes
  private final val crlfBytesSize = crlfBytes.size

  private final def crlfTerminatedStringOfSize(size: Long): Codec[String] =
    filtered(
      utf8,
      new Codec[BitVector] {
        override final def sizeBound: SizeBound                        = SizeBound.unknown
        override final def encode(bits: BitVector): Attempt[BitVector] = Attempt.successful(bits ++ crlf)
        override final def decode(bits: BitVector): Attempt[DecodeResult[BitVector]] =
          bits.bytes.indexOfSlice(crlfBytes, size) match {
            case -1 => Attempt.failure(Err(s"Does not contain 'CRLF' termination bytes. Vector content: ${bits.toByteArray map (_.toChar) mkString ""}"))
            case i  => Attempt.successful(DecodeResult(bits.take(i * BitsInByte), bits.drop(i * BitsInByte + crlfSize)))
          }
      }
    ).withToString("crlf-terminated string")

  private final val crlfTerminatedString = crlfTerminatedStringOfSize(0)

  protected final val longAsCRLFTerminatedString =
    crlfTerminatedString.narrow[Long](
      s =>
        try Attempt.successful(j.Long.parseLong(s))
        catch { case _: NumberFormatException => Attempt.failure(Err(s"Expected long but found $s")) },
      _.toString
    )
    .withToString("crlf-terminated string repr of long")

  private final val simpleStringCodec: Codec[SimpleString] =
    crlfTerminatedString.xmap[SimpleString](str, _.value).withToString("simple-string")

  private final val errorCodec: Codec[Error] =
    crlfTerminatedString.xmap[Error](err, _.message).withToString("error")

  private final val integerCodec: Codec[Integer] =
    longAsCRLFTerminatedString.xmap[Integer](int, _.value).withToString("integer")

  private final val bulkStringCodec: Codec[BulkString] = new Codec[BulkString] {

    private final val nullBulkStringBits = minusOne ++ crlf

    private final val decoder = longAsCRLFTerminatedString.flatMap {
      case -1                => Decoder.point(NullBulkString)
      case size if size >= 0 => fixedSizeBytes(size + crlfBytesSize, crlfTerminatedStringOfSize(size)).map(bulk)
      case negSize           => Decoder.liftAttempt(Attempt.failure(failDec(negSize)))
    }

    private final def failDec(negSize: Long) =
      Err.General(s"failed to decode bulk-string of size $negSize", List("size"))

    private final def failEnc(bulkString: BulkString, err: Err) =
      Err.General(s"failed to encode size of [$bulkString]: ${err.messageWithContext}", List("size"))

    override final def sizeBound: SizeBound = SizeBound.unknown

    override final def encode(bulkString: BulkString): Attempt[BitVector] =
      bulkString match {
        case NullBulkString => Attempt.successful(nullBulkStringBits)
        case NonNullBulkString(s) =>
          crlfTerminatedString.encode(s).flatMap { bits =>
            longAsCRLFTerminatedString
              .encode(bits.size / BitsInByte - crlfBytesSize)
              .mapErr(failEnc(bulkString, _))
              .map(_ ++ bits)
          }
      }

    override final def decode(buffer: BitVector): Attempt[DecodeResult[BulkString]] = decoder.decode(buffer)

    override final def toString: String                                             = "bulk-string"
  }

  protected final val arrayCodec: Codec[Array] = new Codec[Array] {

    private final val nilArrayBits   = minusOne ++ crlf

    private final val emptyArrayBits = zero ++ crlf

    private final def checkSize(v: Vector[RESP], expectedSize: Long) =
      if (v.size == expectedSize) Attempt.successful(v)
      else Attempt.failure(Err(s"Insufficient number of elements: decoded ${v.size} instead of $expectedSize"))

    private final val decoder = longAsCRLFTerminatedString.flatMap {
      case -1 => Decoder.point(NilArray)
      case 0  => Decoder.point(arr(Seq.empty))
      case size if size > 0 =>
        Decoder(decodeCollect[Vector, RESP](respCodec, Some(size.toInt))(_))
          .narrow[Vector[RESP]](checkSize(_, size), identity)
          .map(arr(_))
      case negSize => Decoder.liftAttempt(Attempt.failure(failDec(negSize)))
    }

    private final def failDec(negSize: Long) =
      Err.General(s"failed to decode array of size $negSize", List("size"))

    private final def failEnc(array: Array, err: Err) =
      Err.General(s"failed to encode size of [$array]: ${err.messageWithContext}", List("size"))

    override final def sizeBound: SizeBound = SizeBound.unknown

    override final def encode(array: Array): Attempt[BitVector] = array match {
      case NilArray                    => Attempt.successful(nilArrayBits)
      case NonNilArray(v) if v.isEmpty => Attempt.successful(emptyArrayBits)
      case NonNilArray(v) =>
        longAsCRLFTerminatedString
          .encode(v.size.toLong)
          .mapErr(failEnc(array, _))
          .flatMap(size => encodeSeq(respCodec)(v).map(size ++ _))
    }
    override final def decode(bits: BitVector): Attempt[DecodeResult[Array]] = decoder.decode(bits)
    override final def toString: String                                      = "array"
  }

  implicit final val respCodec: Codec[RESP] = new Codec[RESP] {

    override final def sizeBound: SizeBound = SizeBound.unknown

    override final def encode(value: RESP): Attempt[BitVector] = value match {
      case simpleString : SimpleString  => simpleStringCodec.encode(simpleString).map(plus ++ _)
      case error        : Error         => errorCodec.encode(error).map(minus ++ _)
      case integer      : Integer       => integerCodec.encode(integer).map(colon ++ _)
      case bulkString   : BulkString    => bulkStringCodec.encode(bulkString).map(dollar ++ _)
      case array        : Array         => arrayCodec.encode(array).map(star ++ _)
    }

    override final def decode(bits: BitVector): Attempt[DecodeResult[RESP]] =
      bits
        .consume(BitsInByte) {
          case `plus`   => Right(simpleStringCodec)
          case `minus`  => Right(errorCodec)
          case `colon`  => Right(integerCodec)
          case `dollar` => Right(bulkStringCodec)
          case `star`   => Right(arrayCodec)
          case other    => Left(s"unidentified RESP type (Hex: ${other.toHex})")
        }
        .fold(
          error => Attempt.failure(Err(error)),
          { case (remainder, codec) => codec.decode(remainder) }
        )

    override final def toString: String = "RESP"
  }
}

sealed trait RESPFunctions { this: RESPCodecs =>

  import BitVectorDecoding._

  final def stateOf: BitVector => String | BitVectorState =
    bits => bits.consume(BitsInByte) {
      case `plus`   => Right(SimpleString)
      case `minus`  => Right(Error)
      case `colon`  => Right(Integer)
      case `dollar` => Right(BulkString)
      case `star`   => Right(RedisArray)
      case other    => Left(s"unidentified RESP type (Hex: ${other.toHex})")
    }
    .flatMap {
      case (remainder, BulkString) =>
        evalWithSizeDecodedFrom(remainder) {
          case Left(_)   => IncompleteVector
          case Right(ds) =>
            val expectedBulkSize = (ds.value * BitsInByte) + crlf.size

            if (ds.value >= 0 && ds.remainder.size == expectedBulkSize)
              CompleteVector // Complete: expected size

            else if (ds.value >= 0 && ds.remainder.size > expectedBulkSize) {
              val completeBulkSize = bits.bytes.indexOfSlice(crlfBytes, 0L) * BitsInByte + crlf.size + expectedBulkSize
              CompleteWithRemainder(bits.take(completeBulkSize), bits.drop(completeBulkSize)) // Complete plus some remainder
            }

            else if (ds.value == -1 && ds.remainder.size == 0)
              CompleteVector // Complete: empty bulk

            else if (ds.value == -1 && ds.remainder.size > 0) {
              val completeBulkSize = 3 * BitsInByte + crlf.size
              CompleteWithRemainder(bits.take(completeBulkSize), bits.drop(completeBulkSize)) // Complete plus some remainder
            }

            else
              MissingBits(expectedBulkSize - ds.remainder.size)
        }

      case (remainder, RedisArray) =>
        arrayCodec.decode(remainder).fold(
          _ => Right(IncompleteVector),
          r => Right(CompleteAndDecoded(r))
        )

      case (_, SimpleString)
           | (_, Integer)
           | (_, Error) => Right(CompleteVector)
    }

  private final def evalWithSizeDecodedFrom[A](bits: BitVector)(f: (IncompleteVector | DecodeResult[Long]) => A): String | A =
    if (bits.bytes.indexOfSlice(crlfBytes, 0L) == -1) Right(f(Left(IncompleteVector)))
    else (longAsCRLFTerminatedString.decode(bits) map (r => f(Right(r)))).fold (
      err => Left(err.message),
      res => Right(res)
    )

  private sealed trait ContentType
  private final case object BulkString extends ContentType
  private final case object RedisArray extends ContentType
  private final case object SimpleString extends ContentType
  private final case object Integer extends ContentType
  private final case object Error extends ContentType
}

object BitVectorDecoding {

  type IncompleteVector = IncompleteVector.type
  type CompleteVector = CompleteVector.type

  sealed trait BitVectorState extends Product with Serializable
  final case class MissingBits(stillToReceive: Long) extends BitVectorState
  final case class CompleteAndDecoded(serial: DecodeResult[RESP]) extends BitVectorState
  final case class CompleteWithRemainder(complete: BitVector, remainder: BitVector) extends BitVectorState
  final case object IncompleteVector extends BitVectorState
  final case object CompleteVector extends BitVectorState
}

object RESP extends RESPBuilders with RESPCodecs with RESPFunctions
