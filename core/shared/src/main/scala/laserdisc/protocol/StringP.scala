/*
 * Copyright (c) 2018-2026 LaserDisc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package laserdisc
package protocol

import Protocol.Single

object StringP {
  sealed trait Bit
  object Bit {
    object set   extends Bit
    object unset extends Bit

    implicit val bitShow: Show[Bit] = Show.instance {
      case `set`   => "1"
      case `unset` => "0"
    }
    implicit val num2BitRead: Read[Num, Bit] = Read[Num, Boolean].map(flag => if (flag) set else unset)
  }

  sealed trait Bitwise
  object Bitwise {
    case object and extends Bitwise
    case object or  extends Bitwise
    case object xor extends Bitwise

    final val diff: ExtendedBitwise  = new ExtendedBitwise("DIFF")
    final val diff1: ExtendedBitwise = new ExtendedBitwise("DIFF1")
    final val andor: ExtendedBitwise = new ExtendedBitwise("ANDOR")
    final val one: ExtendedBitwise   = new ExtendedBitwise("ONE")

    implicit val bitwiseShow: Show[Bitwise] = Show.instance {
      case `and` => "AND"
      case `or`  => "OR"
      case `xor` => "XOR"
    }
  }

  /** DIFF, DIFF1, ANDOR and ONE, for BITOP: Redis 8.2 */
  final class ExtendedBitwise private[StringP] (private[StringP] val name: String)
  object ExtendedBitwise {
    implicit val extendedBitwiseShow: Show[ExtendedBitwise] = Show.instance(_.name)
  }

  /** BYTE and BIT, for BITCOUNT and BITPOS: whether start and end count bytes or bits */
  sealed trait BitUnit
  object BitUnit {
    object byte extends BitUnit
    object bit  extends BitUnit

    implicit val bitUnitShow: Show[BitUnit] = Show.instance {
      case `byte` => "BYTE"
      case `bit`  => "BIT"
    }
  }

  /** The steps of a BITFIELD, in order: GET, SET and INCRBY read and write numbers of the given type at the given offset, and OVERFLOW sets
    * what the SET and INCRBY after it do when the number doesn't fit
    */
  final class BitField private (val params: List[String]) {
    import BitField.*

    def get(encoding: Encoding, offset: Offset): BitField              = new BitField(params ++ List("GET", encoding.param, offset.param))
    def set(encoding: Encoding, offset: Offset, value: Long): BitField =
      new BitField(params ++ List("SET", encoding.param, offset.param, value.toString))
    def incrBy(encoding: Encoding, offset: Offset, increment: Long): BitField =
      new BitField(params ++ List("INCRBY", encoding.param, offset.param, increment.toString))
    def overflow(overflow: Overflow): BitField = new BitField(params ++ List("OVERFLOW", overflow.param))
  }
  object BitField {

    /** A signed (`i`) or unsigned (`u`) number of the given width in bits */
    final class Encoding private[BitField] (private[protocol] val param: String)

    /** Where a number starts: at a bit, or at the n-th number of the same width */
    final class Offset private[BitField] (private[protocol] val param: String)

    /** WRAP, SAT and FAIL: wrap around, stay at the smallest or largest number, or do nothing and reply nil */
    final class Overflow private[BitField] (private[protocol] val param: String)

    def i(bits: SignedBitWidth): Encoding   = new Encoding(s"i${bits.value}")
    def u(bits: UnsignedBitWidth): Encoding = new Encoding(s"u${bits.value}")

    def bit(offset: NonNegLong): Offset  = new Offset(offset.value.toString)
    def field(index: NonNegLong): Offset = new Offset(s"#${index.value}")

    final val wrap: Overflow = new Overflow("WRAP")
    final val sat: Overflow  = new Overflow("SAT")
    final val fail: Overflow = new Overflow("FAIL")

    private final val none = new BitField(Nil)

    def get(encoding: Encoding, offset: Offset): BitField                     = none.get(encoding, offset)
    def set(encoding: Encoding, offset: Offset, value: Long): BitField        = none.set(encoding, offset, value)
    def incrBy(encoding: Encoding, offset: Offset, increment: Long): BitField = none.incrBy(encoding, offset, increment)
    def overflow(overflow: Overflow): BitField                                = none.overflow(overflow)
  }

  /** The options of INCREX, for whole numbers (`int`) or decimal numbers (`float`): bounds the result must stay within, whether to stop at
    * a bound instead of refusing the increment (SATURATE), and an expiry, which can be set only when the key has none yet (ENX)
    */
  final class IncrExOptions[A] private (val params: List[String]) {
    def lowerBound(a: A)(implicit A: Show[A]): IncrExOptions[A] = new IncrExOptions(params ++ List("LBOUND", A.show(a)))
    def upperBound(a: A)(implicit A: Show[A]): IncrExOptions[A] = new IncrExOptions(params ++ List("UBOUND", A.show(a)))
    def saturate: IncrExOptions[A]                              = new IncrExOptions(params :+ "SATURATE")
    def expiry(expiry: GetExpiry): IncrExOptions[A]             = new IncrExOptions(params ++ expiry.params)

    /** Sets the expiry only if the key has none yet */
    def expiryIfNone(expiry: Expiry): IncrExOptions[A] = new IncrExOptions(params ++ expiry.params :+ "ENX")
  }
  object IncrExOptions {
    final val int: IncrExOptions[Long]          = new IncrExOptions[Long](Nil)
    final val float: IncrExOptions[ValidDouble] = new IncrExOptions[ValidDouble](Nil)
  }

  /** The reply of INCREX: the value now, and the increment that was applied, which is 0 when a bound refused it */
  final case class Incremented[A](value: A, increment: A)

  private[protocol] final val incrementedLong: Read[Arr.AsTuple[Num *: Num *: EmptyTuple], Incremented[Long]] =
    Read[Arr.AsTuple[Num *: Num *: EmptyTuple], Long *: Long *: EmptyTuple].map { case value *: increment *: _ =>
      Incremented(value, increment)
    }
  private[protocol] final val incrementedDouble: Read[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple], Incremented[Double]] =
    Read[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple], Double *: Double *: EmptyTuple].map { case value *: increment *: _ =>
      Incremented(value, increment)
    }

  /** Where a match is, in each of the two strings LCS compares */
  final case class LcsRange(start: NonNegInt, end: NonNegInt)
  final case class LcsMatch(first: LcsRange, second: LcsRange, length: NonNegInt)
  final case class LcsMatches(matches: Seq[LcsMatch], length: NonNegInt)

  private[protocol] final val bitFieldResults: Read[Arr.Of[OrNullBulk[Num]], Seq[Option[Long]]] =
    Read[Arr.Of[OrNullBulk[Num]], Seq[Option[Long]]]

  private implicit final val lcsRange: Read[Arr.AsTuple[Num *: Num *: EmptyTuple], LcsRange] =
    Read[Arr.AsTuple[Num *: Num *: EmptyTuple], NonNegInt *: NonNegInt *: EmptyTuple].map { case start *: end *: _ => LcsRange(start, end) }
  private implicit final val lcsMatch: Read[Arr.AsTuple[Arr.AsTuple[Num *: Num *: EmptyTuple] *: Arr.AsTuple[Num *: Num *: EmptyTuple] *: Num *: EmptyTuple], LcsMatch] =
    Read[Arr.AsTuple[
      Arr.AsTuple[Num *: Num *: EmptyTuple] *: Arr.AsTuple[Num *: Num *: EmptyTuple] *: Num *: EmptyTuple
    ], LcsRange *: LcsRange *: NonNegInt *: EmptyTuple]
      .map { case first *: second *: length *: _ => LcsMatch(first, second, length) }

  private[protocol] final val lcsMatches: Read[Record, LcsMatches] = Read.record("LCS matches") { f =>
    for {
      matches <- f[Arr.Of[
        Arr.AsTuple[Arr.AsTuple[Num *: Num *: EmptyTuple] *: Arr.AsTuple[Num *: Num *: EmptyTuple] *: Num *: EmptyTuple]
      ], Seq[
        LcsMatch
      ]]("matches")
      length <- f.num[NonNegInt]("len")
    } yield LcsMatches(matches, length)
  }

  /** EX, PX, EXAT, PXAT and KEEPTTL, for SET */
  sealed trait SetExpiry { def params: List[String] }

  /** EX, PX, EXAT, PXAT and PERSIST, for GETEX */
  sealed trait GetExpiry { def params: List[String] }

  sealed trait Expiry extends SetExpiry with GetExpiry
  object Expiry {
    private final class At(override final val params: List[String]) extends Expiry

    def seconds(v: PosLong): Expiry   = new At(List("EX", Show[PosLong].show(v)))
    def millis(v: PosLong): Expiry    = new At(List("PX", Show[PosLong].show(v)))
    def atSeconds(v: PosLong): Expiry = new At(List("EXAT", Show[PosLong].show(v)))
    def atMillis(v: PosLong): Expiry  = new At(List("PXAT", Show[PosLong].show(v)))

    final val keepTtl: SetExpiry = new SetExpiry { override final val params: List[String] = List("KEEPTTL") }
    final val persist: GetExpiry = new GetExpiry { override final val params: List[String] = List("PERSIST") }
  }

  sealed trait Flag
  object Flag {
    object nx extends Flag
    object xx extends Flag

    implicit val flagShow: Show[Flag] = Show.instance {
      case `nx` => "NX"
      case `xx` => "XX"
    }
  }

  /** IFEQ, which Valkey and Redis added separately from IFNE, IFDEQ and IFDNE */
  final class IfEq private (val params: List[String])
  object IfEq {
    def apply[A: Show](a: A): IfEq = new IfEq(List("IFEQ", Show[A].show(a)))
  }

  sealed trait Compare { def params: List[String] }
  object Compare       {
    private final class Of(override final val params: List[String]) extends Compare

    def ifNe[A: Show](a: A): Compare   = new Of(List("IFNE", Show[A].show(a)))
    def ifDeq(digest: String): Compare = new Of(List("IFDEQ", digest))
    def ifDne(digest: String): Compare = new Of(List("IFDNE", digest))
  }
}

trait StringBaseP extends Features {
  import StringP.{Bit, BitField, BitUnit, Bitwise, Compare, Expiry, Flag, GetExpiry, IfEq, IncrExOptions, SetExpiry}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "APPEND",
      "BITCOUNT",
      "BITFIELD: FAIL GET INCRBY OVERFLOW SAT SET WRAP",
      "BITFIELD_RO: GET",
      "BITOP: AND NOT OR XOR",
      "BITPOS",
      "DECR",
      "DECRBY",
      "GET",
      "GETBIT",
      "GETDEL",
      "GETEX: EX EXAT PERSIST PX PXAT",
      "GETRANGE",
      "GETSET",
      "INCR",
      "INCRBY",
      "INCRBYFLOAT",
      "MGET",
      "MSET",
      "MSETNX",
      "PSETEX",
      "SET: EX EXAT GET KEEPTTL NX PX PXAT XX",
      "SETBIT",
      "SETEX",
      "SETNX",
      "SETRANGE",
      "STRLEN",
      "SUBSTR"
    )

  private final val minusOneIsNone = RESPRead.instance(Read.numMinusOneIsNone[NonNegInt])

  object stringtypes {
    final val bit      = Bit
    final val bitfield = BitField
    final val bitUnit  = BitUnit
    final val bitwise  = Bitwise
    final val expiry   = Expiry
    final val flag     = Flag
    final val increx   = IncrExOptions
    final val compare  = Compare

    final def ifEq[A: Show](a: A): IfEq = IfEq(a)
  }

  final def append[A: Show](key: Key, value: A): Single.Aux[NonNegInt] = Protocol("APPEND", key *: value *: EmptyTuple).as[Num, NonNegInt]

  final def bitcount(key: Key): Single.Aux[NonNegInt]                           = Protocol("BITCOUNT", key).as[Num, NonNegInt]
  final def bitcount(key: Key, start: Index, end: Index): Single.Aux[NonNegInt] =
    Protocol("BITCOUNT", key *: start *: end *: EmptyTuple).as[Num, NonNegInt]

  final def bitfield(key: Key, steps: BitField): Single.Aux[Seq[Option[Long]]] =
    Protocol("BITFIELD", key *: steps.params *: EmptyTuple).using(RESPRead.instance(StringP.bitFieldResults))

  final def bitfieldro(key: Key, gets: OneOrMore[(BitField.Encoding, BitField.Offset)]): Single.Aux[Seq[Long]] =
    Protocol("BITFIELD_RO", key *: gets.value.flatMap { case (e, o) => List("GET", e.param, o.param) } *: EmptyTuple)
      .as[Arr.Of[Num], Seq[Long]]

  final def bitop(bitwise: Bitwise, keys: TwoOrMoreKeys, destinationKey: Key): Single.Aux[NonNegInt] =
    Protocol("BITOP", bitwise *: destinationKey *: keys.value *: EmptyTuple).as[Num, NonNegInt]

  final def bitopnot(key: Key, destinationKey: Key): Single.Aux[NonNegInt] =
    Protocol("BITOP", "NOT" :: destinationKey.value :: key.value :: Nil).as[Num, NonNegInt]

  final def bitpos(key: Key, bit: Bit): Single.Aux[Option[NonNegInt]] = Protocol("BITPOS", key *: bit *: EmptyTuple).using(minusOneIsNone)
  final def bitpos(key: Key, bit: Bit, start: Index): Single.Aux[Option[NonNegInt]] =
    Protocol("BITPOS", key *: bit *: start *: EmptyTuple).using(minusOneIsNone)
  final def bitpos(key: Key, bit: Bit, start: Index, end: Index): Single.Aux[Option[NonNegInt]] =
    Protocol("BITPOS", key *: bit *: start *: end *: EmptyTuple).using(minusOneIsNone)

  final def decr[A: Read[Num, _]](key: Key): Single.Aux[A] = Protocol("DECR", key).as[Num, A]

  // TODO verify ok to limit DECRBY to only positive values, REDIS happily accepts 0 and negatives and x + (-decrement)
  final def decrby[A: Read[Num, _]](key: Key, decrement: PosLong): Single.Aux[A] =
    Protocol("DECRBY", key *: decrement *: EmptyTuple).as[Num, A]

  final def get[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]] = Protocol("GET", key).as[OrNullBulk[Bulk], Option[A]]

  final def getbit(key: Key, offset: PosLong): Single.Aux[Bit] = Protocol("GETBIT", key *: offset *: EmptyTuple).as[Num, Bit]

  final def getrange[A: Read[Bulk, _]](key: Key, start: Index, end: Index): Single.Aux[A] =
    Protocol("GETRANGE", key *: start *: end *: EmptyTuple).as[Bulk, A]

  final def getdel[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]] = Protocol("GETDEL", key).as[OrNullBulk[Bulk], Option[A]]

  final def getex[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]] = Protocol("GETEX", key).as[OrNullBulk[Bulk], Option[A]]
  final def getex[A: Read[Bulk, _]](key: Key, expiry: GetExpiry): Single.Aux[Option[A]] =
    Protocol("GETEX", key *: expiry.params *: EmptyTuple).as[OrNullBulk[Bulk], Option[A]]

  final def getset[A: Read[Bulk, _]](key: Key, value: Value): Single.Aux[Option[A]] =
    Protocol("GETSET", key *: value.shown *: EmptyTuple).as[OrNullBulk[Bulk], Option[A]]

  final def incr[A: Read[Num, _]](key: Key): Single.Aux[A] = Protocol("INCR", key).as[Num, A]

  // TODO verify ok to limit INCRBY to only positive values, REDIS happily accepts 0 and negatives
  final def incrby[A: Read[Num, _]](key: Key, increment: PosLong): Single.Aux[A] =
    Protocol("INCRBY", key *: increment *: EmptyTuple).as[Num, A]

  final def incrbyfloat(key: Key, increment: NonZeroDouble): Single.Aux[Double] =
    Protocol("INCRBYFLOAT", key *: increment *: EmptyTuple).as[Bulk, Double]

  /** The values of the keys, as a tuple of as many (a missing key is `None` where the tuple has an `Option`, and an error anywhere else),
    * or as a `Seq` of `Option`s
    */
  final def mget[A: Read[Arr.Of[OrNullBulk[Bulk]], _]](keys: OneOrMoreKeys): Single.Aux[A] =
    Protocol("MGET", keys.value).as[Arr.Of[OrNullBulk[Bulk]], A]

  final def mset[L <: NonEmptyTuple: RESPParamWrite: LUBConstraint[_, (Key, ?)]](l: L): Single.Aux[OK] =
    Protocol("MSET", l).as[Str, OK]
  final def mset[P <: Product: RESPParamWrite: <:!<[EmptyTuple, _]](product: P): Single.Aux[OK] = Protocol("MSET", product).as[Str, OK]
  final def mset[A: Show](values: OneOrMore[(Key, A)]): Single.Aux[OK]                          = Protocol("MSET", values.value).as[Str, OK]

  final def msetnx[L <: NonEmptyTuple: RESPParamWrite: LUBConstraint[_, (Key, ?)]](l: L): Single.Aux[Boolean] =
    Protocol("MSETNX", l).as[Num, Boolean]
  final def msetnx[P <: Product: RESPParamWrite: <:!<[EmptyTuple, _]](product: P): Single.Aux[Boolean] =
    Protocol("MSETNX", product).as[Num, Boolean]
  final def msetnx[A: Show](values: OneOrMore[(Key, A)]): Single.Aux[Boolean] =
    Protocol("MSETNX", values.value).as[Num, Boolean]

  final def psetex[A: Show](key: Key, milliseconds: PosLong, value: A): Single.Aux[OK] =
    Protocol("PSETEX", key *: milliseconds *: value *: EmptyTuple).as[Str, OK]

  final def set[A: Show](key: Key, value: A): Single.Aux[OK]                    = Protocol("SET", key *: value *: EmptyTuple).as[Str, OK]
  final def set[A: Show](key: Key, value: A, expiry: SetExpiry): Single.Aux[OK] =
    Protocol("SET", key *: value *: expiry.params *: EmptyTuple).as[Str, OK]
  final def set[A: Show](key: Key, value: A, flag: Flag): Single.Aux[Option[OK]] =
    Protocol("SET", key *: value *: flag *: EmptyTuple).as[OrNullBulk[Str], Option[OK]]
  final def set[A: Show](key: Key, value: A, expiry: SetExpiry, flag: Flag): Single.Aux[Option[OK]] =
    Protocol("SET", key *: value *: flag *: expiry.params *: EmptyTuple).as[OrNullBulk[Str], Option[OK]]

  /** SET with GET, which replies with the previous value */
  final def setget[A: Read[Bulk, _]](key: Key, value: Value): Single.Aux[Option[A]] =
    Protocol("SET", key *: value.shown *: "GET" *: EmptyTuple).as[OrNullBulk[Bulk], Option[A]]
  final def setget[A: Read[Bulk, _]](key: Key, value: Value, expiry: SetExpiry): Single.Aux[Option[A]] =
    Protocol("SET", key *: value.shown *: "GET" *: expiry.params *: EmptyTuple).as[OrNullBulk[Bulk], Option[A]]

  final def setbit(key: Key, offset: StringLength, bit: Bit): Single.Aux[Bit] =
    Protocol("SETBIT", key *: offset *: bit *: EmptyTuple).as[Num, Bit]

  final def setex[A: Show](key: Key, value: A, seconds: PosLong): Single.Aux[OK] =
    Protocol("SETEX", key *: seconds *: value *: EmptyTuple).as[Str, OK]

  final def setnx[A: Show](key: Key, value: A): Single.Aux[Boolean] = Protocol("SETNX", key *: value *: EmptyTuple).as[Num, Boolean]

  final def setrange[A: Show](key: Key, offset: RangeOffset, value: A): Single.Aux[NonNegInt] =
    Protocol("SETRANGE", key *: offset *: value *: EmptyTuple).as[Num, NonNegInt]

  final def strlen(key: Key): Single.Aux[NonNegInt] = Protocol("STRLEN", key *: EmptyTuple).as[Num, NonNegInt]

  /** The older name of GETRANGE, which servers still have */
  final def substr[A: Read[Bulk, _]](key: Key, start: Index, end: Index): Single.Aux[A] =
    Protocol("SUBSTR", key *: start *: end *: EmptyTuple).as[Bulk, A]
}

/** Redis 7.0 allowed NX and XX with GET */
trait StringSetGetFlagP extends Features {
  import StringP.{Flag, SetExpiry}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SET: GET NX XX")

  final def setget[A: Read[Bulk, _]](key: Key, value: Value, flag: Flag): Single.Aux[Option[A]] =
    Protocol("SET", key *: value.shown *: flag *: "GET" *: EmptyTuple).as[OrNullBulk[Bulk], Option[A]]
  final def setget[A: Read[Bulk, _]](key: Key, value: Value, expiry: SetExpiry, flag: Flag): Single.Aux[Option[A]] =
    Protocol("SET", key *: value.shown *: flag *: "GET" *: expiry.params *: EmptyTuple).as[OrNullBulk[Bulk], Option[A]]
}

/** SET with IFEQ: Valkey 8.1, Redis 8.4 */
trait StringSetIfEqP extends Features {
  import StringP.{IfEq, SetExpiry}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SET: IFEQ")

  final def set[A: Show](key: Key, value: A, ifEq: IfEq): Single.Aux[Option[OK]] =
    Protocol("SET", key *: value *: ifEq.params *: EmptyTuple).as[OrNullBulk[Str], Option[OK]]
  final def set[A: Show](key: Key, value: A, expiry: SetExpiry, ifEq: IfEq): Single.Aux[Option[OK]] =
    Protocol("SET", key *: value *: ifEq.params *: expiry.params *: EmptyTuple).as[OrNullBulk[Str], Option[OK]]
}

/** SET with IFNE, IFDEQ and IFDNE: Redis 8.4 */
trait StringSetCompareP extends Features {
  import StringP.{Compare, SetExpiry}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SET: IFDEQ IFDNE IFNE")

  final def set[A: Show](key: Key, value: A, compare: Compare): Single.Aux[Option[OK]] =
    Protocol("SET", key *: value *: compare.params *: EmptyTuple).as[OrNullBulk[Str], Option[OK]]
  final def set[A: Show](key: Key, value: A, expiry: SetExpiry, compare: Compare): Single.Aux[Option[OK]] =
    Protocol("SET", key *: value *: compare.params *: expiry.params *: EmptyTuple).as[OrNullBulk[Str], Option[OK]]
}

/** BITCOUNT and BITPOS with BYTE or BIT: Redis 7.0 */
trait StringBitUnitP extends Features {
  import StringP.{Bit, BitUnit}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("BITCOUNT: BIT BYTE", "BITPOS: BIT BYTE")

  final def bitcount(key: Key, start: Index, end: Index, unit: BitUnit): Single.Aux[NonNegInt] =
    Protocol("BITCOUNT", key *: start *: end *: unit *: EmptyTuple).as[Num, NonNegInt]

  final def bitpos(key: Key, bit: Bit, start: Index, end: Index, unit: BitUnit): Single.Aux[Option[NonNegInt]] =
    Protocol("BITPOS", key *: bit *: start *: end *: unit *: EmptyTuple).using(RESPRead.instance(Read.numMinusOneIsNone[NonNegInt]))
}

/** LCS: Redis 7.0 */
trait StringLcsP extends Features {
  import StringP.LcsMatches

  override private[laserdisc] def supported: List[String] = super.supported ++ List("LCS: IDX LEN MINMATCHLEN WITHMATCHLEN")

  /** The longest common substring of the values at the two keys */
  final def lcs[A: Read[Bulk, _]](key1: Key, key2: Key): Single.Aux[A] = Protocol("LCS", key1 :: key2 :: Nil).as[Bulk, A]

  /** The length of the longest common substring of the values at the two keys */
  final def lcslen(key1: Key, key2: Key): Single.Aux[NonNegInt] =
    Protocol("LCS", key1.value :: key2.value :: "LEN" :: Nil).as[Num, NonNegInt]

  /** Where the matches are in each value, with their lengths */
  final def lcsidx(key1: Key, key2: Key): Single.Aux[LcsMatches] =
    Protocol("LCS", key1.value :: key2.value :: "IDX" :: "WITHMATCHLEN" :: Nil).using(RESPRead.instance(StringP.lcsMatches))
  final def lcsidx(key1: Key, key2: Key, minMatchLength: PosInt): Single.Aux[LcsMatches] =
    Protocol("LCS", key1 *: key2 *: "IDX" *: "MINMATCHLEN" *: minMatchLength *: "WITHMATCHLEN" *: EmptyTuple)
      .using(RESPRead.instance(StringP.lcsMatches))
}

/** BITOP with DIFF, DIFF1, ANDOR and ONE: Redis 8.2 */
trait StringBitopExtendedP extends Features {
  import StringP.ExtendedBitwise

  override private[laserdisc] def supported: List[String] = super.supported ++ List("BITOP: ANDOR DIFF DIFF1 ONE")

  final def bitop(bitwise: ExtendedBitwise, keys: TwoOrMoreKeys, destinationKey: Key): Single.Aux[NonNegInt] =
    Protocol("BITOP", bitwise *: destinationKey *: keys.value *: EmptyTuple).as[Num, NonNegInt]
}

/** DIGEST and DELEX: Redis 8.4 */
trait StringDigestAndDelExP extends Features {
  import StringP.{Compare, IfEq}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("DELEX: IFDEQ IFDNE IFEQ IFNE", "DIGEST")

  /** A hash of the value at the key, which IFDEQ and IFDNE compare against */
  final def digest(key: Key): Single.Aux[Option[String]] = Protocol("DIGEST", key).as[OrNullBulk[Bulk], Option[String]]

  /** Deletes the key, like DEL with one key, and replies whether it did */
  final def delex(key: Key): Single.Aux[Boolean]                   = Protocol("DELEX", key).as[Num, Boolean]
  final def delex(key: Key, ifEq: IfEq): Single.Aux[Boolean]       = Protocol("DELEX", key *: ifEq.params *: EmptyTuple).as[Num, Boolean]
  final def delex(key: Key, compare: Compare): Single.Aux[Boolean] = Protocol("DELEX", key *: compare.params *: EmptyTuple).as[Num, Boolean]
}

/** DELIFEQ: Valkey 9.0 */
trait StringDelIfEqP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("DELIFEQ")

  /** Deletes the key only if its value is `value`, and replies whether it did */
  final def delifeq[A: Show](key: Key, value: A): Single.Aux[Boolean] = Protocol("DELIFEQ", key *: value *: EmptyTuple).as[Num, Boolean]
}

/** MSETEX: Redis 8.4, Valkey 9.1 */
trait StringMSetExP extends Features {
  import StringP.{Flag, SetExpiry}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("MSETEX: EX EXAT KEEPTTL NX PX PXAT XX")

  /** Sets every key, or none of them when NX or XX doesn't hold for all of them, and replies whether it did */
  final def msetex[A: Show](values: OneOrMore[(Key, A)]): Single.Aux[Boolean] =
    Protocol("MSETEX", values.value.length *: values.value *: EmptyTuple).as[Num, Boolean]
  final def msetex[A: Show](values: OneOrMore[(Key, A)], flag: Flag): Single.Aux[Boolean] =
    Protocol("MSETEX", values.value.length *: values.value *: flag *: EmptyTuple).as[Num, Boolean]
  final def msetex[A: Show](values: OneOrMore[(Key, A)], expiry: SetExpiry): Single.Aux[Boolean] =
    Protocol("MSETEX", values.value.length *: values.value *: expiry.params *: EmptyTuple).as[Num, Boolean]
  final def msetex[A: Show](values: OneOrMore[(Key, A)], flag: Flag, expiry: SetExpiry): Single.Aux[Boolean] =
    Protocol("MSETEX", values.value.length *: values.value *: flag *: expiry.params *: EmptyTuple).as[Num, Boolean]
}

/** INCREX: Redis 8.8 */
trait StringIncrExP extends Features {
  import StringP.{IncrExOptions, Incremented}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("INCREX: BYFLOAT BYINT ENX EX EXAT LBOUND PERSIST PX PXAT SATURATE UBOUND")

  private final val longs   = RESPRead.instance(StringP.incrementedLong)
  private final val doubles = RESPRead.instance(StringP.incrementedDouble)

  /** Adds 1, starting from 0 when the key doesn't exist */
  final def increx(key: Key): Single.Aux[Incremented[Long]]                  = Protocol("INCREX", key).using(longs)
  final def increx(key: Key, increment: Long): Single.Aux[Incremented[Long]] =
    Protocol("INCREX", key *: "BYINT" *: increment *: EmptyTuple).using(longs)
  final def increx(key: Key, increment: Long, options: IncrExOptions[Long]): Single.Aux[Incremented[Long]] =
    Protocol("INCREX", key *: "BYINT" *: increment *: options.params *: EmptyTuple).using(longs)
  final def increx(key: Key, increment: ValidDouble): Single.Aux[Incremented[Double]] =
    Protocol("INCREX", key *: "BYFLOAT" *: increment *: EmptyTuple).using(doubles)
  final def increx(key: Key, increment: ValidDouble, options: IncrExOptions[ValidDouble]): Single.Aux[Incremented[Double]] =
    Protocol("INCREX", key *: "BYFLOAT" *: increment *: options.params *: EmptyTuple).using(doubles)
}

trait StringP
    extends StringBaseP
    with StringExtP
    with StringSetGetFlagP
    with StringSetIfEqP
    with StringSetCompareP
    with StringBitUnitP
    with StringLcsP
    with StringBitopExtendedP
    with StringDigestAndDelExP
    with StringDelIfEqP
    with StringMSetExP
    with StringIncrExP
