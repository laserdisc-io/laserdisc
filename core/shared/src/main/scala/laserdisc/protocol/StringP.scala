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

    implicit val bitwiseShow: Show[Bitwise] = Show.instance {
      case `and` => "AND"
      case `or`  => "OR"
      case `xor` => "XOR"
    }
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
  import StringP.{Bit, Bitwise, Compare, Expiry, Flag, GetExpiry, IfEq, SetExpiry}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "APPEND",
      "BITCOUNT",
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
      "STRLEN"
    )

  private final val minusOneIsNone = RESPRead.instance(Read.numMinusOneIsNone[NonNegInt])

  object strings {
    final val bit     = Bit
    final val bitwise = Bitwise
    final val expiry  = Expiry
    final val flag    = Flag
    final val compare = Compare

    final def ifEq[A: Show](a: A): IfEq = IfEq(a)
  }

  final def append[A: Show](key: Key, value: A): Single.Aux[NonNegInt] = Protocol("APPEND", key *: value *: EmptyTuple).as[Num, NonNegInt]

  final def bitcount(key: Key): Single.Aux[NonNegInt]                           = Protocol("BITCOUNT", key).as[Num, NonNegInt]
  final def bitcount(key: Key, start: Index, end: Index): Single.Aux[NonNegInt] =
    Protocol("BITCOUNT", key *: start *: end *: EmptyTuple).as[Num, NonNegInt]

  // FIXME add BITFIELD

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

  final def get[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]] = Protocol("GET", key).asOpt[Bulk, A]

  final def getbit(key: Key, offset: PosLong): Single.Aux[Bit] = Protocol("GETBIT", key *: offset *: EmptyTuple).as[Num, Bit]

  final def getrange[A: Read[Bulk, _]](key: Key, start: Index, end: Index): Single.Aux[A] =
    Protocol("GETRANGE", key *: start *: end *: EmptyTuple).as[Bulk, A]

  final def getdel[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]] = Protocol("GETDEL", key).asOpt[Bulk, A]

  final def getex[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]]                    = Protocol("GETEX", key).asOpt[Bulk, A]
  final def getex[A: Read[Bulk, _]](key: Key, expiry: GetExpiry): Single.Aux[Option[A]] =
    Protocol("GETEX", key *: expiry.params *: EmptyTuple).asOpt[Bulk, A]

  final def getset[A: Read[Bulk, _]](key: Key, value: Value): Single.Aux[Option[A]] =
    Protocol("GETSET", key *: value.shown *: EmptyTuple).asOpt[Bulk, A]

  final def incr[A: Read[Num, _]](key: Key): Single.Aux[A] = Protocol("INCR", key).as[Num, A]

  // TODO verify ok to limit INCRBY to only positive values, REDIS happily accepts 0 and negatives
  final def incrby[A: Read[Num, _]](key: Key, increment: PosLong): Single.Aux[A] =
    Protocol("INCRBY", key *: increment *: EmptyTuple).as[Num, A]

  final def incrbyfloat(key: Key, increment: NonZeroDouble): Single.Aux[Double] =
    Protocol("INCRBYFLOAT", key *: increment *: EmptyTuple).as[Bulk, Double]

  final def mget[A: Read[Arr, _]](keys: OneOrMoreKeys): Single.Aux[A] = Protocol("MGET", keys.value).as[Arr, A]

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
    Protocol("SET", key *: value *: flag *: EmptyTuple).asOpt[Str, OK]
  final def set[A: Show](key: Key, value: A, expiry: SetExpiry, flag: Flag): Single.Aux[Option[OK]] =
    Protocol("SET", key *: value *: flag *: expiry.params *: EmptyTuple).asOpt[Str, OK]

  /** SET with GET, which replies with the previous value */
  final def setget[A: Read[Bulk, _]](key: Key, value: Value): Single.Aux[Option[A]] =
    Protocol("SET", key *: value.shown *: "GET" *: EmptyTuple).asOpt[Bulk, A]
  final def setget[A: Read[Bulk, _]](key: Key, value: Value, expiry: SetExpiry): Single.Aux[Option[A]] =
    Protocol("SET", key *: value.shown *: "GET" *: expiry.params *: EmptyTuple).asOpt[Bulk, A]

  final def setbit(key: Key, offset: StringLength, bit: Bit): Single.Aux[Bit] =
    Protocol("SETBIT", key *: offset *: bit *: EmptyTuple).as[Num, Bit]

  final def setex[A: Show](key: Key, value: A, seconds: PosLong): Single.Aux[OK] =
    Protocol("SETEX", key *: seconds *: value *: EmptyTuple).as[Str, OK]

  final def setnx[A: Show](key: Key, value: A): Single.Aux[Boolean] = Protocol("SETNX", key *: value *: EmptyTuple).as[Num, Boolean]

  final def setrange[A: Show](key: Key, offset: RangeOffset, value: A): Single.Aux[NonNegInt] =
    Protocol("SETRANGE", key *: offset *: value *: EmptyTuple).as[Num, NonNegInt]

  final def strlen(key: Key): Single.Aux[NonNegInt] = Protocol("STRLEN", key *: EmptyTuple).as[Num, NonNegInt]
}

/** Redis 7.0 allowed NX and XX with GET */
trait StringSetGetFlagP extends Features {
  import StringP.{Flag, SetExpiry}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SET: GET NX XX")

  final def setget[A: Read[Bulk, _]](key: Key, value: Value, flag: Flag): Single.Aux[Option[A]] =
    Protocol("SET", key *: value.shown *: flag *: "GET" *: EmptyTuple).asOpt[Bulk, A]
  final def setget[A: Read[Bulk, _]](key: Key, value: Value, expiry: SetExpiry, flag: Flag): Single.Aux[Option[A]] =
    Protocol("SET", key *: value.shown *: flag *: "GET" *: expiry.params *: EmptyTuple).asOpt[Bulk, A]
}

/** SET with IFEQ: Valkey 8.1, Redis 8.4 */
trait StringSetIfEqP extends Features {
  import StringP.{IfEq, SetExpiry}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SET: IFEQ")

  final def set[A: Show](key: Key, value: A, ifEq: IfEq): Single.Aux[Option[OK]] =
    Protocol("SET", key *: value *: ifEq.params *: EmptyTuple).asOpt[Str, OK]
  final def set[A: Show](key: Key, value: A, expiry: SetExpiry, ifEq: IfEq): Single.Aux[Option[OK]] =
    Protocol("SET", key *: value *: ifEq.params *: expiry.params *: EmptyTuple).asOpt[Str, OK]
}

/** SET with IFNE, IFDEQ and IFDNE: Redis 8.4 */
trait StringSetCompareP extends Features {
  import StringP.{Compare, SetExpiry}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SET: IFDEQ IFDNE IFNE")

  final def set[A: Show](key: Key, value: A, compare: Compare): Single.Aux[Option[OK]] =
    Protocol("SET", key *: value *: compare.params *: EmptyTuple).asOpt[Str, OK]
  final def set[A: Show](key: Key, value: A, expiry: SetExpiry, compare: Compare): Single.Aux[Option[OK]] =
    Protocol("SET", key *: value *: compare.params *: expiry.params *: EmptyTuple).asOpt[Str, OK]
}

trait StringP extends StringBaseP with StringExtP with StringSetGetFlagP with StringSetIfEqP with StringSetCompareP
