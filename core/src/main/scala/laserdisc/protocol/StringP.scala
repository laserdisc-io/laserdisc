package laserdisc
package protocol

object StringP {
  sealed trait Bit
  final object Bit {
    final object set   extends Bit
    final object unset extends Bit

    implicit val bitShow: Show[Bit] = Show.instance {
      case `set`   => "1"
      case `unset` => "0"
    }
    implicit val num2BitRead: Num ==> Bit = Read[Num, Boolean].map(flag => if (flag) set else unset)
  }

  sealed trait Bitwise
  final object Bitwise {
    final case object and extends Bitwise
    final case object or  extends Bitwise
    final case object xor extends Bitwise

    implicit val bitwiseShow: Show[Bitwise] = Show.instance {
      case `and` => "AND"
      case `or`  => "OR"
      case `xor` => "XOR"
    }
  }

  sealed trait Expiry { def value: PosLong; def unit: Expiry.Unit }
  final object Expiry {
    sealed trait Unit
    final object Unit {
      final case object milliseconds extends Unit
      final case object seconds      extends Unit

      implicit val expiryUnitShow: Show[Unit] = Show.instance {
        case `milliseconds` => "PX"
        case `seconds`      => "EX"
      }
    }

    def millis(v: PosLong): Expiry = new Expiry {
      override final val value: PosLong = v
      override final val unit: Unit     = Unit.milliseconds
    }
    def seconds(v: PosLong): Expiry = new Expiry {
      override final val value: PosLong = v
      override final val unit: Unit     = Unit.seconds
    }
  }

  sealed trait Flag
  final object Flag {
    final object nx extends Flag
    final object xx extends Flag

    implicit val flagShow: Show[Flag] = Show.instance {
      case `nx` => "NX"
      case `xx` => "XX"
    }
  }

  final class PartiallyAppliedGetSet[A](private val dummy: Boolean) extends AnyVal {
    import shapeless._
    def apply[B: Show](key: Key, value: B)(implicit ev: Bulk ==> A): Protocol.Aux[Option[A]] =
      Protocol("GETSET", key :: value :: HNil).asC[Bulk :+: NullBulk :+: CNil, Option[A]]
  }
}

trait StringBaseP {
  import StringP.{Bit, Bitwise, Expiry, Flag, PartiallyAppliedGetSet}
  import shapeless._
  import shapeless.labelled.FieldType
  import shapeless.nat._
  import shapeless.ops.hlist.Length
  import shapeless.ops.nat.GTEq.>=

  private[this] final val minusOneIsNone = RESPRead.instance(Read.numMinusOneIsNone[NonNegInt])

  final object strings {
    final val bit     = Bit
    final val bitwise = Bitwise
    final val expiry  = Expiry
    final val flag    = Flag
  }

  final def append[A: Show](key: Key, value: A): Protocol.Aux[NonNegInt] = Protocol("APPEND", key :: value :: HNil).as[Num, NonNegInt]

  final def bitcount(key: Key): Protocol.Aux[NonNegInt] = Protocol("BITCOUNT", key).as[Num, NonNegInt]
  final def bitcount(key: Key, start: Index, end: Index): Protocol.Aux[NonNegInt] =
    Protocol("BITCOUNT", key :: start :: end :: HNil).as[Num, NonNegInt]

  //FIXME add BITFIELD

  final def bitop(bitwise: Bitwise, keys: TwoOrMoreKeys, destinationKey: Key): Protocol.Aux[NonNegInt] =
    Protocol("BITOP", bitwise :: destinationKey :: keys.value :: HNil).as[Num, NonNegInt]

  final def bitopnot(key: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    Protocol("BITOP", "NOT" :: destinationKey.value :: key.value :: Nil).as[Num, NonNegInt]

  final def bitpos(key: Key, bit: Bit): Protocol.Aux[Option[NonNegInt]] = Protocol("BITPOS", key :: bit :: HNil).using(minusOneIsNone)
  final def bitpos(key: Key, bit: Bit, start: Index): Protocol.Aux[Option[NonNegInt]] =
    Protocol("BITPOS", key :: bit :: start :: HNil).using(minusOneIsNone)
  final def bitpos(key: Key, bit: Bit, start: Index, end: Index): Protocol.Aux[Option[NonNegInt]] =
    Protocol("BITPOS", key :: bit :: start :: end :: HNil).using(minusOneIsNone)

  final def decr[A: Num ==> *](key: Key): Protocol.Aux[A] = Protocol("DECR", key).as[Num, A]

  //TODO verify ok to limit DECRBY to only positive values, REDIS happily accepts 0 and negatives and x + (-decrement)
  final def decrby[A: Num ==> *](key: Key, decrement: PosLong): Protocol.Aux[A] = Protocol("DECRBY", key :: decrement :: HNil).as[Num, A]

  final def get[A: Bulk ==> *](key: Key): Protocol.Aux[Option[A]] = Protocol("GET", key).opt[GenBulk].as[A]

  final def getbit(key: Key, offset: PosLong): Protocol.Aux[Bit] = Protocol("GETBIT", key :: offset :: HNil).as[Num, Bit]

  final def getrange[A: Bulk ==> *](key: Key, start: Index, end: Index): Protocol.Aux[A] =
    Protocol("GETRANGE", key :: start :: end :: HNil).as[Bulk, A]

  final def getset[A]: PartiallyAppliedGetSet[A] = new PartiallyAppliedGetSet[A](false)

  final def incr[A: Num ==> *](key: Key): Protocol.Aux[A] = Protocol("INCR", key).as[Num, A]

  //TODO verify ok to limit INCRBY to only positive values, REDIS happily accepts 0 and negatives
  final def incrby[A: Num ==> *](key: Key, increment: PosLong): Protocol.Aux[A] = Protocol("INCRBY", key :: increment :: HNil).as[Num, A]

  final def incrbyfloat(key: Key, increment: NonZeroDouble): Protocol.Aux[Double] =
    Protocol("INCRBYFLOAT", key :: increment :: HNil).as[Bulk, Double]

  final def mget[A: Arr ==> *](keys: OneOrMoreKeys): Protocol.Aux[A] = Protocol("MGET", keys.value).as[Arr, A]

  final def mset[L <: HList: RESPParamWrite: LUBConstraint[*, (Key, _)], N <: Nat](l: L)(
      implicit ev0: Length.Aux[L, N],
      ev1: N >= _1
  ): Protocol.Aux[OK] = Protocol("MSET", l).as[Str, OK]
  final def mset[P <: Product, L <: HList, N <: Nat](product: P)(
      implicit gen: LabelledGeneric.Aux[P, L],
      ev0: Length.Aux[L, N],
      ev1: N >= _1,
      ev2: LUBConstraint[L, FieldType[_, _]],
      ev3: RESPParamWrite[L]
  ): Protocol.Aux[OK] = Protocol("MSET", gen.to(product)).as[Str, OK]

  final def mset[A: Show](values: OneOrMore[(Key, A)]): Protocol.Aux[OK] = Protocol("MSET", values.value).as[Str, OK]

  final def msetnx[L <: HList: RESPParamWrite: LUBConstraint[*, (Key, _)], N <: Nat](l: L)(
      implicit ev0: Length.Aux[L, N],
      ev1: N >= _1
  ): Protocol.Aux[Boolean] = Protocol("MSETNX", l).as[Num, Boolean]

  final def msetnx[P <: Product, L <: HList, N <: Nat](product: P)(
      implicit gen: LabelledGeneric.Aux[P, L],
      ev0: Length.Aux[L, N],
      ev1: N >= _1,
      ev2: LUBConstraint[L, FieldType[_, _]],
      ev3: RESPParamWrite[L]
  ): Protocol.Aux[Boolean] = Protocol("MSETNX", gen.to(product)).as[Num, Boolean]
  final def msetnx[A: Show](values: OneOrMore[(Key, A)]): Protocol.Aux[Boolean] =
    Protocol("MSETNX", values.value).as[Num, Boolean]

  final def psetex[A: Show](key: Key, milliseconds: PosLong, value: A): Protocol.Aux[OK] =
    Protocol("PSETEX", key :: milliseconds :: value :: HNil).as[Str, OK]

  final def set[A: Show](key: Key, value: A): Protocol.Aux[OK] = Protocol("SET", key :: value :: HNil).as[Str, OK]
  final def set[A: Show](key: Key, value: A, expiry: Expiry): Protocol.Aux[OK] =
    Protocol("SET", key :: value :: expiry.unit :: expiry.value :: HNil).as[Str, OK]
  final def set[A: Show](key: Key, value: A, flag: Flag): Protocol.Aux[Option[OK]] =
    Protocol("SET", key :: value :: flag :: HNil).asC[Str :+: NullBulk :+: CNil, Option[OK]]
  final def set[A: Show](key: Key, value: A, expiry: Expiry, flag: Flag): Protocol.Aux[Option[OK]] =
    Protocol("SET", key :: value :: flag :: expiry.unit :: expiry.value :: HNil).asC[Str :+: NullBulk :+: CNil, Option[OK]]

  final def setbit(key: Key, offset: StringLength, bit: Bit): Protocol.Aux[Bit] =
    Protocol("SETBIT", key :: offset :: bit :: HNil).as[Num, Bit]

  final def setex[A: Show](key: Key, value: A, seconds: PosLong): Protocol.Aux[OK] =
    Protocol("SETEX", key :: seconds :: value :: HNil).as[Str, OK]

  final def setnx[A: Show](key: Key, value: A): Protocol.Aux[Boolean] = Protocol("SETNX", key :: value :: HNil).as[Num, Boolean]

  final def setrange[A: Show](key: Key, offset: RangeOffset, value: A): Protocol.Aux[NonNegInt] =
    Protocol("SETRANGE", key :: offset :: value :: HNil).as[Num, NonNegInt]

  final def strlen(key: Key): Protocol.Aux[NonNegInt] = Protocol("STRLEN", key :: HNil).as[Num, NonNegInt]
}

trait StringP extends StringBaseP with StringExtP
