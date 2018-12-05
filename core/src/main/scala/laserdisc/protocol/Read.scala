package laserdisc
package protocol

import shapeless._
import shapeless.labelled._

import scala.annotation.implicitNotFound

@implicitNotFound(
  """Implicit not found Read[${A}, ${B}].

Try writing your own, for example:

implicit final val myRead: Read[${A}, ${B}] = new Read[${A}, ${B}] {
  override final def read(a: ${A}): Option[${B}] = ???
}

Note 1: you can use the factory methods Read.instance / Read.instancePF instead of creating it manually as shown above
Note 2: make sure to inspect the combinators as you may be able to leverage some other Read instance
"""
) trait Read[A, B] { self =>

  def read(a: A): Option[B]

  final def map[C](f: B => C): Read[A, C] = Read.instance(read(_).map(f))

  final def flatMap[C](f: B => Read[A, C]): Read[A, C] = Read.instance(a => read(a).flatMap(f(_).read(a)))

  final def contramap[C](f: C => A): Read[C, B] = Read.instance(read _ compose f)

  final def orElse[C](other: Read[A, C]): Read[A, B | C] = Read.instancePF {
    case `self`(b)  => Left(b)
    case `other`(c) => Right(c)
  }

  final def unapply(a: A): Option[B] = read(a)
}

object Read extends LowPriorityReadInstances {
  @inline final def apply[A, B](implicit instance: A ==> B): A ==> B = instance

  final def instance[A, B](f: A => Option[B]): A ==> B = new Read[A, B] {
    override def read(a: A): Option[B] = f(a)
  }
  final def instancePF[A, B](pf: PartialFunction[A, B]): A ==> B = instance(pf.lift)

  final def lift2OptionWhen[A, B](cond: A => Boolean)(
      implicit R: A ==> B
  ): (A :+: CNil) ==> Option[B] = Read.instancePF {
    case Inl(i @ R(b)) if !cond(i) => Some(b)
    case Inl(_)                    => None
  }

  final def integerMinusOneIsNone[A](
      implicit ev: Integer ==> A
  ): (Integer :+: CNil) ==> Option[A] = lift2OptionWhen(_.value == -1L)
  final def integerZeroIsNone[A](
      implicit ev: Integer ==> A
  ): (Integer :+: CNil) ==> Option[A] = lift2OptionWhen(_.value == 0L)

  implicit final def identity[A]: A ==> A = instance(Some(_))
}

trait LowPriorityReadInstances extends LowerPriorityReadInstances {
  implicit final val simpleString2StringRead: SimpleString ==> String = Read.instance {
    case SimpleString(s) => Some(s)
  }
  implicit final val simpleString2OKRead: SimpleString ==> OK = Read.instancePF {
    case SimpleString("OK") => OK
  }
  implicit final val simpleString2KeyRead: SimpleString ==> Key = Read.instancePF {
    case SimpleString(Key(s)) => s
  }

  implicit final val integer2BooleanRead: Integer ==> Boolean = Read.instancePF {
    case Integer(0L) => false
    case Integer(1L) => true
  }
  implicit final val integer2IntRead: Integer ==> Int = Read.instancePF { //TODO: maybe useless
    case Integer(ToInt(i)) => i
  }
  implicit final val integer2LongRead: Integer ==> Long = Read.instancePF {
    case Integer(l) => l
  }
  implicit final val integer2NonNegIntRead: Integer ==> NonNegInt = Read.instancePF {
    case Integer(ToInt(NonNegInt(i))) => i
  }
  implicit final val integer2NonNegLongRead: Integer ==> NonNegLong = Read.instancePF {
    case Integer(NonNegLong(l)) => l
  }
  implicit final val integer2NonZeroIntRead: Integer ==> NonZeroInt = Read.instancePF {
    case Integer(ToInt(NonZeroInt(i))) => i
  }
  implicit final val integer2NonZeroLongRead: Integer ==> NonZeroLong = Read.instancePF {
    case Integer(NonZeroLong(l)) => l
  }
  implicit final val integer2PosIntRead: Integer ==> PosInt = Read.instancePF {
    case Integer(ToInt(PosInt(i))) => i
  }
  implicit final val integer2PosLongRead: Integer ==> PosLong = Read.instancePF {
    case Integer(PosLong(l)) => l
  }

  implicit final val nonNullBulkString2StringRead: NonNullBulkString ==> String = Read.instance {
    case NonNullBulkString(s) => Some(s)
  }
  implicit final val nonNullBulkString2DoubleRead: NonNullBulkString ==> Double = Read.instancePF {
    case NonNullBulkString(ToDouble(d)) => d
  }
  implicit final val nonNullBulkString2IntRead: NonNullBulkString ==> Int = Read.instancePF { //TODO: maybe useless
    case NonNullBulkString(ToInt(i)) => i
  }
  implicit final val nonNullBulkString2LongRead: NonNullBulkString ==> Long = Read.instancePF {
    case NonNullBulkString(ToLong(l)) => l
  }
  implicit final val nonNullBulkString2ValidDoubleRead: NonNullBulkString ==> ValidDouble = Read.instancePF {
    case NonNullBulkString(ToDouble(ValidDouble(d))) => d
  }
  implicit final val nonNullBulkString2NonNegIntRead: NonNullBulkString ==> NonNegInt = Read.instancePF {
    case NonNullBulkString(ToInt(NonNegInt(i))) => i
  }
  implicit final val nonNullBulkString2NonNegLongRead: NonNullBulkString ==> NonNegLong = Read.instancePF {
    case NonNullBulkString(ToLong(NonNegLong(l))) => l
  }
  implicit final val nonNullBulkString2NonZeroDoubleRead: NonNullBulkString ==> NonZeroDouble = Read.instancePF {
    case NonNullBulkString(ToDouble(NonZeroDouble(d))) => d
  }
  implicit final val nonNullBulkString2NonZeroIntRead: NonNullBulkString ==> NonZeroInt = Read.instancePF {
    case NonNullBulkString(ToInt(NonZeroInt(i))) => i
  }
  implicit final val nonNullBulkString2NonZeroLongRead: NonNullBulkString ==> NonZeroLong = Read.instancePF {
    case NonNullBulkString(ToLong(NonZeroLong(l))) => l
  }
  implicit final val nonNullBulkString2PosIntRead: NonNullBulkString ==> PosInt = Read.instancePF {
    case NonNullBulkString(ToInt(PosInt(i))) => i
  }
  implicit final val nonNullBulkString2PosLongRead: NonNullBulkString ==> PosLong = Read.instancePF {
    case NonNullBulkString(ToLong(PosLong(l))) => l
  }
  implicit final val nonNullBulkString2ConnectionNameRead: NonNullBulkString ==> ConnectionName = Read.instancePF {
    case NonNullBulkString(ConnectionName(connectionName)) => connectionName
  }
  implicit final val nonNullBulkString2KeyRead: NonNullBulkString ==> Key = Read.instancePF {
    case NonNullBulkString(Key(s)) => s
  }

  implicit final def nonNilArray2Seq[A](
      implicit R: NonNullBulkString ==> A
  ): NonNilArray ==> Seq[A] = Read.instance {
    case NonNilArray(vector) =>
      val (vectorLength, (as, asLength)) = vector.foldLeft(0 -> (List.empty[A] -> 0)) {
        case ((vl, (as0, asl)), R(a)) => (vl + 1) -> ((a :: as0) -> (asl + 1))
        case ((vl, acc), _)           => (vl + 1) -> acc
      }
      if (vectorLength == asLength) Some(as.reverse) else None
    case _ => None
  }

  implicit final def nonNilArray2Tuple2Read[A, B](
      implicit RA: NonNullBulkString ==> A,
      RB: NonNullBulkString ==> B
  ): NonNilArray ==> (A, B) = Read.instancePF {
    case NonNilArray(RA(key) +: RB(value) +: Seq()) => key -> value
  }

  implicit final def nonNilArray2Tuple2Seq[A, B](
      implicit RA: NonNullBulkString ==> A,
      RB: NonNullBulkString ==> B
  ): NonNilArray ==> Seq[(A, B)] = Read.instance {
    case NonNilArray(vector) =>
      val (vectorLength, (kvs, kvsLength)) =
        vector.grouped(2).foldLeft(0 -> (List.empty[(A, B)] -> 0)) {
          case ((vl, (kv, kvl)), RA(a) +: RB(b) +: Seq()) => (vl + 1) -> (((a -> b) :: kv) -> (kvl + 1))
          case ((vl, acc), _)                             => (vl + 1) -> acc
        }
      if (vectorLength == kvsLength) Some(kvs.reverse) else None
    case _ => None
  }

  implicit final def nonNilArray2Scan[A](
      implicit R: NonNilArray ==> Seq[A]
  ): NonNilArray ==> Scan[A] = Read.instance {
    case NonNilArray(NonNullBulkString(ToLong(NonNegLong(cursor))) +: NilArray +: Seq()) =>
      Some(Scan(cursor, None))
    case NonNilArray(NonNullBulkString(ToLong(NonNegLong(cursor))) +: R(as) +: Seq()) =>
      Some(Scan(cursor, Some(as)))
    case _ => None
  }

  implicit final val nonNilArray2Map: NonNilArray ==> Map[Key, String] = Read.instance {
    case NonNilArray(vector) =>
      val (vectorLength, (kvs, kvsLength)) =
        vector.grouped(2).foldLeft(0 -> (Map.empty[Key, String] -> 0)) {
          case ((vl, (kv, kvl)), NonNullBulkString(Key(k)) +: NonNullBulkString(v) +: Seq()) =>
            (vl + 1) -> ((kv + (k -> v)) -> (kvl + 1))
          case ((vl, acc), _) => (vl + 1) -> acc
        }
      if (vectorLength == kvsLength) Some(kvs) else None
    case _ => None
  }
  implicit final val nonNilArray2ScanKV: NonNilArray ==> ScanKV = Read.instance {
    case NonNilArray(NonNullBulkString(ToLong(NonNegLong(cursor))) +: NilArray +: Seq()) =>
      Some(ScanKV(cursor, None))
    case NonNilArray(NonNullBulkString(ToLong(NonNegLong(cursor))) +: NonNilArray(vector) +: Seq()) =>
      val (vectorLength, (kvs, kvsLength)) =
        vector.grouped(2).foldLeft(0 -> (List.empty[KV[String]] -> 0)) {
          case ((vl, (kv, kvl)), NonNullBulkString(Key(k)) +: NonNullBulkString(v) +: Seq()) =>
            (vl + 1) -> ((KV(k, v) :: kv) -> (kvl + 1))
          case ((vl, acc), _) => (vl + 1) -> acc
        }
      if (vectorLength == kvsLength) Some(ScanKV(cursor, Some(kvs.reverse))) else None
    case _ => None
  }
  implicit final val nonNilArray2TimeRead: NonNilArray ==> Time = Read.instancePF {
    case NonNilArray(
        NonNullBulkString(ToLong(NonNegLong(ts))) +: NonNullBulkString(ToLong(NonNegLong(em))) +: Seq()
        ) =>
      Time(ts, em)
  }

  implicit final def nonNilArrayToProduct[P <: Product, L <: HList](
      implicit G: LabelledGeneric.Aux[P, L],
      R: NonNilArray ==> L
  ): NonNilArray ==> P = Read.instancePF {
    case R(l) => G.from(l)
  }

  implicit final def nonNilArray2LabelledHCons[HK <: Symbol, HV, T <: HList](
      implicit HK: Witness.Aux[HK],
      RHV: NonNullBulkString ==> HV,
      RT: NonNilArray ==> T
  ): NonNilArray ==> (FieldType[HK, HV] :: T) = Read.instance {
    case NonNilArray(NonNullBulkString(HK.value.`name`) +: RHV(hv) +: rest) =>
      RT.read(RESP.arr(rest)).map(t => field[HK](hv) :: t)
    case _ => None
  }

  implicit final def nonNilArray2HCons[H, T <: HList](
      implicit
      ev: H <:!< FieldType[_, _],
      RH: NonNullBulkString ==> H,
      RT: NonNilArray ==> T
  ): NonNilArray ==> (H :: T) = Read.instance {
    case NonNilArray(RH(h) +: rest) => RT.read(RESP.arr(rest)).map(h :: _)
  }

  implicit final val nonNilArray2HNil: NonNilArray ==> HNil = Read.instancePF {
    case NonNilArray(Seq()) => HNil
  }
}

sealed trait LowerPriorityReadInstances {
  final type ==>[A, B] = Read[A, B]

  implicit final def liftNonNullBulkString2Option[A, B](
      implicit R: A ==> B
  ): (NullBulkString :+: A :+: CNil) ==> Option[B] = Read.instancePF {
    case Inl(_)         => None
    case Inr(Inl(R(b))) => Some(b)
  }
  implicit final def liftNonNilArray2Option[A, B](
      implicit R: A ==> B
  ): (NilArray :+: A :+: CNil) ==> Option[B] = Read.instancePF {
    case Inl(_)         => None
    case Inr(Inl(R(b))) => Some(b)
  }

  implicit final def liftSimpleToSum[A, B](
      implicit R: A ==> B,
      ev: A <:!< Coproduct
  ): (A :+: CNil) ==> B = Read.instancePF {
    case Inl(R(b)) => b
  }
}
