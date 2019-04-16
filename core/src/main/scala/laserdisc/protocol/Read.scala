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

object Read extends ReadInstances0 {
  @inline final def apply[A, B](implicit instance: Read[A, B]): Read[A, B] = instance

  final def instance[A, B](f: A => Option[B]): Read[A, B] = new Read[A, B] {
    override def read(a: A): Option[B] = f(a)
  }
  final def instancePF[A, B](pf: PartialFunction[A, B]): Read[A, B] = instance(pf.lift)

  final def lift2OptionWhen[A, B](cond: A => Boolean)(
      implicit R: Read[A, B]
  ): Read[A :+: CNil, Option[B]] = Read.instancePF {
    case Inl(i @ R(b)) if !cond(i) => Some(b)
    case Inl(_)                    => None
  }

  final def integerMinusOneIsNone[A: Read[Integer, ?]]: Read[Integer :+: CNil, Option[A]] =
    lift2OptionWhen(_.value == -1L)

  final def integerZeroIsNone[A: Read[Integer, ?]]: Read[Integer :+: CNil, Option[A]] =
    lift2OptionWhen(_.value == 0L)
}

trait ReadInstances0 extends ReadInstances1 {
  implicit final def identity[A]: Read[A, A] = Read.instance(Some(_))
}

trait ReadInstances1 extends ReadInstances2 {
  implicit final val simpleString2StringRead: Read[SimpleString, String] = Read.instance {
    case SimpleString(s) => Some(s)
  }
  implicit final val simpleString2OKRead: Read[SimpleString, OK] = Read.instancePF {
    case SimpleString("OK") => OK
  }
  implicit final val simpleString2KeyRead: Read[SimpleString, Key] = Read.instancePF {
    case SimpleString(Key(s)) => s
  }

  implicit final val integer2BooleanRead: Read[Integer, Boolean] = Read.instancePF {
    case Integer(0L) => false
    case Integer(1L) => true
  }
  implicit final val integer2IntRead: Read[Integer, Int] = Read.instancePF { //TODO: maybe useless
    case Integer(ToInt(i)) => i
  }
  implicit final val integer2LongRead: Read[Integer, Long] = Read.instancePF {
    case Integer(l) => l
  }
  implicit final val integer2NonNegIntRead: Read[Integer, NonNegInt] = Read.instancePF {
    case Integer(ToInt(NonNegInt(i))) => i
  }
  implicit final val integer2NonNegLongRead: Read[Integer, NonNegLong] = Read.instancePF {
    case Integer(NonNegLong(l)) => l
  }
  implicit final val integer2NonZeroIntRead: Read[Integer, NonZeroInt] = Read.instancePF {
    case Integer(ToInt(NonZeroInt(i))) => i
  }
  implicit final val integer2NonZeroLongRead: Read[Integer, NonZeroLong] = Read.instancePF {
    case Integer(NonZeroLong(l)) => l
  }
  implicit final val integer2PosIntRead: Read[Integer, PosInt] = Read.instancePF {
    case Integer(ToInt(PosInt(i))) => i
  }
  implicit final val integer2PosLongRead: Read[Integer, PosLong] = Read.instancePF {
    case Integer(PosLong(l)) => l
  }

  implicit final val nonNullBulkString2StringRead: Read[NonNullBulkString, String] = Read.instance {
    case NonNullBulkString(s) => Some(s)
  }
  implicit final val nonNullBulkString2DoubleRead: Read[NonNullBulkString, Double] = Read.instancePF {
    case NonNullBulkString(ToDouble(d)) => d
  }
  implicit final val nonNullBulkString2IntRead: Read[NonNullBulkString, Int] = Read.instancePF { //TODO: maybe useless
    case NonNullBulkString(ToInt(i)) => i
  }
  implicit final val nonNullBulkString2LongRead: Read[NonNullBulkString, Long] = Read.instancePF {
    case NonNullBulkString(ToLong(l)) => l
  }
  implicit final val nonNullBulkString2ValidDoubleRead: Read[NonNullBulkString, ValidDouble] = Read.instancePF {
    case NonNullBulkString(ToDouble(ValidDouble(d))) => d
  }
  implicit final val nonNullBulkString2NonNegIntRead: Read[NonNullBulkString, NonNegInt] = Read.instancePF {
    case NonNullBulkString(ToInt(NonNegInt(i))) => i
  }
  implicit final val nonNullBulkString2NonNegLongRead: Read[NonNullBulkString, NonNegLong] = Read.instancePF {
    case NonNullBulkString(ToLong(NonNegLong(l))) => l
  }
  implicit final val nonNullBulkString2NonNegDoubleRead: Read[NonNullBulkString, NonNegDouble] = Read.instancePF {
    case NonNullBulkString(ToDouble(NonNegDouble(d))) => d
  }
  implicit final val nonNullBulkString2NonZeroDoubleRead: Read[NonNullBulkString, NonZeroDouble] = Read.instancePF {
    case NonNullBulkString(ToDouble(NonZeroDouble(d))) => d
  }
  implicit final val nonNullBulkString2NonZeroIntRead: Read[NonNullBulkString, NonZeroInt] = Read.instancePF {
    case NonNullBulkString(ToInt(NonZeroInt(i))) => i
  }
  implicit final val nonNullBulkString2NonZeroLongRead: Read[NonNullBulkString, NonZeroLong] = Read.instancePF {
    case NonNullBulkString(ToLong(NonZeroLong(l))) => l
  }
  implicit final val nonNullBulkString2PosIntRead: Read[NonNullBulkString, PosInt] = Read.instancePF {
    case NonNullBulkString(ToInt(PosInt(i))) => i
  }
  implicit final val nonNullBulkString2PosLongRead: Read[NonNullBulkString, PosLong] = Read.instancePF {
    case NonNullBulkString(ToLong(PosLong(l))) => l
  }
  implicit final val nonNullBulkString2ConnectionNameRead: Read[NonNullBulkString, ConnectionName] = Read.instancePF {
    case NonNullBulkString(ConnectionName(connectionName)) => connectionName
  }
  implicit final val nonNullBulkString2KeyRead: Read[NonNullBulkString, Key] = Read.instancePF {
    case NonNullBulkString(Key(key)) => key
  }
  implicit final val nonNullBulkString2GeoHashRead: Read[NonNullBulkString, GeoHash] = Read.instancePF {
    case NonNullBulkString(GeoHash(hash)) => hash
  }

  implicit final def nonNilArrayOfBulk2Seq[A](
      implicit R: Read[NonNullBulkString, A]
  ): Read[NonNilArray, Seq[A]] = Read.instance {
    case NonNilArray(vector) =>
      val (vectorLength, (as, asLength)) = vector.foldLeft(0 -> (List.empty[A] -> 0)) {
        case ((vl, (as0, asl)), R(a)) => (vl + 1) -> ((a :: as0) -> (asl + 1))
        case ((vl, acc), _)           => (vl + 1) -> acc
      }
      if (vectorLength == asLength) Some(as.reverse) else None
    case _ => None
  }

  implicit final def nonNilArrayOfBulk2OptionSeq[A](
      implicit R: Read[NonNullBulkString, A]
  ): Read[NonNilArray, Seq[Option[A]]] = Read.instance {
    case NonNilArray(vector) =>
      val (vectorLength, (as, asLength)) = vector.foldLeft(0 -> (List.empty[Option[A]] -> 0)) {
        case ((vl, (as0, asl)), RESP.`nullBulk`) => (vl + 1) -> ((None :: as0) -> (asl + 1))
        case ((vl, (as0, asl)), R(a))            => (vl + 1) -> ((Some(a) :: as0) -> (asl + 1))
        case ((vl, acc), _)                      => (vl + 1) -> acc
      }
      if (vectorLength == asLength) Some(as.reverse) else None
    case _ => None
  }

  implicit final def nonNilArrayOfArray2OptionSeq[A](
      implicit R: Read[NonNilArray, A]
  ): Read[NonNilArray, Seq[Option[A]]] = Read.instance {
    case NonNilArray(vector) =>
      val (vectorLength, (as, asLength)) = vector.foldLeft(0 -> (List.empty[Option[A]] -> 0)) {
        case ((vl, (as0, asl)), RESP.`nilArray`) => (vl + 1) -> ((None :: as0) -> (asl + 1))
        case ((vl, (as0, asl)), R(a))            => (vl + 1) -> ((Some(a) :: as0) -> (asl + 1))
        case ((vl, acc), _)                      => (vl + 1) -> acc
      }
      if (vectorLength == asLength) Some(as.reverse) else None
    case _ => None
  }

  implicit final def nonNilArray2Tuple2Read[A, B](
      implicit RA: Read[NonNullBulkString, A],
      RB: Read[NonNullBulkString, B]
  ): Read[NonNilArray, (A, B)] = Read.instancePF {
    case NonNilArray(RA(key) +: RB(value) +: Seq()) => key -> value
  }
//
//  implicit final def nonNilArray2Tuple3Read[A, B, C](
//      implicit RA: Read[NonNullBulkString, A],
//      RB: Read[NonNullBulkString, B]
//  ): Read[NonNilArray, (A, B)] = Read.instancePF {
//    case NonNilArray(RA(key) +: RB(value) +: Seq()) => key -> value
//  }
//
//  implicit final def nonNilArray2Tuple4Read[A, B, C, D](
//      implicit RA: Read[NonNullBulkString, A],
//      RB: Read[NonNullBulkString, B]
//  ): Read[NonNilArray, (A, B)] = Read.instancePF {
//    case NonNilArray(RA(key) +: RB(value) +: Seq()) => key -> value
//  }

  implicit final def nonNilArray2Tuple2Seq[A, B](
      implicit RA: Read[NonNullBulkString, A],
      RB: Read[NonNullBulkString, B]
  ): Read[NonNilArray, Seq[(A, B)]] = Read.instance {
    case NonNilArray(vector) =>
      val (vectorLength, (kvs, kvsLength)) =
        vector.grouped(2).foldLeft(0 -> (List.empty[(A, B)] -> 0)) {
          case ((vl, (kv, kvl)), RA(a) +: RB(b) +: Seq()) => (vl + 1) -> (((a -> b) :: kv) -> (kvl + 1))
          case ((vl, acc), _)                             => (vl + 1) -> acc
        }
      if (vectorLength == kvsLength) Some(kvs.reverse) else None
    case _ => None
  }

  implicit final def nonNilArray2KV[A](
      implicit R: Read[NonNullBulkString, A]
  ): Read[NonNilArray, KV[A]] = Read.instancePF {
    case NonNilArray(NonNullBulkString(Key(key)) +: R(a) +: Seq()) => KV(key, a)
  }

  implicit final def nonNilArray2Scan[A](
      implicit R: Read[NonNilArray, Seq[A]]
  ): Read[NonNilArray, Scan[A]] = Read.instance {
    case NonNilArray(NonNullBulkString(ToLong(NonNegLong(cursor))) +: NilArray +: Seq()) =>
      Some(Scan(cursor, None))
    case NonNilArray(NonNullBulkString(ToLong(NonNegLong(cursor))) +: R(as) +: Seq()) =>
      Some(Scan(cursor, Some(as)))
    case _ => None
  }

  implicit final val nonNilArray2Coordinates: Read[NonNilArray, Coordinates] = Read.instancePF {
    case NonNilArray(
        NonNullBulkString(ToDouble(Longitude(long))) +: NonNullBulkString(ToDouble(Latitude(lat))) +: Seq()
    ) =>
      Coordinates(lat, long)
  }
  implicit final val nonNilArray2Map: Read[NonNilArray, Map[Key, String]] = Read.instance {
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
  implicit final val nonNilArray2ScanKV: Read[NonNilArray, ScanKV] = Read.instance {
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
  implicit final val nonNilArray2TimeRead: Read[NonNilArray, Time] = Read.instancePF {
    case NonNilArray(
        NonNullBulkString(ToLong(NonNegLong(ts))) +: NonNullBulkString(ToLong(NonNegLong(em))) +: Seq()
        ) =>
      Time(ts, em)
  }

  implicit final def nonNilArray2LabelledHCons[HK <: Symbol, HV, T <: HList](
      implicit HK: Witness.Aux[HK],
      RHV: Read[NonNullBulkString, HV],
      RT: Read[NonNilArray, T]
  ): Read[NonNilArray, FieldType[HK, HV] :: T] = Read.instance {
    case NonNilArray(NonNullBulkString(HK.value.`name`) +: RHV(hv) +: rest) =>
      RT.read(RESP.arr(rest)).map(t => field[HK](hv) :: t)
    case _ => None
  }

  implicit final def nonNilArray2HCons[H, T <: HList](
      implicit
      ev: H <:!< FieldType[_, _],
      RH: Read[NonNullBulkString, H],
      RT: Read[NonNilArray, T]
  ): Read[NonNilArray, H :: T] = Read.instance {
    case NonNilArray(RH(h) +: rest) => RT.read(RESP.arr(rest)).map(h :: _)
  }

  implicit final val nonNilArray2HNil: Read[NonNilArray, HNil] = Read.instancePF {
    case NonNilArray(Seq()) => HNil
  }
}

sealed trait ReadInstances2 {
  implicit final def liftNonNullBulkString2Option[A, B](
      implicit R: Read[A, B]
  ): Read[NullBulkString :+: A :+: CNil, Option[B]] = Read.instancePF {
    case Inl(_)         => None
    case Inr(Inl(R(b))) => Some(b)
  }
  implicit final def liftNonNilArray2Option[A, B](
      implicit R: Read[A, B]
  ): Read[NilArray :+: A :+: CNil, Option[B]] = Read.instancePF {
    case Inl(_)         => None
    case Inr(Inl(R(b))) => Some(b)
  }

  implicit final def liftSimpleToSum[A, B](
      implicit R: Read[A, B],
      ev: A <:!< Coproduct
  ): Read[A :+: CNil, B] = Read.instancePF {
    case Inl(R(b)) => b
  }
}
