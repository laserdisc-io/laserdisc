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

  final def numMinusOneIsNone[A: Read[Num, ?]]: Read[Num :+: CNil, Option[A]] =
    lift2OptionWhen(_.value == -1L)

  final def numZeroIsNone[A: Read[Num, ?]]: Read[Num :+: CNil, Option[A]] =
    lift2OptionWhen(_.value == 0L)
}

trait ReadInstances0 extends ReadInstances1 {
  implicit final def identity[A]: Read[A, A] = Read.instance(Some(_))
}

trait ReadInstances1 extends ReadInstances2 {
  implicit final val str2StringRead: Read[Str, String] = Read.instance(s => Some(s.value))
  implicit final val str2OKRead: Read[Str, OK]         = Read.instancePF { case Str("OK") => OK }
  implicit final val str2KeyRead: Read[Str, Key]       = Read.instancePF { case Str(Key(s)) => s }

  implicit final val num2BooleanRead: Read[Num, Boolean] = Read.instancePF {
    case Num(0L) => false; case Num(1L) => true
  }
  implicit final val num2IntRead: Read[Num, Int]                 = Read.instancePF { case Num(ToInt(i))             => i }
  implicit final val num2LongRead: Read[Num, Long]               = Read.instancePF { case Num(l)                    => l }
  implicit final val num2NonNegIntRead: Read[Num, NonNegInt]     = Read.instancePF { case Num(ToInt(NonNegInt(i)))  => i }
  implicit final val num2NonNegLongRead: Read[Num, NonNegLong]   = Read.instancePF { case Num(NonNegLong(l))        => l }
  implicit final val num2NonZeroIntRead: Read[Num, NonZeroInt]   = Read.instancePF { case Num(ToInt(NonZeroInt(i))) => i }
  implicit final val num2NonZeroLongRead: Read[Num, NonZeroLong] = Read.instancePF { case Num(NonZeroLong(l))       => l }
  implicit final val num2PosIntRead: Read[Num, PosInt]           = Read.instancePF { case Num(ToInt(PosInt(i)))     => i }
  implicit final val num2PosLongRead: Read[Num, PosLong]         = Read.instancePF { case Num(PosLong(l))           => l }
  implicit final val num2SlotRead: Read[Num, Slot]               = Read.instancePF { case Num(ToInt(Slot(i)))       => i }

  implicit final val bulk2StringRead: Read[Bulk, String]                 = Read.instance { case Bulk(s)                            => Some(s) }
  implicit final val bulk2DoubleRead: Read[Bulk, Double]                 = Read.instancePF { case Bulk(ToDouble(d))                => d }
  implicit final val bulk2IntRead: Read[Bulk, Int]                       = Read.instancePF { case Bulk(ToInt(i))                   => i }
  implicit final val bulk2LongRead: Read[Bulk, Long]                     = Read.instancePF { case Bulk(ToLong(l))                  => l }
  implicit final val bulk2ValidDoubleRead: Read[Bulk, ValidDouble]       = Read.instancePF { case Bulk(ToDouble(ValidDouble(d)))   => d }
  implicit final val bulk2NonNegIntRead: Read[Bulk, NonNegInt]           = Read.instancePF { case Bulk(ToInt(NonNegInt(i)))        => i }
  implicit final val bulk2NonNegLongRead: Read[Bulk, NonNegLong]         = Read.instancePF { case Bulk(ToLong(NonNegLong(l)))      => l }
  implicit final val bulk2NonNegDoubleRead: Read[Bulk, NonNegDouble]     = Read.instancePF { case Bulk(ToDouble(NonNegDouble(d)))  => d }
  implicit final val bulk2NonZeroDoubleRead: Read[Bulk, NonZeroDouble]   = Read.instancePF { case Bulk(ToDouble(NonZeroDouble(d))) => d }
  implicit final val bulk2NonZeroIntRead: Read[Bulk, NonZeroInt]         = Read.instancePF { case Bulk(ToInt(NonZeroInt(i)))       => i }
  implicit final val bulk2NonZeroLongRead: Read[Bulk, NonZeroLong]       = Read.instancePF { case Bulk(ToLong(NonZeroLong(l)))     => l }
  implicit final val bulk2PosIntRead: Read[Bulk, PosInt]                 = Read.instancePF { case Bulk(ToInt(PosInt(i)))           => i }
  implicit final val bulk2PosLongRead: Read[Bulk, PosLong]               = Read.instancePF { case Bulk(ToLong(PosLong(l)))         => l }
  implicit final val bulk2ConnectionNameRead: Read[Bulk, ConnectionName] = Read.instancePF { case Bulk(ConnectionName(cn))         => cn }
  implicit final val bulk2KeyRead: Read[Bulk, Key]                       = Read.instancePF { case Bulk(Key(k))                     => k }
  implicit final val bulk2GeoHashRead: Read[Bulk, GeoHash]               = Read.instancePF { case Bulk(GeoHash(gh))                => gh }

  implicit final def arrOfBulk2Seq[A](implicit R: Read[Bulk, A]): Read[Arr, Seq[A]] = Read.instance {
    case Arr(vector) =>
      val (vectorLength, (as, asLength)) = vector.foldLeft(0 -> (List.empty[A] -> 0)) {
        case ((vl, (as0, asl)), R(a)) => (vl + 1) -> ((a :: as0) -> (asl + 1))
        case ((vl, acc), _)           => (vl + 1) -> acc
      }
      if (vectorLength == asLength) Some(as.reverse) else None
    case _ => None
  }

  implicit final def arrOfBulk2OptionSeq[A](implicit R: Read[Bulk, A]): Read[Arr, Seq[Option[A]]] = Read.instance {
    case Arr(vector) =>
      val (vectorLength, (as, asLength)) = vector.foldLeft(0 -> (List.empty[Option[A]] -> 0)) {
        case ((vl, (as0, asl)), NullBulk) => (vl + 1) -> ((None :: as0) -> (asl + 1))
        case ((vl, (as0, asl)), R(a))     => (vl + 1) -> ((Some(a) :: as0) -> (asl + 1))
        case ((vl, acc), _)               => (vl + 1) -> acc
      }
      if (vectorLength == asLength) Some(as.reverse) else None
    case _ => None
  }

  implicit final def arrOfArr2OptionSeq[A](implicit R: Read[Arr, A]): Read[Arr, Seq[Option[A]]] = Read.instance {
    case Arr(vector) =>
      val (vectorLength, (as, asLength)) = vector.foldLeft(0 -> (List.empty[Option[A]] -> 0)) {
        case ((vl, (as0, asl)), NilArr) => (vl + 1) -> ((None :: as0) -> (asl + 1))
        case ((vl, (as0, asl)), R(a))   => (vl + 1) -> ((Some(a) :: as0) -> (asl + 1))
        case ((vl, acc), _)             => (vl + 1) -> acc
      }
      if (vectorLength == asLength) Some(as.reverse) else None
    case _ => None
  }

  implicit final def arrOfArr2Seq[A](implicit R: Read[Arr, A]): Read[Arr, Seq[A]] = Read.instance {
    case Arr(vector) =>
      val (vectorLength, (as, asLength)) = vector.foldLeft(0 -> (List.empty[A] -> 0)) {
        case ((vl, (as0, asl)), R(a)) => (vl + 1) -> ((a :: as0) -> (asl + 1))
        case ((vl, acc), _)           => (vl + 1) -> acc
      }
      if (vectorLength == asLength) Some(as.reverse) else None
    case _ => None
  }

  implicit final def arr2Tuple2Read[A, B](implicit RA: Read[Bulk, A], RB: Read[Bulk, B]): Read[Arr, (A, B)] = Read.instancePF {
    case Arr(RA(a) +: RB(b) +: Seq()) => a -> b
  }
//
//  implicit final def arr2Tuple3Read[A, B, C](
//      implicit RA: Read[Bulk, A],
//      RB: Read[Bulk, B]
//  ): Read[Arr, (A, B)] = Read.instancePF {
//    case Arr(RA(key) +: RB(value) +: Seq()) => key -> value
//  }
//
//  implicit final def arr2Tuple4Read[A, B, C, D](
//      implicit RA: Read[Bulk, A],
//      RB: Read[Bulk, B]
//  ): Read[Arr, (A, B)] = Read.instancePF {
//    case Arr(RA(key) +: RB(value) +: Seq()) => key -> value
//  }

  implicit final def arr2Tuple2Seq[A, B](implicit RA: Read[Bulk, A], RB: Read[Bulk, B]): Read[Arr, Seq[(A, B)]] = Read.instance {
    case Arr(vector) =>
      val (vectorLength, (abs, absLength)) =
        vector.grouped(2).foldLeft(0 -> (List.empty[(A, B)] -> 0)) {
          case ((vl, (abs0, absl)), RA(a) +: RB(b) +: Seq()) => (vl + 1) -> (((a -> b) :: abs0) -> (absl + 1))
          case ((vl, acc), _)                                => (vl + 1) -> acc
        }
      if (vectorLength == absLength) Some(abs.reverse) else None
    case _ => None
  }

  implicit final def arr2KV[A](implicit R: Read[Bulk, A]): Read[Arr, KV[A]] = Read.instancePF {
    case Arr(Bulk(Key(k)) +: R(a) +: Seq()) => KV(k, a)
  }

  implicit final def arr2Scan[A](implicit R: Read[Arr, Seq[A]]): Read[Arr, Scan[A]] = Read.instancePF {
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: NilArr +: Seq()) => Scan(cursor, None)
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: R(as) +: Seq())  => Scan(cursor, Some(as))
  }

  implicit final val arr2Map: Read[Arr, Map[Key, String]] = Read.instance {
    case Arr(vector) =>
      val (vectorLength, (kvs, kvsLength)) = vector.grouped(2).foldLeft(0 -> (Map.empty[Key, String] -> 0)) {
        case ((vl, (kv, kvl)), Bulk(Key(k)) +: Bulk(v) +: Seq()) => (vl + 1) -> ((kv + (k -> v)) -> (kvl + 1))
        case ((vl, acc), _)                                      => (vl + 1) -> acc
      }
      if (vectorLength == kvsLength) Some(kvs) else None
    case _ => None
  }
  implicit final val arr2ScanKV: Read[Arr, ScanKV] = Read.instance {
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: NilArr +: Seq()) => Some(ScanKV(cursor, None))
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: Arr(vector) +: Seq()) =>
      val (vectorLength, (kvs, kvsLength)) = vector.grouped(2).foldLeft(0 -> (List.empty[KV[String]] -> 0)) {
        case ((vl, (kv, kvl)), Bulk(Key(k)) +: Bulk(v) +: Seq()) => (vl + 1) -> ((KV(k, v) :: kv) -> (kvl + 1))
        case ((vl, acc), _)                                      => (vl + 1) -> acc
      }
      if (vectorLength == kvsLength) Some(ScanKV(cursor, Some(kvs.reverse))) else None
    case _ => None
  }
  implicit final val arr2TimeRead: Read[Arr, Time] = Read.instancePF {
    case Arr(Bulk(ToLong(NonNegLong(ts))) +: Bulk(ToLong(NonNegLong(em))) +: Seq()) => Time(ts, em)
  }

  implicit final def arr2LabelledHCons[HK <: Symbol, HV, T <: HList](
      implicit HK: Witness.Aux[HK],
      RHV: Read[Bulk, HV],
      RT: Read[Arr, T]
  ): Read[Arr, FieldType[HK, HV] :: T] = Read.instance {
    case Arr(Bulk(HK.value.`name`) +: RHV(hv) +: rest) => RT.read(Arr(rest)).map(t => field[HK](hv) :: t)
    case _                                             => None
  }

  implicit final def arr2HCons[H: <:!<[?, FieldType[_, _]], T <: HList](
      implicit RH: Read[Bulk, H],
      RT: Read[Arr, T]
  ): Read[Arr, H :: T] = Read.instance {
    case Arr(RH(h) +: rest) => RT.read(Arr(rest)).map(h :: _)
  }

  implicit final val arr2HNil: Read[Arr, HNil] = Read.instancePF { case Arr(Seq()) => HNil }
}

sealed trait ReadInstances2 {
  implicit final def liftNullBulk2Option[A, B](implicit R: Read[A, B]): Read[A :+: NullBulk :+: CNil, Option[B]] = Read.instancePF {
    case Inl(R(b)) => Some(b)
    case Inr(_)    => None
  }
  implicit final def liftNilArr2Option[A, B](implicit R: Read[A, B]): Read[A :+: NilArr :+: CNil, Option[B]] = Read.instancePF {
    case Inl(R(b)) => Some(b)
    case Inr(_)    => None
  }
  implicit final def liftSimpleToSum[A: <:!<[?, Coproduct], B](implicit R: Read[A, B]): Read[A :+: CNil, B] = Read.instancePF {
    case Inl(R(b)) => b
  }
}
