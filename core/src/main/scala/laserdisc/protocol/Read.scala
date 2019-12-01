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

Note 1: you can use the factory method Read.instance instead of creating it manually as shown above
Note 2: make sure to inspect the combinators as you may be able to leverage some other Read instance
"""
) trait Read[A, B] { self =>

  def read(a: A): Err | B

  final def map[C](f: B => C): Read[A, C] = Read.instance(read(_).map(f))

  final def flatMap[C](f: B => Read[A, C]): Read[A, C] = Read.instance(a => read(a).flatMap(f(_).read(a)))

  final def contramap[C](f: C => A): Read[C, B] = Read.instance(read _ compose f)

  private[this] final val _extract: Any => Read.Extract[Any] = new Read.Extract[Any](_)
  final def unapply(a: A): Read.Extract[Err | B] =
    _extract(read(a)).asInstanceOf[Read.Extract[Err | B]]
}

object Read extends ReadInstances0 {
  private[Read] final class Extract[T](private val t: T) extends AnyVal {
    def isEmpty: Boolean = false
    def get: T           = t
  }

  @inline final def apply[A, B](implicit instance: Read[A, B]): Read[A, B] = instance

  @inline final def instance[A, B](f: A => Err | B): Read[A, B] = (a: A) => f(a)

  @inline final def lift2OptionWhen[A, B](cond: A => Boolean)(
      implicit R: Read[A, B]
  ): Read[A :+: CNil, Option[B]] = Read.instance {
    case Inl(i @ R(Right(b))) if !cond(i) => Right(Some(b))
    case Inl(_)                           => Right(None)
  }

  @inline final def numMinusOneIsNone[A: Read[Num, *]]: Read[Num :+: CNil, Option[A]] =
    lift2OptionWhen(_.value == -1L)

  @inline final def numZeroIsNone[A: Read[Num, *]]: Read[Num :+: CNil, Option[A]] =
    lift2OptionWhen(_.value == 0L)
}

trait ReadInstances0 extends ReadInstances1 {
  implicit final def identity[A]: Read[A, A] = Read.instance(_.asRight)
}

trait ReadInstances1 extends EitherSyntax with ReadInstances2 {
  implicit final val str2StringRead: Read[Str, String] = Read.instance(s => Right(s.value))
  implicit final val str2OKRead: Read[Str, OK] = Read.instance {
    case Str("OK")  => Right(OK)
    case Str(other) => Left(readError("OK", other))
  }
  implicit final val str2KeyRead: Read[Str, Key] = Read.instance {
    case Str(Key(s)) => Right(s)
    case Str(other)  => Left(readError("a Key", other))
  }

  implicit final val num2LongRead: Read[Num, Long] = Read.instance(n => Right(n.value))
  implicit final val num2BooleanRead: Read[Num, Boolean] = Read.instance {
    case Num(0L)    => Right(false)
    case Num(1L)    => Right(true)
    case Num(other) => Left(readError("0L or 1L", other))
  }
  implicit final val num2IntRead: Read[Num, Int] = Read.instance {
    case Num(ToInt(i)) => Right(i)
    case Num(other)    => Left(readError("a Int", other))
  }
  implicit final val num2NonNegIntRead: Read[Num, NonNegInt] = Read.instance {
    case Num(ToInt(NonNegInt(i))) => Right(i)
    case Num(other)               => Left(readError("a NonNeg Int", other))
  }
  implicit final val num2NonNegLongRead: Read[Num, NonNegLong] = Read.instance {
    case Num(NonNegLong(l)) => Right(l)
    case Num(other)         => Left(readError("a NonNeg Long", other))
  }
  implicit final val num2NonZeroIntRead: Read[Num, NonZeroInt] = Read.instance {
    case Num(ToInt(NonZeroInt(i))) => Right(i)
    case Num(other)                => Left(readError("a NonZero Int", other))
  }
  implicit final val num2NonZeroLongRead: Read[Num, NonZeroLong] = Read.instance {
    case Num(NonZeroLong(l)) => Right(l)
    case Num(other)          => Left(readError("a NonZero Long", other))
  }
  implicit final val num2PosIntRead: Read[Num, PosInt] = Read.instance {
    case Num(ToInt(PosInt(i))) => Right(i)
    case Num(other)            => Left(readError("a Pos Int", other))
  }
  implicit final val num2PosLongRead: Read[Num, PosLong] = Read.instance {
    case Num(PosLong(l)) => Right(l)
    case Num(other)      => Left(readError("a Pos Long", other))
  }
  implicit final val num2SlotRead: Read[Num, Slot] = Read.instance {
    case Num(ToInt(Slot(i))) => Right(i)
    case Num(other)          => Left(readError("a Slot Int", other))
  }

  implicit final val bulk2StringRead: Read[Bulk, String] = Read.instance { case Bulk(s) => Right(s) }
  implicit final val bulk2DoubleRead: Read[Bulk, Double] = Read.instance {
    case Bulk(ToDouble(d)) => Right(d)
    case Bulk(other)       => Left(readError("a Double", other))
  }
  implicit final val bulk2IntRead: Read[Bulk, Int] = Read.instance {
    case Bulk(ToInt(i)) => Right(i)
    case Bulk(other)    => Left(readError("a Int", other))
  }
  implicit final val bulk2LongRead: Read[Bulk, Long] = Read.instance {
    case Bulk(ToLong(l)) => Right(l)
    case Bulk(other)     => Left(readError("a Long", other))
  }
  implicit final val bulk2ValidDoubleRead: Read[Bulk, ValidDouble] = Read.instance {
    case Bulk(ToDouble(ValidDouble(d))) => Right(d)
    case Bulk(other)                    => Left(readError("a Valid Double", other))
  }
  implicit final val bulk2NonNegIntRead: Read[Bulk, NonNegInt] = Read.instance {
    case Bulk(ToInt(NonNegInt(i))) => Right(i)
    case Bulk(other)               => Left(readError("a NonNeg Int", other))
  }
  implicit final val bulk2NonNegLongRead: Read[Bulk, NonNegLong] = Read.instance {
    case Bulk(ToLong(NonNegLong(l))) => Right(l)
    case Bulk(other)                 => Left(readError("a NonNeg Long", other))
  }
  implicit final val bulk2NonNegDoubleRead: Read[Bulk, NonNegDouble] = Read.instance {
    case Bulk(ToDouble(NonNegDouble(d))) => Right(d)
    case Bulk(other)                     => Left(readError("a NonNeg Double", other))
  }
  implicit final val bulk2NonZeroDoubleRead: Read[Bulk, NonZeroDouble] = Read.instance {
    case Bulk(ToDouble(NonZeroDouble(d))) => Right(d)
    case Bulk(other)                      => Left(readError("a NonZero Double", other))
  }
  implicit final val bulk2NonZeroIntRead: Read[Bulk, NonZeroInt] = Read.instance {
    case Bulk(ToInt(NonZeroInt(i))) => Right(i)
    case Bulk(other)                => Left(readError("a NonZero Int", other))
  }
  implicit final val bulk2NonZeroLongRead: Read[Bulk, NonZeroLong] = Read.instance {
    case Bulk(ToLong(NonZeroLong(l))) => Right(l)
    case Bulk(other)                  => Left(readError("a NonZero Long", other))
  }
  implicit final val bulk2PosIntRead: Read[Bulk, PosInt] = Read.instance {
    case Bulk(ToInt(PosInt(i))) => Right(i)
    case Bulk(other)            => Left(readError("a Pos Int", other))
  }
  implicit final val bulk2PosLongRead: Read[Bulk, PosLong] = Read.instance {
    case Bulk(ToLong(PosLong(l))) => Right(l)
    case Bulk(other)              => Left(readError("a Pos Long", other))
  }
  implicit final val bulk2ConnectionNameRead: Read[Bulk, ConnectionName] = Read.instance {
    case Bulk(ConnectionName(cn)) => Right(cn)
    case Bulk(other)              => Left(readError("a Connection Name", other))
  }
  implicit final val bulk2KeyRead: Read[Bulk, Key] = Read.instance {
    case Bulk(Key(k)) => Right(k)
    case Bulk(other)  => Left(readError("a Key", other))
  }
  implicit final val bulk2GeoHashRead: Read[Bulk, GeoHash] = Read.instance {
    case Bulk(GeoHash(gh)) => Right(gh)
    case Bulk(other)       => Left(readError("a GeoHash", other))
  }

  implicit final def arrOfBulk2Seq[A](implicit R: Read[Bulk, A]): Read[Arr, Seq[A]] = Read.instance {
    case Arr(vector) =>
      vector.foldRight[Err | (List[A], Int)](Right(Nil -> 0)) {
        case (R(Right(a)), Right((as0, asl))) => Right((a :: as0) -> (asl + 1))
        case (R(Left(e)), Right((_, asl)))    => Left(Err(s"Arr(Bulk) ==> Seq[A] error at element ${asl + 1}: ${e.message}"))
        case (other, Right((_, asl)))         => Left(Err(s"Arr(Bulk) ==> Seq[A] error at element ${asl + 1}: Unexpected for Bulk. Was $other"))
        case (_, Left(err))                   => Left(err)
      } map (_._1)
  }
  implicit final def arrOfArr2Seq[A](implicit R: Read[Arr, A]): Read[Arr, Seq[A]] = Read.instance {
    case Arr(vector) =>
      vector.foldRight[Err | (List[A], Int)](Right(Nil -> 0)) {
        case (R(Right(a)), Right((as0, asl))) => Right((a :: as0) -> (asl + 1))
        case (R(Left(e)), Right((_, asl)))    => Left(Err(s"Arr(Arr) ==> Seq[A] error at element ${asl + 1}: ${e.message}"))
        case (other, Right((_, asl)))         => Left(Err(s"Arr(Arr) ==> Seq[A] error at element ${asl + 1}: Unexpected for Arr. Was $other"))
        case (_, Left(err))                   => Left(err)
      } map (_._1)
  }
  implicit final def arrOfBulk2OptionSeq[A](implicit R: Read[Bulk, A]): Read[Arr, Seq[Option[A]]] = Read.instance {
    case Arr(vector) =>
      vector.foldRight[Err | (List[Option[A]], Int)](Right(Nil -> 0)) {
        case (NullBulk, Right((as0, asl)))    => Right((None :: as0) -> (asl + 1))
        case (R(Right(a)), Right((as0, asl))) => Right((Some(a) :: as0) -> (asl + 1))
        case (R(Left(e)), Right((_, asl)))    => Left(Err(s"Arr(Bulk) ==> Seq[Option[A]] error at element ${asl + 1}: ${e.message}"))
        case (other, Right((_, asl))) =>
          Left(Err(s"Arr(Bulk) ==> Seq[Option[A]] error at element ${asl + 1}: Unexpected for Bulk. Was $other"))
        case (_, Left(err)) => Left(err)
      } map (_._1)
  }
  implicit final def arrOfArr2OptionSeq[A](implicit R: Read[Arr, A]): Read[Arr, Seq[Option[A]]] = Read.instance {
    case Arr(vector) =>
      vector.foldRight[Err | (List[Option[A]], Int)](Right(Nil -> 0)) {
        case (NilArr, Right((as0, asl)))      => Right((None :: as0) -> (asl + 1))
        case (R(Right(a)), Right((as0, asl))) => Right((Some(a) :: as0) -> (asl + 1))
        case (R(Left(e)), Right((_, asl)))    => Left(Err(s"Arr(Arr) ==> Seq[Option[A]] error at element ${asl + 1}: ${e.message}"))
        case (other, Right((_, asl))) =>
          Left(Err(s"Arr(Arr) ==> Seq[Option[A]] error at element ${asl + 1}: Unexpected for Arr. Was $other"))
        case (_, Left(err)) => Left(err)
      } map (_._1)
  }
  implicit final def arr2Tuple2Read[A, B](implicit RA: Read[Bulk, A], RB: Read[Bulk, B]): Read[Arr, (A, B)] = Read.instance {
    case Arr(RA(Right(a)) +: RB(Right(b)) +: Seq()) => Right(a -> b)
    case Arr(other)                                 => Left(readError("a valid Arr sequence for the pair", other))
  }
  implicit final def arr2Tuple2Seq[A, B](implicit RA: Read[Bulk, A], RB: Read[Bulk, B]): Read[Arr, Seq[(A, B)]] = Read.instance {
    case Arr(vector) =>
      vector.grouped(2).foldRight[Err | (List[(A, B)], Int)](Right(Nil -> 0)) {
        case (RA(Right(a)) +: RB(Right(b)) +: Seq(), Right((abs0, absl))) =>
          Right(((a -> b) :: abs0) -> (absl + 1))
        case (RA(Left(ea)) +: _ +: Seq(), Right((_, absl))) =>
          Left(Err(s"Arr(Bulk) ==> Seq[(A, B)] error in the first element at pair ${absl + 1}: ${ea.message}"))
        case (_ +: RA(Left(eb)) +: Seq(), Right((_, absl))) =>
          Left(Err(s"Arr(Bulk) ==> Seq[(A, B)] error in the second element at pair ${absl + 1}: ${eb.message}"))
        case (otherA +: otherB +: Seq(), Right((_, absl))) =>
          Left(Err(s"Arr(Bulk) ==> Seq[(A, B)] error at element ${absl + 1}: Unexpected for A or B. Was ${(otherA, otherB)}"))
        case (_ +: Seq(), Right(_)) =>
          Left(Err(s"Arr(Bulk) ==> Seq[(A, B)] error: uneven number of elements in Arr. Can't form pairs."))
        case (_, Left(err)) => Left(err)
      } map (_._1)
  }
  implicit final val arr2Map: Read[Arr, Map[Key, String]] = Read.instance {
    case Arr(vector) =>
      vector.grouped(2).foldRight[Err | (Map[Key, String], Int)](Right(Map.empty -> 0)) {
        case (Bulk(Key(k)) +: Bulk(v) +: Seq(), Right((kvs, kvl))) =>
          Right(((kvs + (k -> v)) -> (kvl + 1)))
        case (Bulk(Key(_)) +: any +: Seq(), Right((_, kvl))) =>
          Left(Err(s"Arr ==> Map[Key, String] error in the value at pair ${kvl + 1}: $any is not Bulk(String)"))
        case (any +: Bulk(_) +: Seq(), Right((_, kvl))) =>
          Left(Err(s"Arr ==> Map[Key, String] error in the key at pair ${kvl + 1}: $any is not Key(String)"))
        case (_ +: Seq(), Right(_)) =>
          Left(Err(s"Arr ==> Map[Key, String] error: uneven number of elements in Arr. Can't form a Map."))
        case (_, Left(err)) => Left(err)
      } map (_._1)
  }
  implicit final val arr2ScanKV: Read[Arr, ScanKV] = Read.instance {
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: NilArr +: Seq()) => Right(ScanKV(cursor, None))
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: Arr(vector) +: Seq()) =>
      vector.grouped(2).foldRight[Err | (List[KV[String]], Int)](Right(Nil -> 0)) {
        case (Bulk(Key(k)) +: Bulk(v) +: Seq(), Right((kv, kvl))) =>
          Right((KV(k, v) :: kv) -> (kvl + 1))
        case (Bulk(Key(_)) +: any +: Seq(), Right((_, kvl))) =>
          Left(Err(s"Arr ==> ScanKV error in the value at pair ${kvl + 1}: $any is not Bulk(String)"))
        case (any +: Bulk(_) +: Seq(), Right((_, kvl))) =>
          Left(Err(s"Arr ==> ScanKV error in the key at pair ${kvl + 1}: $any is not Key(String)"))
        case (_ +: Seq(), Right(_)) =>
          Left(Err(s"Arr ==> ScanKV error: uneven number of elements in Arr. Can't form a KV[String]."))
        case (_, Left(err)) => Left(err)
      } map (r => ScanKV(cursor, Some(r._1)))
  }
  implicit final def arr2KV[A](implicit R: Read[Bulk, A]): Read[Arr, KV[A]] = Read.instance {
    case Arr(Bulk(Key(k)) +: R(Right(a)) +: Seq()) => Right(KV(k, a))
    case Arr(other)                                => Left(readError("a valid KV", other))
  }
  implicit final def arr2Scan[A](implicit R: Read[Arr, Seq[A]]): Read[Arr, Scan[A]] = Read.instance {
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: NilArr +: Seq())       => Right(Scan(cursor, None))
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: R(Right(as)) +: Seq()) => Right(Scan(cursor, Some(as)))
    case Arr(other)                                                     => Left(readError("a valid scan", other))
  }
  implicit final val arr2TimeRead: Read[Arr, Time] = Read.instance {
    case Arr(Bulk(ToLong(NonNegLong(ts))) +: Bulk(ToLong(NonNegLong(em))) +: Seq()) => Right(Time(ts, em))
    case Arr(other)                                                                 => Left(readError("a valid time", other))
  }
  implicit final def arr2LabelledHCons[HK <: Symbol, HV, T <: HList](
      implicit HK: Witness.Aux[HK],
      RHV: Read[Bulk, HV],
      RT: Read[Arr, T]
  ): Read[Arr, FieldType[HK, HV] :: T] = Read.instance {
    case Arr(Bulk(HK.value.`name`) +: RHV(Right(hv)) +: rest) => RT.read(Arr(rest)).map(t => field[HK](hv) :: t)
    case Arr(other)                                           => Left(readError("[K, V] :: T", other))
  }
  implicit final def arr2HCons[H: <:!<[*, FieldType[_, _]], T <: HList](
      implicit RH: Read[Bulk, H],
      RT: Read[Arr, T]
  ): Read[Arr, H :: T] = Read.instance {
    case Arr(RH(Right(h)) +: rest) => RT.read(Arr(rest)).map(h :: _)
    case Arr(other)                => Left(readError("H :: T", other))
  }
  implicit final val arr2HNil: Read[Arr, HNil] = Read.instance {
    case Arr(Seq()) => Right(HNil)
    case Arr(other) => Left(readError("Seq()", other))
  }

  @inline final private[this] def readError[A](expected: String, read: A): Err =
    Err(s"Read Error: expected $expected but was '$read'")
}

sealed trait ReadInstances2 {
  implicit final def liftNullBulk2Option[A, B](implicit R: Read[A, B]): Read[A :+: NullBulk :+: CNil, Option[B]] =
    Read.instance {
      case Inl(R(Right(b))) => Right(Some(b))
      case Inr(_)           => Right(None)
    }
  implicit final def liftNilArr2Option[A, B](implicit R: Read[A, B]): Read[A :+: NilArr :+: CNil, Option[B]] =
    Read.instance {
      case Inl(R(Right(b))) => Right(Some(b))
      case Inr(_)           => Right(None)
    }
  implicit final def liftSimpleToSum[A: <:!<[*, Coproduct], B](implicit R: Read[A, B]): Read[A :+: CNil, B] =
    Read.instance {
      case Inl(R(rb)) => rb
      case Inl(R(le)) => le
    }
}
