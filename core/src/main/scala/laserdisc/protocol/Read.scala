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

  def read(a: A): RESPDecErr | B

  final def map[C](f: B => C): Read[A, C] = Read.instance(read(_).map(f))

  final def contramap[C](f: C => A): Read[C, B] = Read.instance(read _ compose f)

  final def flatMap[C](f: B => Read[A, C]): Read[A, C] = Read.instance(a => read(a).flatMap(f(_).read(a)))

  private[this] final val _extract: Any => Read.Extract[Any] = new Read.Extract[Any](_)
  final def unapply(a: A): Read.Extract[RESPDecErr | B] =
    _extract(read(a)).asInstanceOf[Read.Extract[RESPDecErr | B]]
}

object Read extends ReadInstances0 {
  private[Read] final class Extract[T](private val t: T) extends AnyVal {
    def isEmpty: Boolean = false
    def get: T           = t
  }

  @inline final def apply[A, B](implicit instance: Read[A, B]): Read[A, B] = instance

  @inline final def instance[A, B](f: A => RESPDecErr | B): Read[A, B] = (a: A) => f(a)

  @inline final def infallible[A, B](f: A => B): Read[A, B] = (a: A) => Right(f(a))

  @inline final def const[A, B](b: B): Read[A, B] = Read.infallible(_ => b)

  @inline final def instancePF[A, B](expectation: String)(pf: PartialFunction[A, B]): Read[A, B] =
    a => if (pf.isDefinedAt(a)) Right(pf(a)) else Left(RESPDecErr(s"Read Error: expected $expectation but was $a"))

  @inline final def lift2OptionWhen[A, B](cond: A => Boolean)(
      implicit R: Read[A, B]
  ): Read[A :+: CNil, Option[B]] = Read.infallible {
    case Inl(i @ R(Right(b))) if !cond(i) => Some(b)
    case Inl(_)                           => None
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
  import Read.{infallible, instance, instancePF}

  implicit final val str2StringRead: Read[Str, String] = infallible(_.value)
  implicit final val str2OKRead: Read[Str, OK]         = instancePF("Str(OK)") { case Str("OK") => OK }
  implicit final val str2KeyRead: Read[Str, Key]       = instancePF("Str(Key)") { case Str(Key(s)) => s }

  implicit final val num2LongRead: Read[Num, Long] = Read.instance(n => Right(n.value))
  implicit final val num2BooleanRead: Read[Num, Boolean] = Read.instancePF("0L or 1L") {
    case Num(0L) => false
    case Num(1L) => true
  }
  implicit final val num2IntRead: Read[Num, Int]                 = instancePF("Num(Int)") { case Num(ToInt(i))                    => i }
  implicit final val num2NonNegIntRead: Read[Num, NonNegInt]     = instancePF("Num(NonNegInt)") { case Num(ToInt(NonNegInt(i)))   => i }
  implicit final val num2NonNegLongRead: Read[Num, NonNegLong]   = instancePF("Num(NonNegLong)") { case Num(NonNegLong(l))        => l }
  implicit final val num2NonZeroIntRead: Read[Num, NonZeroInt]   = instancePF("Num(NonZeroInt)") { case Num(ToInt(NonZeroInt(i))) => i }
  implicit final val num2NonZeroLongRead: Read[Num, NonZeroLong] = instancePF("Num(NonZeroLong)") { case Num(NonZeroLong(l))      => l }
  implicit final val num2PosIntRead: Read[Num, PosInt]           = instancePF("Num(PosInt)") { case Num(ToInt(PosInt(i)))         => i }
  implicit final val num2PosLongRead: Read[Num, PosLong]         = instancePF("Num(PosLong)") { case Num(PosLong(l))              => l }
  implicit final val num2SlotRead: Read[Num, Slot]               = instancePF("Num(SlotInt)") { case Num(ToInt(Slot(i)))          => i }

  implicit final val bulk2StringRead: Read[Bulk, String] = infallible(_.value)
  implicit final val bulk2DoubleRead: Read[Bulk, Double] = instancePF("Bulk(Double)") { case Bulk(ToDouble(d)) => d }
  implicit final val bulk2IntRead: Read[Bulk, Int]       = instancePF("Bulk(Int)") { case Bulk(ToInt(i)) => i }
  implicit final val bulk2LongRead: Read[Bulk, Long]     = instancePF("Bulk(Long)") { case Bulk(ToLong(l)) => l }
  implicit final val bulk2ValidDoubleRead: Read[Bulk, ValidDouble] = instancePF("Bulk(ValidDouble)") {
    case Bulk(ToDouble(ValidDouble(d))) => d
  }
  implicit final val bulk2NonNegIntRead: Read[Bulk, NonNegInt]   = instancePF("Bulk(NonNegInt)") { case Bulk(ToInt(NonNegInt(i)))    => i }
  implicit final val bulk2NonNegLongRead: Read[Bulk, NonNegLong] = instancePF("Bulk(NonNegLong)") { case Bulk(ToLong(NonNegLong(l))) => l }
  implicit final val bulk2NonNegDoubleRead: Read[Bulk, NonNegDouble] = instancePF("Bulk(NonNegDouble)") {
    case Bulk(ToDouble(NonNegDouble(d))) => d
  }
  implicit final val bulk2NonZeroDoubleRead: Read[Bulk, NonZeroDouble] = instancePF("Bulk(NonZeroDouble)") {
    case Bulk(ToDouble(NonZeroDouble(d))) => d
  }
  implicit final val bulk2NonZeroIntRead: Read[Bulk, NonZeroInt] = instancePF("Bulk(NonZeroInt)") { case Bulk(ToInt(NonZeroInt(i))) => i }
  implicit final val bulk2NonZeroLongRead: Read[Bulk, NonZeroLong] = instancePF("Bulk(NonZeroLong)") {
    case Bulk(ToLong(NonZeroLong(l))) => l
  }
  implicit final val bulk2PosIntRead: Read[Bulk, PosInt]   = instancePF("Bulk(PosInt)") { case Bulk(ToInt(PosInt(i)))    => i }
  implicit final val bulk2PosLongRead: Read[Bulk, PosLong] = instancePF("Bulk(PosLong)") { case Bulk(ToLong(PosLong(l))) => l }
  implicit final val bulk2ConnectionNameRead: Read[Bulk, ConnectionName] = instancePF("Bulk(ConnectionName)") {
    case Bulk(ConnectionName(cn)) => cn
  }
  implicit final val bulk2KeyRead: Read[Bulk, Key]         = instancePF("Bulk(Key)") { case Bulk(Key(k))          => k }
  implicit final val bulk2GeoHashRead: Read[Bulk, GeoHash] = instancePF("Bulk(GeoHash)") { case Bulk(GeoHash(gh)) => gh }

  implicit final def arrOfBulk2Seq[A](implicit R: Read[Bulk, A]): Read[Arr, Seq[A]] = Read.instance {
    case Arr(vector) =>
      vector.foldRight[RESPDecErr | (List[A], Int)](Right(Nil -> 0)) {
        case (R(Right(a)), Right((as0, asl))) => Right((a :: as0) -> (asl + 1))
        case (R(Left(e)), Right((_, asl)))    => Left(RESPDecErr(s"Arr(Bulk) ==> Seq[A] error at element ${asl + 1}: ${e.message}"))
        case (other, Right((_, asl))) =>
          Left(RESPDecErr(s"Arr(Bulk) ==> Seq[A] error at element ${asl + 1}: Unexpected for Bulk. Was $other"))
        case (_, left) => left
      } map (_._1)
  }
  implicit final def arrOfArr2Seq[A](implicit R: Read[Arr, A]): Read[Arr, Seq[A]] = instance {
    case Arr(vector) =>
      vector.foldRight[RESPDecErr | (List[A], Int)](Right(Nil -> 0)) {
        case (R(Right(a)), Right((as0, asl))) => Right((a :: as0) -> (asl + 1))
        case (R(Left(e)), Right((_, asl)))    => Left(RESPDecErr(s"Arr(Arr) ==> Seq[A] error at element ${asl + 1}: ${e.message}"))
        case (other, Right((_, asl))) =>
          Left(RESPDecErr(s"Arr(Arr) ==> Seq[A] error at element ${asl + 1}: Unexpected for Arr. Was $other"))
        case (_, left) => left
      } map (_._1)
  }
  implicit final def arrOfBulk2OptionSeq[A](implicit R: Read[Bulk, A]): Read[Arr, Seq[Option[A]]] = instance {
    case Arr(vector) =>
      vector.foldRight[RESPDecErr | (List[Option[A]], Int)](Right(Nil -> 0)) {
        case (NullBulk, Right((as0, asl)))    => Right((None :: as0) -> (asl + 1))
        case (R(Right(a)), Right((as0, asl))) => Right((Some(a) :: as0) -> (asl + 1))
        case (R(Left(e)), Right((_, asl)))    => Left(RESPDecErr(s"Arr(Bulk) ==> Seq[Option[A]] error at element ${asl + 1}: ${e.message}"))
        case (other, Right((_, asl))) =>
          Left(RESPDecErr(s"Arr(Bulk) ==> Seq[Option[A]] error at element ${asl + 1}: Unexpected for Bulk. Was $other"))
        case (_, left) => left
      } map (_._1)
  }
  implicit final def arrOfArr2OptionSeq[A](implicit R: Read[Arr, A]): Read[Arr, Seq[Option[A]]] = instance {
    case Arr(vector) =>
      vector.foldRight[RESPDecErr | (List[Option[A]], Int)](Right(Nil -> 0)) {
        case (NilArr, Right((as0, asl)))      => Right((None :: as0) -> (asl + 1))
        case (R(Right(a)), Right((as0, asl))) => Right((Some(a) :: as0) -> (asl + 1))
        case (R(Left(e)), Right((_, asl)))    => Left(RESPDecErr(s"Arr(Arr) ==> Seq[Option[A]] error at element ${asl + 1}: ${e.message}"))
        case (other, Right((_, asl))) =>
          Left(RESPDecErr(s"Arr(Arr) ==> Seq[Option[A]] error at element ${asl + 1}: Unexpected for Arr. Was $other"))
        case (_, left) => left
      } map (_._1)
  }
  implicit final def arr2Tuple2Read[A, B](implicit RA: Read[Bulk, A], RB: Read[Bulk, B]): Read[Arr, (A, B)] =
    instancePF("Arr(A, B)") {
      case Arr(RA(Right(a)) +: RB(Right(b)) +: Seq()) => a -> b
    }
  implicit final def arr2Tuple2Seq[A, B](implicit RA: Read[Bulk, A], RB: Read[Bulk, B]): Read[Arr, Seq[(A, B)]] = instance {
    case Arr(vector) =>
      vector.grouped(2).foldRight[RESPDecErr | (List[(A, B)], Int)](Right(Nil -> 0)) {
        case (RA(Right(a)) +: RB(Right(b)) +: Seq(), Right((abs0, absl))) =>
          Right(((a -> b) :: abs0) -> (absl + 1))
        case (RA(Left(ea)) +: _ +: Seq(), Right((_, absl))) =>
          Left(RESPDecErr(s"Arr(Bulk) ==> Seq[(A, B)] error in the first element at pair ${absl + 1}: ${ea.message}"))
        case (_ +: RA(Left(eb)) +: Seq(), Right((_, absl))) =>
          Left(RESPDecErr(s"Arr(Bulk) ==> Seq[(A, B)] error in the second element at pair ${absl + 1}: ${eb.message}"))
        case (otherA +: otherB +: Seq(), Right((_, absl))) =>
          Left(RESPDecErr(s"Arr(Bulk) ==> Seq[(A, B)] error at element ${absl + 1}: Unexpected for A or B. Was ${(otherA, otherB)}"))
        case (_ +: Seq(), Right(_)) =>
          Left(RESPDecErr(s"Arr(Bulk) ==> Seq[(A, B)] error: uneven number of elements in Arr. Can't form pairs."))
        case (_, left) => left
      } map (_._1)
  }
  implicit final val arr2Map: Read[Arr, Map[Key, String]] = instance {
    case Arr(vector) =>
      vector.grouped(2).foldRight[RESPDecErr | (Map[Key, String], Int)](Right(Map.empty -> 0)) {
        case (Bulk(Key(k)) +: Bulk(v) +: Seq(), Right((kvs, kvl))) =>
          Right(((kvs + (k -> v)) -> (kvl + 1)))
        case (Bulk(Key(_)) +: any +: Seq(), Right((_, kvl))) =>
          Left(RESPDecErr(s"Arr ==> Map[Key, String] error in the value at pair ${kvl + 1}: $any is not Bulk(String)"))
        case (any +: Bulk(_) +: Seq(), Right((_, kvl))) =>
          Left(RESPDecErr(s"Arr ==> Map[Key, String] error in the key at pair ${kvl + 1}: $any is not Key(String)"))
        case (_ +: Seq(), Right(_)) =>
          Left(RESPDecErr(s"Arr ==> Map[Key, String] error: uneven number of elements in Arr. Can't form a Map."))
        case (_, left) => left
      } map (_._1)
  }
  implicit final val arr2ScanKV: Read[Arr, ScanKV] = instance {
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: NilArr +: Seq()) => Right(ScanKV(cursor, None))
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: Arr(vector) +: Seq()) =>
      vector.grouped(2).foldRight[RESPDecErr | (List[KV[String]], Int)](Right(Nil -> 0)) {
        case (Bulk(Key(k)) +: Bulk(v) +: Seq(), Right((kv, kvl))) =>
          Right((KV(k, v) :: kv) -> (kvl + 1))
        case (Bulk(Key(_)) +: any +: Seq(), Right((_, kvl))) =>
          Left(RESPDecErr(s"Arr ==> ScanKV error in the value at pair ${kvl + 1}: $any is not Bulk(String)"))
        case (any +: Bulk(_) +: Seq(), Right((_, kvl))) =>
          Left(RESPDecErr(s"Arr ==> ScanKV error in the key at pair ${kvl + 1}: $any is not Key(String)"))
        case (_ +: Seq(), Right(_)) =>
          Left(RESPDecErr(s"Arr ==> ScanKV error: uneven number of elements in Arr. Can't form a KV[String]."))
        case (_, left) => left
      } map (r => ScanKV(cursor, Some(r._1)))
  }
  implicit final def arr2KV[A](implicit R: Read[Bulk, A]): Read[Arr, KV[A]] = instancePF("Arr(KV[A])") {
    case Arr(Bulk(Key(k)) +: R(Right(a)) +: Seq()) => KV(k, a)
  }
  implicit final def arr2Scan[A](implicit R: Read[Arr, Seq[A]]): Read[Arr, Scan[A]] = instancePF("Arr(Scan[A])") {
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: NilArr +: Seq())       => Scan(cursor, None)
    case Arr(Bulk(ToLong(NonNegLong(cursor))) +: R(Right(as)) +: Seq()) => Scan(cursor, Some(as))
  }
  implicit final val arr2TimeRead: Read[Arr, Time] = instancePF("Arr(Time)") {
    case Arr(Bulk(ToLong(NonNegLong(ts))) +: Bulk(ToLong(NonNegLong(em))) +: Seq()) => Time(ts, em)
  }
  implicit final def arr2LabelledHCons[HK <: Symbol, HV, T <: HList](
      implicit HK: Witness.Aux[HK],
      RHV: Read[Bulk, HV],
      RT: Read[Arr, T]
  ): Read[Arr, FieldType[HK, HV] :: T] = instance {
    case Arr(Bulk(HK.value.`name`) +: RHV(Right(hv)) +: rest) => RT.read(Arr(rest)).map(t => field[HK](hv) :: t)
    case Arr(other)                                           => Left(RESPDecErr(s"Read Error: expected `[K, V] :: T` but was $other"))
  }
  implicit final def arr2HCons[H: <:!<[*, FieldType[_, _]], T <: HList](
      implicit RH: Read[Bulk, H],
      RT: Read[Arr, T]
  ): Read[Arr, H :: T] = instance {
    case Arr(RH(Right(h)) +: rest) => RT.read(Arr(rest)).map(h :: _)
    case Arr(other)                => Left(RESPDecErr(s"Read Error: expected `H :: T` but was $other"))
  }
  implicit final val arr2HNil: Read[Arr, HNil] = instancePF("Arr(Seq())") { case Arr(Seq()) => HNil }
}

sealed trait ReadInstances2 {
  implicit final def liftNullBulk2Option[A, B](implicit R: Read[A, B]): Read[A :+: NullBulk :+: CNil, Option[B]] =
    Read.infallible {
      case Inl(R(Right(b))) => Some(b)
      case Inr(_)           => None
    }
  implicit final def liftNilArr2Option[A, B](implicit R: Read[A, B]): Read[A :+: NilArr :+: CNil, Option[B]] =
    Read.infallible {
      case Inl(R(Right(b))) => Some(b)
      case Inr(_)           => None
    }
  implicit final def liftSimpleToSum[A: <:!<[*, Coproduct], B](implicit R: Read[A, B]): Read[A :+: CNil, B] =
    Read.instance {
      case Inl(R(rb)) => rb
      case Inl(R(le)) => le
    }
}
