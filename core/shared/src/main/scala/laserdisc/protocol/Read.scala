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

import scala.annotation.implicitNotFound

@implicitNotFound(
  """Implicit not found Read[${A}, ${B}].

Try writing your own, for example:

implicit final val myRead: Read[${A}, ${B}] = Read.instance { (a: ${A}) =>
  ??? // Right(b) when a can be read as ${B}, Left(RESPDecErr(...)) when it can't
}

You may also be able to build it from an existing Read with map, contramap or flatMap.
"""
) trait Read[A, B] { self =>

  def read(a: A): Either[RESPDecErr, B]

  final def map[C](f: B => C): Read[A, C] = Read.instance(read(_).map(f))

  final def contramap[C](f: C => A): Read[C, B] = Read.instance(c => read(f(c)))

  final def flatMap[C](f: B => Read[A, C]): Read[A, C] = Read.instance(a => read(a).flatMap(f(_).read(a)))

  private final val _extract: Any => Read.Extract[Any]         = new Read.Extract[Any](_)
  final def unapply(a: A): Read.Extract[Either[RESPDecErr, B]] =
    _extract(read(a)).asInstanceOf[Read.Extract[Either[RESPDecErr, B]]]
}

object Read extends ReadInstances0 {
  private[Read] final class Extract[T](private val t: T) extends AnyVal {
    def isEmpty: Boolean = false
    def get: T           = t
  }

  @inline final def apply[A, B](implicit instance: Read[A, B]): Read[A, B] = instance

  @inline final def instance[A, B](f: A => Either[RESPDecErr, B]): Read[A, B] = (a: A) => f(a)

  @inline final def infallible[A, B](f: A => B): Read[A, B] = (a: A) => Right(f(a))

  @inline final def const[A, B](b: B): Read[A, B] = Read.infallible(_ => b)

  @inline final def instancePF[A, B](expectation: String)(pf: PartialFunction[A, B]): Read[A, B] =
    a => if (pf.isDefinedAt(a)) Right(pf(a)) else Left(RESPDecErr(s"Read Error: expected $expectation but was $a"))

  @inline final def lift2OptionWhen[A, B](cond: A => Boolean)(
      implicit R: Read[A, B]
  ): Read[A, Option[B]] =
    Read.infallible {
      case i @ R(Right(b)) if !cond(i) => Some(b)
      case _                           => None
    }

  @inline final def numMinusOneIsNone[A: Read[Num, _]]: Read[Num, Option[A]] =
    lift2OptionWhen(_.value == -1L)

  @inline final def numZeroIsNone[A: Read[Num, _]]: Read[Num, Option[A]] =
    lift2OptionWhen(_.value == 0L)
}

trait ReadInstances0 extends ReadInstances1 {
  implicit final def identity[A]: Read[A, A] = Read.instance(_.asRight)
}

trait ReadInstances1 extends EitherSyntax with ReadLabelledInstances {
  import Read.{infallible, instance, instancePF}

  implicit final val str2StringRead: Read[Str, String] = infallible(_.value)
  implicit final val str2OKRead: Read[Str, OK]         = instancePF("Str(OK)") { case Str("OK") => OK }
  implicit final val str2KeyRead: Read[Str, Key]       = instancePF("Str(Key)") { case Str(Key(s)) => s }

  implicit final val num2LongRead: Read[Num, Long] = instance(n => Right(n.value))

  /** Nothing, as a script returning nil replies */
  implicit final val nullBulk2UnitRead: Read[NullBulk, Unit] = infallible(_ => ())

  // the first element that fails fails the whole array, naming it
  private final def elements[B](arr: Arr.Of[?])(read: RESP => Option[Either[RESPDecErr, B]]): Either[RESPDecErr, Seq[B]] = {
    @scala.annotation.tailrec
    def go(rest: List[RESP], i: Int, acc: List[B]): Either[RESPDecErr, Seq[B]] = rest match {
      case Nil          => Right(acc.reverse)
      case resp :: tail =>
        read(resp) match {
          case Some(Right(b)) => go(tail, i + 1, b :: acc)
          case Some(Left(e))  => Left(RESPDecErr(s"Read[Arr.Of, Seq] error at element $i: ${e.message}"))
          case None           => Left(RESPDecErr(s"Read[Arr.Of, Seq] error at element $i: Unexpected element. Was $resp"))
        }
    }
    go(arr.elements, 1, Nil)
  }

  implicit final def arrOf2Seq[A, B](implicit S: RESPSub[A], R: Read[A, B]): Read[Arr.Of[A], Seq[B]] = instance { arr =>
    elements(arr)(resp => S.unapply(resp).map(R.read))
  }

  // the first pair that fails fails the whole array, naming it
  private final def pairsOf[A, B, C, D](SA: RESPSub[A], RA: Read[A, C], SB: RESPSub[B], RB: Read[B, D])(
      elements: List[RESP]
  ): Either[RESPDecErr, Seq[(C, D)]] = {
    def element[X, Y](resp: RESP, i: Int, which: String)(S: RESPSub[X], R: Read[X, Y]): Either[RESPDecErr, Y] = S.unapply(resp) match {
      case Some(x) => R.read(x).left.map(e => RESPDecErr(s"Read[Arr.OfPairs, Seq] error in the $which element of pair $i: ${e.message}"))
      case None    => Left(RESPDecErr(s"Read[Arr.OfPairs, Seq] error in the $which element of pair $i: Unexpected element. Was $resp"))
    }
    @scala.annotation.tailrec
    def go(rest: List[RESP], i: Int, acc: List[(C, D)]): Either[RESPDecErr, Seq[(C, D)]] = rest match {
      case Nil            => Right(acc.reverse)
      case a :: b :: tail =>
        element(a, i, "first")(SA, RA).flatMap(c => element(b, i, "second")(SB, RB).map(c -> _)) match {
          case Right(cd) => go(tail, i + 1, cd :: acc)
          case Left(e)   => Left(e)
        }
      case last :: Nil => Left(RESPDecErr(s"Read[Arr.OfPairs, Seq] error: odd number of elements, the last being $last"))
    }
    go(elements, 1, Nil)
  }

  implicit final def arrOfPairs2Seq[A, B, C, D](
      implicit SA: RESPSub[A],
      RA: Read[A, C],
      SB: RESPSub[B],
      RB: Read[B, D]
  ): Read[Arr.OfPairs[A, B], Seq[(C, D)]] = instance(arr => pairsOf(SA, RA, SB, RB)(arr.elements))

  implicit final def arrOfPairs2Map[A, B, C, D](
      implicit SA: RESPSub[A],
      RA: Read[A, C],
      SB: RESPSub[B],
      RB: Read[B, D]
  ): Read[Arr.OfPairs[A, B], Map[C, D]] = instance(arr => pairsOf(SA, RA, SB, RB)(arr.elements).map(_.toMap))

  // a plain Arr read as an Arr.OfPairs[A, B], for readers within others
  private[protocol] final def pairs[A, B, C, D](RA: Read[A, C], RB: Read[B, D])(
      implicit SA: RESPSub[A],
      SB: RESPSub[B]
  ): Read[Arr, Seq[(C, D)]] = instance(arr => pairsOf(SA, RA, SB, RB)(arr.elements))

  // the names and values of a plain Arr, for readers of replies made of fields
  private[protocol] final lazy val fields: Read[Arr, Map[String, RESP]] =
    pairs(bulk2StringRead, infallible[RESP, RESP](r => r)).map(_.toMap)

  // a reply made of names and values read field by field: a failure says which record, and which field
  private[protocol] final def record[A](name: String)(read: Fields => Either[RESPDecErr, A]): Read[Record, A] = instance { pairs =>
    fields.read(Arr(pairs.elements)).flatMap(m => read(new Fields(m))).left.map(e => RESPDecErr(s"$name: ${e.message}"))
  }

  implicit final val num2BooleanRead: Read[Num, Boolean] = instancePF("0L or 1L") {
    case Num(0L) => false
    case Num(1L) => true
  }
  implicit final val num2IntRead: Read[Num, Int]                 = instancePF("Num(Int)") { case Num(ToInt(i)) => i }
  implicit final val num2NonNegIntRead: Read[Num, NonNegInt]     = instancePF("Num(NonNegInt)") { case Num(ToInt(NonNegInt(i))) => i }
  implicit final val num2NonNegLongRead: Read[Num, NonNegLong]   = instancePF("Num(NonNegLong)") { case Num(NonNegLong(l)) => l }
  implicit final val num2NonZeroIntRead: Read[Num, NonZeroInt]   = instancePF("Num(NonZeroInt)") { case Num(ToInt(NonZeroInt(i))) => i }
  implicit final val num2NonZeroLongRead: Read[Num, NonZeroLong] = instancePF("Num(NonZeroLong)") { case Num(NonZeroLong(l)) => l }
  implicit final val num2PosIntRead: Read[Num, PosInt]           = instancePF("Num(PosInt)") { case Num(ToInt(PosInt(i))) => i }
  implicit final val num2PosLongRead: Read[Num, PosLong]         = instancePF("Num(PosLong)") { case Num(PosLong(l)) => l }
  implicit final val num2SlotRead: Read[Num, Slot]               = instancePF("Num(SlotInt)") { case Num(ToInt(Slot(i))) => i }

  implicit final val bulk2StringRead: Read[Bulk, String]           = infallible(_.value)
  implicit final val bulk2DoubleRead: Read[Bulk, Double]           = instancePF("Bulk(Double)") { case Bulk(ToDouble(d)) => d }
  implicit final val bulk2IntRead: Read[Bulk, Int]                 = instancePF("Bulk(Int)") { case Bulk(ToInt(i)) => i }
  implicit final val bulk2LongRead: Read[Bulk, Long]               = instancePF("Bulk(Long)") { case Bulk(ToLong(l)) => l }
  implicit final val bulk2ValidDoubleRead: Read[Bulk, ValidDouble] = instancePF("Bulk(ValidDouble)") {
    case Bulk(ToDouble(ValidDouble(d))) => d
  }
  implicit final val bulk2NonNegIntRead: Read[Bulk, NonNegInt]   = instancePF("Bulk(NonNegInt)") { case Bulk(ToInt(NonNegInt(i))) => i }
  implicit final val bulk2NonNegLongRead: Read[Bulk, NonNegLong] = instancePF("Bulk(NonNegLong)") { case Bulk(ToLong(NonNegLong(l))) => l }
  implicit final val bulk2NonNegDoubleRead: Read[Bulk, NonNegDouble] = instancePF("Bulk(NonNegDouble)") {
    case Bulk(ToDouble(NonNegDouble(d))) => d
  }
  implicit final val bulk2NonZeroDoubleRead: Read[Bulk, NonZeroDouble] = instancePF("Bulk(NonZeroDouble)") {
    case Bulk(ToDouble(NonZeroDouble(d))) => d
  }
  implicit final val bulk2NonZeroIntRead: Read[Bulk, NonZeroInt]   = instancePF("Bulk(NonZeroInt)") { case Bulk(ToInt(NonZeroInt(i))) => i }
  implicit final val bulk2NonZeroLongRead: Read[Bulk, NonZeroLong] = instancePF("Bulk(NonZeroLong)") { case Bulk(ToLong(NonZeroLong(l))) =>
    l
  }
  implicit final val bulk2PosIntRead: Read[Bulk, PosInt]   = instancePF("Bulk(PosInt)") { case Bulk(ToInt(PosInt(i))) => i }
  implicit final val bulk2PosLongRead: Read[Bulk, PosLong] = instancePF("Bulk(PosLong)") { case Bulk(ToLong(PosLong(l))) => l }
  implicit final val bulk2ConnectionNameRead: Read[Bulk, ConnectionName] = instancePF("Bulk(ConnectionName)") {
    case Bulk(ConnectionName(cn)) => cn
  }
  implicit final val bulk2KeyRead: Read[Bulk, Key]         = instancePF("Bulk(Key)") { case Bulk(Key(k)) => k }
  implicit final val bulk2GeoHashRead: Read[Bulk, GeoHash] = instancePF("Bulk(GeoHash)") { case Bulk(GeoHash(gh)) => gh }

  // the first alternative that matches and reads, or what the one that matched said, or what each of them said, numbered
  implicit final def oneOf2Read[T <: Tuple, B](implicit R: AlternativesRead[T, B]): Read[OneOf[T], B] = instance { oneOf =>
    R.read(oneOf.value, 1).left.map {
      case Nil           => RESPDecErr(s"Read[OneOf] error: no alternative matched ${oneOf.value}")
      case (_, e) :: Nil => RESPDecErr(s"Read[OneOf] error: $e")
      case failures      => RESPDecErr(s"Read[OneOf] error: ${failures.map { case (i, e) => s"alternative $i: $e" }.mkString("; ")}")
    }
  }

  implicit final def arrAsTuple2Tuple[T, L](implicit R: TupleRead[T, L]): Read[Arr.AsTuple[T], L] =
    instance(arr => TupleRead.read(R, "Arr.AsTuple")(arr.elements))

  private final def restPositions[T, L](R: TupleRead[T, L])(elements: List[RESP]): Either[RESPDecErr, L] =
    if (elements.size < R.size) Left(RESPDecErr(s"Read[Arr.AsTupleThen] error: expected at least ${R.size} elements, got ${elements.size}"))
    else R.read(elements, 1).left.map(e => RESPDecErr(s"Read[Arr.AsTupleThen] ${e.message}"))

  implicit final def arrAsTupleThen2Tuple[T, E, L, B](
      implicit R: TupleRead[T, L],
      S: RESPSub[E],
      RE: Read[E, B]
  ): Read[Arr.AsTupleThen[T, E], (L, Seq[B])] = instance { arr =>
    restPositions(R)(arr.elements).flatMap { l =>
      elements(new Arr.Of[E](arr.elements.drop(R.size)))(resp => S.unapply(resp).map(RE.read)) match {
        case Right(bs) => Right(l -> bs)
        case Left(e)   => Left(RESPDecErr(s"Read[Arr.AsTupleThen] after the first ${R.size} elements: ${e.message}"))
      }
    }
  }

  // what's at the positions alone, leaving out what follows
  implicit final def arrAsTupleThen2Positions[T, L](implicit R: TupleRead[T, L]): Read[Arr.AsTupleThen[T, RESP], L] =
    instance(arr => restPositions(R)(arr.elements))

  // an array of values of one type, like HMGET's that may be missing, read position by position
  implicit final def arrOf2Tuple[E, L <: Tuple](implicit R: OfTupleRead[E, L]): Read[Arr.Of[E], L] = instance { arr =>
    if (arr.elements.size != R.size) Left(RESPDecErr(s"Read[Arr.Of] error: expected ${R.size} elements, got ${arr.elements.size}"))
    else R.read(arr.elements, 1).left.map(e => RESPDecErr(s"Read[Arr.Of] ${e.message}"))
  }

  // a cursor, then the values, if any
  implicit final def arrAsTuple2Scan[E, A](
      implicit R: TupleRead[Bulk *: OrNilArr[E] *: EmptyTuple, NonNegLong *: Option[Seq[A]] *: EmptyTuple]
  ): Read[Arr.AsTuple[Bulk *: OrNilArr[E] *: EmptyTuple], Scan[A]] = instance { arr =>
    TupleRead.read(R, "Arr.AsTuple")(arr.elements).map { case cursor *: values *: _ => Scan(cursor, values) }
  }
  implicit final val arrAsTuple2ScanKV: Read[Arr.AsTuple[Bulk *: OrNilArr[Arr.OfPairs[Bulk, Bulk]] *: EmptyTuple], ScanKV] =
    Read[Arr.AsTuple[Bulk *: OrNilArr[Arr.OfPairs[Bulk, Bulk]] *: EmptyTuple], Scan[(Key, String)]].map { scan =>
      ScanKV(scan.cursor, scan.values.map(_.map { case (k, v) => KV(k, v) }))
    }

  // a key, and what's under it
  implicit final def arrAsTuple2KV[E, A](
      implicit R: TupleRead[Bulk *: E *: EmptyTuple, Key *: A *: EmptyTuple]
  ): Read[Arr.AsTuple[Bulk *: E *: EmptyTuple], KV[A]] =
    instance(arr => TupleRead.read(R, "Arr.AsTuple")(arr.elements).map { case k *: a *: _ => KV(k, a) })

  implicit final val arrAsTuple2Time: Read[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple], Time] =
    Read[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple], NonNegLong *: NonNegLong *: EmptyTuple].map { case ts *: em *: _ => Time(ts, em) }
}
