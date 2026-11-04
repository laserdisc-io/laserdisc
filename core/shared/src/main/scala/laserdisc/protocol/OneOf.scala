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

import scala.annotation.nowarn

/** A reply, an element or a field that may be any one of the RESP types in `T`, like `OneOf[NullBulk *: Bulk *: EmptyTuple]` for a value
  * that may be missing (see [[laserdisc.OrNullBulk]] and [[laserdisc.OrNilArr]]).
  *
  * It's read as `B` when each of its alternatives can be: the first alternative that matches the value and reads it wins, so alternatives
  * of the same RESP type, like several shapes of array, are told apart by what they hold. When `B` is an `Option`, the null alternatives
  * ([[NullBulk]], [[NilArr]]) read as `None`, and the others as `Some`.
  *
  * As in a union, each alternative is listed once: `OneOf[Bulk *: Bulk *: EmptyTuple]` doesn't compile. Alternatives may still overlap,
  * like two shapes of array: when both could read a value, the first one does.
  */
final class OneOf[T <: Tuple] private[protocol] (private[protocol] val value: RESP) {
  override final def toString: String = s"OneOf($value)"
}

/** Evidence that the alternatives of a [[OneOf]] are distinct types, as a union's are */
@scala.annotation.implicitNotFound("The alternatives of OneOf[${T}] must be distinct types: one of them is listed more than once")
sealed trait Distinct[T]

object Distinct {
  private final val instance: Distinct[Any] = new Distinct[Any] {}

  implicit final val emptyDistinct: Distinct[EmptyTuple] = instance.asInstanceOf[Distinct[EmptyTuple]]

  implicit final def consDistinct[H, T <: Tuple](implicit @nowarn H: NotIn[H, T], @nowarn T: Distinct[T]): Distinct[H *: T] =
    instance.asInstanceOf[Distinct[H *: T]]
}

/** Evidence that `H` isn't one of the types in `T` */
@scala.annotation.implicitNotFound("${H} is listed more than once in a OneOf")
sealed trait NotIn[H, T]

object NotIn extends NotInListedTwice {
  private final val instance: NotIn[Any, Any] = new NotIn[Any, Any] {}

  implicit final def emptyNotIn[H]: NotIn[H, EmptyTuple] = instance.asInstanceOf[NotIn[H, EmptyTuple]]

  implicit final def consNotIn[H, X, T <: Tuple](implicit @nowarn ev: H =:!= X, @nowarn T: NotIn[H, T]): NotIn[H, X *: T] =
    instance.asInstanceOf[NotIn[H, X *: T]]
}

/** The alternatives of a [[OneOf]], to match a value against */
sealed trait Alternatives[T] {
  def matches(resp: RESP): Boolean
}

object Alternatives {
  implicit final val emptyAlternatives: Alternatives[EmptyTuple] = new Alternatives[EmptyTuple] {
    override final def matches(resp: RESP): Boolean = false
  }

  implicit final def consAlternatives[H, T <: Tuple](implicit H: RESPSub[H], T: Alternatives[T]): Alternatives[H *: T] =
    new Alternatives[H *: T] {
      override final def matches(resp: RESP): Boolean = H.unapply(resp).isDefined || T.matches(resp)
    }
}

/** How one alternative of a [[OneOf]] is read as `B`: `None` if it doesn't match the value, or the result of reading it */
sealed trait AlternativeRead[H, B] {
  def read(resp: RESP): Option[Either[String, B]]
}

object AlternativeRead extends AlternativeReadPlain {
  implicit final def nullBulkNoneRead[A]: AlternativeRead[NullBulk, Option[A]] = AlternativeRead.nil(NullBulk)(Right(None))
  implicit final def nilArrNoneRead[A]: AlternativeRead[NilArr, Option[A]]     = AlternativeRead.nil(NilArr)(Right(None))

  private[protocol] final def nil[H, B](nil: RESP)(result: Either[String, B]): AlternativeRead[H, B] = new AlternativeRead[H, B] {
    override final def read(resp: RESP): Option[Either[String, B]] = if (resp == nil) Some(result) else None
  }

  private[protocol] final def of[H, B](S: RESPSub[H])(f: H => Either[RESPDecErr, B]): AlternativeRead[H, B] =
    new AlternativeRead[H, B] {
      override final def read(resp: RESP): Option[Either[String, B]] = S.unapply(resp).map(h => f(h).left.map(_.message))
    }
}

sealed trait AlternativeReadPlain extends AlternativeReadSome {
  implicit final def plainAlternativeRead[H, B](implicit S: RESPSub[H], R: Read[H, B]): AlternativeRead[H, B] =
    AlternativeRead.of(S)(R.read)
}

sealed trait AlternativeReadSome extends AlternativeReadMissing {
  implicit final def someAlternativeRead[H, A](implicit S: RESPSub[H], R: Read[H, A]): AlternativeRead[H, Option[A]] =
    AlternativeRead.of(S)(R.read(_).map(Some(_)))
}

// a null read as anything but an Option is a value that's missing
sealed trait AlternativeReadMissing {
  implicit final def nullBulkMissingRead[B]: AlternativeRead[NullBulk, B] = AlternativeRead.nil(NullBulk)(Left("missing value"))
  implicit final def nilArrMissingRead[B]: AlternativeRead[NilArr, B]     = AlternativeRead.nil(NilArr)(Left("missing value"))
}

/** How the alternatives of a [[OneOf]] are read as `B`, trying each in turn */
sealed trait AlternativesRead[T, B] {
  def read(resp: RESP, i: Int): Either[List[(Int, String)], B]
}

object AlternativesRead {
  implicit final def emptyAlternativesRead[B]: AlternativesRead[EmptyTuple, B] = new AlternativesRead[EmptyTuple, B] {
    override final def read(resp: RESP, i: Int): Either[List[(Int, String)], B] = Left(Nil)
  }

  implicit final def consAlternativesRead[H, T <: Tuple, B](
      implicit H: AlternativeRead[H, B],
      T: AlternativesRead[T, B]
  ): AlternativesRead[H *: T, B] = new AlternativesRead[H *: T, B] {
    override final def read(resp: RESP, i: Int): Either[List[(Int, String)], B] = H.read(resp) match {
      case Some(Right(b)) => Right(b)
      case Some(Left(e))  => T.read(resp, i + 1).left.map((i -> e) :: _)
      case None           => T.read(resp, i + 1)
    }
  }
}
