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

import scala.annotation.{implicitNotFound, nowarn}

/** Evidence that `A` is one of the non-error [[RESP]] types Redis may reply with.
  *
  * Instances exist only for the concrete [[RESP]] cases, and for [[RESP]] itself as any of them (and never for [[Err]], which is always
  * handled as a failure), so a [[RESPRead]] can never be built for a type that Redis could not possibly send back. A value that may be
  * missing is a [[OneOf]] of its type and of a null, like [[laserdisc.OrNullBulk]] or [[laserdisc.OrNilArr]].
  */
@implicitNotFound(
  """Implicit not found RESPSub[${A}].

${A} must be one of the RESP types Arr, Arr.Of, Arr.Pairs, Arr.Rest, Arr.Tuple, Bulk, NilArr, NullBulk, Num or Str, a OneOf of them, or RESP for any of them
  """
) sealed abstract class RESPSub[A] {
  def unapply(resp: RESP): Option[A]
}

object RESPSub {
  @inline final def apply[A](implicit instance: RESPSub[A]): RESPSub[A] = instance

  private final def instance[A](pf: PartialFunction[RESP, A]): RESPSub[A] =
    new RESPSub[A] {
      override final def unapply(resp: RESP): Option[A] = pf.lift(resp)
    }

  implicit final val arrRESPSub: RESPSub[Arr]           = instance { case arr: Arr => arr }
  implicit final val bulkRESPSub: RESPSub[Bulk]         = instance { case bulk: Bulk => bulk }
  implicit final val nilArrRESPSub: RESPSub[NilArr]     = instance { case NilArr => NilArr }
  implicit final val nullBulkRESPSub: RESPSub[NullBulk] = instance { case NullBulk => NullBulk }
  implicit final val numRESPSub: RESPSub[Num]           = instance { case num: Num => num }
  implicit final val strRESPSub: RESPSub[Str]           = instance { case str: Str => str }

  // any array: its elements are checked as they're read
  implicit final def arrOfRESPSub[A]: RESPSub[Arr.Of[A]]             = instance { case Arr(elements) => new Arr.Of[A](elements) }
  implicit final def arrPairsRESPSub[A, B]: RESPSub[Arr.Pairs[A, B]] = instance { case Arr(elements) =>
    new Arr.Pairs[A, B](elements)
  }

  implicit final def arrTupleRESPSub[T]: RESPSub[Arr.Tuple[T]]     = instance { case Arr(elements) => new Arr.Tuple[T](elements) }
  implicit final def arrRestRESPSub[T, E]: RESPSub[Arr.Rest[T, E]] = instance { case Arr(elements) => new Arr.Rest[T, E](elements) }

  implicit final def oneOfRESPSub[T <: Tuple](implicit A: Alternatives[T], @nowarn D: Distinct[T]): RESPSub[OneOf[T]] = instance {
    case resp if A.matches(resp) => new OneOf[T](resp)
  }

  // any of the above, never Err
  implicit final val respRESPSub: RESPSub[RESP] = instance { case resp if !resp.isInstanceOf[Err] => resp }
}

@implicitNotFound(
  """Implicit not found RESPRead[${A}].

You should not need to define one manually, as one will be derived for you automatically iff:
- evidence of a Read instance from some RESP type to ${A} can be provided
- said RESP type is not Err (see RESPSub)
  """
) sealed trait RESPRead[A] {
  type Sub

  def read(resp: RESP): Maybe[A]
}

object RESPRead {
  final type Aux[Sub0, A] = RESPRead[A] { type Sub = Sub0 }
  final def apply[Sub, A](implicit instance: Aux[Sub, A]): Aux[Sub, A] = instance

  private final def mismatch(resp: RESP): Maybe[Nothing] = resp match {
    case err: Err => Left(err)
    case other    => Left(RESPDecErr(s"RESP type(s) did not match: $other"))
  }

  private final def readMatched[A, B](R: Read[A, B], resp: RESP, a: A): Maybe[B] = R.read(a) match {
    case Right(b)            => Right(b)
    case Left(RESPDecErr(m)) =>
      Left(RESPDecErr(s"RESP type(s) of $resp matched but failed to deserialize correctly with error $m")).widenLeft[Throwable]
  }

  final def instance[A, B](R: Read[A, B])(implicit S: RESPSub[A]): RESPRead.Aux[A, B] =
    new RESPRead[B] {
      override final type Sub = A
      override final def read(resp: RESP): Maybe[B] = resp match {
        case S(a) => readMatched(R, resp, a)
        case _    => mismatch(resp)
      }
    }

  implicit final def derive[A: RESPSub, B](implicit R: Read[A, B]): RESPRead.Aux[A, B] = instance(R)

  /** Reads OK as `None`, and a message as `Some`, for commands that reply OK when all is well and explain otherwise (e.g. ACL DRYRUN) */
  private[laserdisc] final val okOrMessage: RESPRead.Aux[RESP, Option[String]] =
    new RESPRead[Option[String]] {
      override final type Sub = RESP
      override final def read(resp: RESP): Maybe[Option[String]] = resp match {
        case Str("OK") => Right(None)
        case Bulk(s)   => Right(Some(s))
        case err: Err  => Left(err)
        case other     => Left(RESPDecErr(s"Expected OK or a message, but was $other"))
      }
    }

  /** Reads whatever Redis replies with as is ([[Err]] still being a failure), for commands without a dedicated [[Protocol]] (e.g. the CLI)
    */
  private[laserdisc] final val raw: RESPRead.Aux[RESP, RESP] =
    new RESPRead[RESP] {
      override final type Sub = RESP
      override final def read(resp: RESP): Maybe[RESP] = resp match {
        case err: Err => Left(err)
        case other    => Right(other)
      }
    }
}
