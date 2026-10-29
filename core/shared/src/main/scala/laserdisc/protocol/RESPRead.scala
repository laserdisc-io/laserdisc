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

/** Evidence that `A` is one of the non-error [[RESP]] types Redis may reply with.
  *
  * Instances exist only for the concrete [[RESP]] cases (and never for [[Err]], which is always handled as a failure), so a [[RESPRead]]
  * can never be built for a type that Redis could not possibly send back.
  *
  * Each instance also knows the "null" [[RESP]] that stands for a missing value of `A`: [[NilArr]] for [[Arr]], [[NullBulk]] for everything
  * else.
  */
@implicitNotFound(
  """Implicit not found RESPSub[${A}].

${A} must be one of the RESP types Arr, Bulk, NilArr, NullBulk, Num or Str
  """
) sealed abstract class RESPSub[A](val nil: RESP) {
  def unapply(resp: RESP): Option[A]
}

object RESPSub {
  @inline final def apply[A](implicit instance: RESPSub[A]): RESPSub[A] = instance

  private final def instance[A](nil: RESP)(pf: PartialFunction[RESP, A]): RESPSub[A] =
    new RESPSub[A](nil) {
      override final def unapply(resp: RESP): Option[A] = pf.lift(resp)
    }

  implicit final val arrRESPSub: RESPSub[Arr]           = instance(NilArr) { case arr: Arr => arr }
  implicit final val bulkRESPSub: RESPSub[Bulk]         = instance(NullBulk) { case bulk: Bulk => bulk }
  implicit final val nilArrRESPSub: RESPSub[NilArr]     = instance(NilArr) { case NilArr => NilArr }
  implicit final val nullBulkRESPSub: RESPSub[NullBulk] = instance(NullBulk) { case NullBulk => NullBulk }
  implicit final val numRESPSub: RESPSub[Num]           = instance(NullBulk) { case num: Num => num }
  implicit final val strRESPSub: RESPSub[Str]           = instance(NullBulk) { case str: Str => str }
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

  /** Like [[instance]], but the "null" [[RESP]] for `A` (see [[RESPSub]]) is read as `None` */
  final def nullable[A, B](R: Read[A, B])(implicit S: RESPSub[A]): RESPRead.Aux[A, Option[B]] =
    new RESPRead[Option[B]] {
      override final type Sub = A
      override final def read(resp: RESP): Maybe[Option[B]] = resp match {
        case S(a)                => readMatched(R, resp, a).map(Some(_))
        case nil if nil == S.nil => Right(None)
        case _                   => mismatch(resp)
      }
    }

  implicit final def derive[A: RESPSub, B](implicit R: Read[A, B]): RESPRead.Aux[A, B] = instance(R)

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
