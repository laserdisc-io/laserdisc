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

/** How one element of an array is read: an `E` as an `A`, or with `Option[E]`, the "null" of `E` (see [[RESPSub]]) as `None` */
sealed trait ElementRead[E, A] {
  def read(resp: RESP): Either[String, A]
}

object ElementRead extends ElementReadLowPriority {
  implicit final def optionElementRead[E, A](implicit S: RESPSub[E], R: Read[E, A]): ElementRead[Option[E], Option[A]] =
    new ElementRead[Option[E], Option[A]] {
      override final def read(resp: RESP): Either[String, Option[A]] =
        if (resp == S.nil) Right(None) else ElementRead.read(S, R)(resp).map(Some(_))
    }

  private[protocol] final def read[E, A](S: RESPSub[E], R: Read[E, A])(resp: RESP): Either[String, A] = S.unapply(resp) match {
    case Some(e)               => R.read(e).left.map(_.message)
    case None if resp == S.nil => Left("missing value")
    case None                  => Left(s"Unexpected element. Was $resp")
  }
}

sealed trait ElementReadLowPriority {
  implicit final def elementRead[E, A](implicit S: RESPSub[E], R: Read[E, A]): ElementRead[E, A] = new ElementRead[E, A] {
    override final def read(resp: RESP): Either[String, A] = ElementRead.read(S, R)(resp)
  }
}

/** How the elements of an array are read position by position: the one at each position of `T` as the one at the same position of `L` */
sealed trait TupleRead[T, L] {
  def size: Int
  def read(elements: List[RESP], i: Int): Either[RESPDecErr, L]
}

object TupleRead {
  implicit final val emptyTupleRead: TupleRead[EmptyTuple, EmptyTuple] = new TupleRead[EmptyTuple, EmptyTuple] {
    override final val size: Int                                                          = 0
    override final def read(elements: List[RESP], i: Int): Either[RESPDecErr, EmptyTuple] = Right(EmptyTuple)
  }

  implicit final def consTupleRead[H, A, T <: Tuple, L <: Tuple](
      implicit H: ElementRead[H, A],
      T: TupleRead[T, L]
  ): TupleRead[H *: T, A *: L] = new TupleRead[H *: T, A *: L] {
    override final val size: Int                                                      = T.size + 1
    override final def read(elements: List[RESP], i: Int): Either[RESPDecErr, A *: L] = elements match {
      case resp :: rest =>
        H.read(resp) match {
          case Right(a) => T.read(rest, i + 1).map(a *: _)
          case Left(e)  => Left(RESPDecErr(s"error at element $i: $e"))
        }
      case Nil => Left(RESPDecErr(s"error at element $i: missing"))
    }
  }

  // all the elements, or how many there were when that's not as many as expected
  private[protocol] final def read[T, L](R: TupleRead[T, L], name: String)(elements: List[RESP]): Either[RESPDecErr, L] =
    if (elements.size != R.size) Left(RESPDecErr(s"Read[$name] error: expected ${R.size} elements, got ${elements.size}"))
    else R.read(elements, 1).left.map(e => RESPDecErr(s"Read[$name] ${e.message}"))
}

/** How the elements of an array whose elements are all `E`s, or their "null", are read position by position: as `Option[A]` where `L` says
  * so, with the "null" as `None`, and as `A` anywhere else, where the "null" is an error
  */
sealed trait NullableTupleRead[E, L] {
  def size: Int
  def read(elements: List[RESP], i: Int): Either[RESPDecErr, L]
}

object NullableTupleRead extends NullableTupleReadLowPriority {
  implicit final def emptyNullableTupleRead[E]: NullableTupleRead[E, EmptyTuple] = new NullableTupleRead[E, EmptyTuple] {
    override final val size: Int                                                          = 0
    override final def read(elements: List[RESP], i: Int): Either[RESPDecErr, EmptyTuple] = Right(EmptyTuple)
  }

  implicit final def optionNullableTupleRead[E, A, L <: Tuple](
      implicit H: ElementRead[Option[E], Option[A]],
      T: NullableTupleRead[E, L]
  ): NullableTupleRead[E, Option[A] *: L] = cons(H, T)

  private[protocol] final def cons[E, X, A, L <: Tuple](H: ElementRead[X, A], T: NullableTupleRead[E, L]): NullableTupleRead[E, A *: L] =
    new NullableTupleRead[E, A *: L] {
      override final val size: Int                                                      = T.size + 1
      override final def read(elements: List[RESP], i: Int): Either[RESPDecErr, A *: L] = elements match {
        case resp :: rest =>
          H.read(resp) match {
            case Right(a) => T.read(rest, i + 1).map(a *: _)
            case Left(e)  => Left(RESPDecErr(s"error at element $i: $e"))
          }
        case Nil => Left(RESPDecErr(s"error at element $i: missing"))
      }
    }
}

sealed trait NullableTupleReadLowPriority {
  implicit final def nullableTupleRead[E, A, L <: Tuple](
      implicit H: ElementRead[E, A],
      T: NullableTupleRead[E, L]
  ): NullableTupleRead[E, A *: L] = NullableTupleRead.cons(H, T)
}
