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

import Protocol.Single

object ListP {
  sealed trait Position
  object Position {
    object before extends Position
    object after  extends Position

    implicit val positionShow: Show[Position] = Show.instance {
      case `before` => "BEFORE"
      case `after`  => "AFTER"
    }
  }
}

trait ListBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "LINDEX",
      "LINSERT: AFTER BEFORE",
      "LLEN",
      "LPOP",
      "LPUSH",
      "LPUSHX",
      "LRANGE",
      "LREM",
      "LSET",
      "LTRIM",
      "RPOP",
      "RPOPLPUSH",
      "RPUSH",
      "RPUSHX"
    )

  object listtypes {
    final type ListPosition = ListP.Position

    final val ListPosition = ListP.Position
  }

  import listtypes.*

  private final val minusOneIsNone = RESPRead.instance(Read.numMinusOneIsNone[PosInt])
  private final val zeroIsNone     = RESPRead.instance(Read.numZeroIsNone[PosInt])

  final def lindex[A: Read[Bulk, _]](key: Key, index: Index): Single.Aux[Option[A]] =
    Protocol("LINDEX", key *: index *: EmptyTuple).asOpt[Bulk, A]

  final def linsert[A: Show](key: Key, position: ListPosition, pivot: A, value: A): Single.Aux[Option[PosInt]] =
    Protocol("LINSERT", key *: position *: pivot *: value *: EmptyTuple).using(minusOneIsNone)

  final def llen(key: Key): Single.Aux[NonNegInt] = Protocol("LLEN", key).as[Num, NonNegInt]

  final def lpop[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]] = Protocol("LPOP", key).asOpt[Bulk, A]

  final def lpush[A: Show](key: Key, values: OneOrMore[A]): Single.Aux[PosInt] =
    Protocol("LPUSH", key *: values.value *: EmptyTuple).as[Num, PosInt]

  final def lpushx[A: Show](key: Key, value: A): Single.Aux[Option[PosInt]] =
    Protocol("LPUSHX", key *: value *: EmptyTuple).using(zeroIsNone)

  final def lrange[A: Read[Bulk, _]](key: Key, start: Index, end: Index): Single.Aux[Seq[A]] =
    Protocol("LRANGE", key *: start *: end *: EmptyTuple).as[Arr, Seq[A]]

  final def lrem[A: Show](key: Key, count: Index, value: A): Single.Aux[NonNegInt] =
    Protocol("LREM", key *: count *: value *: EmptyTuple).as[Num, NonNegInt]

  final def lset[A: Show](key: Key, index: Index, value: A): Single.Aux[OK] =
    Protocol("LSET", key *: index *: value *: EmptyTuple).as[Str, OK]

  final def ltrim(key: Key, start: Index, stop: Index): Single.Aux[OK] = Protocol("LTRIM", key *: start *: stop *: EmptyTuple).as[Str, OK]

  final def rpop[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]] = Protocol("RPOP", key).asOpt[Bulk, A]

  final def rpoplpush[A: Read[Bulk, _]](source: Key, destination: Key): Single.Aux[Option[A]] =
    Protocol("RPOPLPUSH", source :: destination :: Nil).asOpt[Bulk, A]

  final def rpush[A: Show](key: Key, values: OneOrMore[A]): Single.Aux[PosInt] =
    Protocol("RPUSH", key *: values.value *: EmptyTuple).as[Num, PosInt]

  final def rpushx[A: Show](key: Key, value: A): Single.Aux[Option[PosInt]] =
    Protocol("RPUSHX", key *: value *: EmptyTuple).using(zeroIsNone)
}

trait ListP extends ListBaseP with ListExtP
