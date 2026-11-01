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

  /** LEFT and RIGHT, for LMOVE, LMPOP, LMOVEM and their blocking versions */
  sealed trait Side
  object Side {
    object left  extends Side
    object right extends Side

    implicit val sideShow: Show[Side] = Show.instance {
      case `left`  => "LEFT"
      case `right` => "RIGHT"
    }
  }

  /** RANK and MAXLEN, for LPOS */
  sealed trait Search { def params: List[String] }
  object Search       {
    private final class Of(override final val params: List[String]) extends Search

    private def rankParams(rank: NonZeroInt) = List("RANK", Show[NonZeroInt].show(rank))
    private def maxLenParams(maxLen: PosInt) = List("MAXLEN", Show[PosInt].show(maxLen))

    def rank(rank: NonZeroInt): Search                          = new Of(rankParams(rank))
    def maxLen(maxLen: PosInt): Search                          = new Of(maxLenParams(maxLen))
    def rankAndMaxLen(rank: NonZeroInt, maxLen: PosInt): Search = new Of(rankParams(rank) ++ maxLenParams(maxLen))
  }

  /** COUNT and EXACTLY, for LMOVEM and BLMOVEM */
  sealed trait Amount { def params: List[String] }
  object Amount       {
    private final class Of(override final val params: List[String]) extends Amount

    def upTo(count: PosInt): Amount    = new Of(List("COUNT", Show[PosInt].show(count)))
    def exactly(count: PosInt): Amount = new Of(List("EXACTLY", Show[PosInt].show(count)))
  }

  /** OBO and BULK, for LMOVEM and BLMOVEM: one element at a time, or all of them as one block keeping their order */
  sealed trait Order
  object Order {
    object oneByOne extends Order
    object asBlock  extends Order

    implicit val orderShow: Show[Order] = Show.instance {
      case `oneByOne` => "OBO"
      case `asBlock`  => "BULK"
    }
  }

  private[protocol] final val positions: Read[Arr, Seq[NonNegInt]] = Read.instance { case Arr(vector) =>
    vector
      .foldRight[Either[RESPDecErr, List[NonNegInt]]](Right(Nil)) {
        case (Read.num2NonNegIntRead(Right(i)), Right(is)) => Right(i :: is)
        case (other, Right(_))                             => Left(RESPDecErr(s"Read[Arr(Num), Seq[NonNegInt]] error: unexpected $other"))
        case (_, left)                                     => left
      }
  }

  private[protocol] final def keyedSeq[A](implicit R: Read[Bulk, A]): RESPRead.Aux[Arr, Option[KV[Seq[A]]]] = {
    val as = Read.arrOfBulk2Seq[A]
    RESPRead.nullable(Read.instancePF("Arr(Key, Seq[A])") { case Arr(Bulk(Key(k)) +: as(Right(values)) +: Seq()) => KV(k, values) })
  }
}

trait ListBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "LINDEX",
      "LINSERT: AFTER BEFORE",
      "LLEN",
      "LMOVE: LEFT RIGHT",
      "LPOP",
      "LPOS: COUNT MAXLEN RANK",
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
    final type ListAmount   = ListP.Amount
    final type ListOrder    = ListP.Order
    final type ListPosition = ListP.Position
    final type ListSearch   = ListP.Search
    final type ListSide     = ListP.Side

    final val ListAmount   = ListP.Amount
    final val ListOrder    = ListP.Order
    final val ListPosition = ListP.Position
    final val ListSearch   = ListP.Search
    final val ListSide     = ListP.Side
  }

  import listtypes.*

  private final val minusOneIsNone = RESPRead.instance(Read.numMinusOneIsNone[PosInt])
  private final val zeroIsNone     = RESPRead.instance(Read.numZeroIsNone[PosInt])

  final def lindex[A: Read[Bulk, _]](key: Key, index: Index): Single.Aux[Option[A]] =
    Protocol("LINDEX", key *: index *: EmptyTuple).asOpt[Bulk, A]

  final def linsert[A: Show](key: Key, position: ListPosition, pivot: A, value: A): Single.Aux[Option[PosInt]] =
    Protocol("LINSERT", key *: position *: pivot *: value *: EmptyTuple).using(minusOneIsNone)

  final def llen(key: Key): Single.Aux[NonNegInt] = Protocol("LLEN", key).as[Num, NonNegInt]

  final def lmove[A: Read[Bulk, _]](source: Key, destination: Key, from: ListSide, to: ListSide): Single.Aux[Option[A]] =
    Protocol("LMOVE", source *: destination *: from *: to *: EmptyTuple).asOpt[Bulk, A]

  final def lpop[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]]                     = Protocol("LPOP", key).asOpt[Bulk, A]
  final def lpop[A: Read[Bulk, _]](key: Key, count: PosInt): Single.Aux[Option[Seq[A]]] =
    Protocol("LPOP", key *: count *: EmptyTuple).using(RESPRead.nullableArr(Read[Arr, Seq[A]]))

  final def lpos[A: Show](key: Key, element: A): Single.Aux[Option[NonNegInt]] =
    Protocol("LPOS", key *: element *: EmptyTuple).asOpt[Num, NonNegInt]
  final def lpos[A: Show](key: Key, element: A, search: ListSearch): Single.Aux[Option[NonNegInt]] =
    Protocol("LPOS", key *: element *: search.params *: EmptyTuple).asOpt[Num, NonNegInt]
  final def lpos[A: Show](key: Key, element: A, count: NonNegInt): Single.Aux[Seq[NonNegInt]] =
    Protocol("LPOS", key *: element *: "COUNT" *: count *: EmptyTuple).using(RESPRead.instance(ListP.positions))
  final def lpos[A: Show](key: Key, element: A, count: NonNegInt, search: ListSearch): Single.Aux[Seq[NonNegInt]] =
    Protocol("LPOS", key *: element *: "COUNT" *: count *: search.params *: EmptyTuple).using(RESPRead.instance(ListP.positions))

  final def lpush[A: Show](key: Key, values: OneOrMore[A]): Single.Aux[PosInt] =
    Protocol("LPUSH", key *: values.value *: EmptyTuple).as[Num, PosInt]

  final def lpushx[A: Show](key: Key, value: A): Single.Aux[Option[PosInt]] =
    Protocol("LPUSHX", key *: value *: EmptyTuple).using(zeroIsNone)
  final def lpushx[A: Show](key: Key, values: OneOrMore[A]): Single.Aux[Option[PosInt]] =
    Protocol("LPUSHX", key *: values.value *: EmptyTuple).using(zeroIsNone)

  final def lrange[A: Read[Bulk, _]](key: Key, start: Index, end: Index): Single.Aux[Seq[A]] =
    Protocol("LRANGE", key *: start *: end *: EmptyTuple).as[Arr, Seq[A]]

  final def lrem[A: Show](key: Key, count: Index, value: A): Single.Aux[NonNegInt] =
    Protocol("LREM", key *: count *: value *: EmptyTuple).as[Num, NonNegInt]

  final def lset[A: Show](key: Key, index: Index, value: A): Single.Aux[OK] =
    Protocol("LSET", key *: index *: value *: EmptyTuple).as[Str, OK]

  final def ltrim(key: Key, start: Index, stop: Index): Single.Aux[OK] = Protocol("LTRIM", key *: start *: stop *: EmptyTuple).as[Str, OK]

  final def rpop[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]]                     = Protocol("RPOP", key).asOpt[Bulk, A]
  final def rpop[A: Read[Bulk, _]](key: Key, count: PosInt): Single.Aux[Option[Seq[A]]] =
    Protocol("RPOP", key *: count *: EmptyTuple).using(RESPRead.nullableArr(Read[Arr, Seq[A]]))

  final def rpoplpush[A: Read[Bulk, _]](source: Key, destination: Key): Single.Aux[Option[A]] =
    Protocol("RPOPLPUSH", source :: destination :: Nil).asOpt[Bulk, A]

  final def rpush[A: Show](key: Key, values: OneOrMore[A]): Single.Aux[PosInt] =
    Protocol("RPUSH", key *: values.value *: EmptyTuple).as[Num, PosInt]

  final def rpushx[A: Show](key: Key, value: A): Single.Aux[Option[PosInt]] =
    Protocol("RPUSHX", key *: value *: EmptyTuple).using(zeroIsNone)
  final def rpushx[A: Show](key: Key, values: OneOrMore[A]): Single.Aux[Option[PosInt]] =
    Protocol("RPUSHX", key *: values.value *: EmptyTuple).using(zeroIsNone)
}

/** LMPOP: Redis 7.0 */
trait ListMPopP extends Features {
  import ListP.{Side, keyedSeq}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("LMPOP: COUNT LEFT RIGHT")

  final def lmpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, side: Side): Single.Aux[Option[KV[Seq[A]]]] =
    Protocol("LMPOP", keys.value.length *: keys.value *: side *: EmptyTuple).using(keyedSeq[A])
  final def lmpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, side: Side, count: PosInt): Single.Aux[Option[KV[Seq[A]]]] =
    Protocol("LMPOP", keys.value.length *: keys.value *: side *: "COUNT" *: count *: EmptyTuple).using(keyedSeq[A])
}

/** LMOVEM: Redis 8.10 */
trait ListMoveManyP extends Features {
  import ListP.{Amount, Order, Side}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("LMOVEM: BULK COUNT EXACTLY LEFT OBO RIGHT")

  final def lmovem[A: Read[Bulk, _]](
      source: Key,
      destination: Key,
      from: Side,
      to: Side,
      amount: Amount,
      order: Order
  ): Single.Aux[Option[Seq[A]]] =
    Protocol("LMOVEM", source *: destination *: from *: to *: amount.params *: order *: EmptyTuple).asOpt[Arr, Seq[A]]
}

trait ListP extends ListBaseP with ListExtP with ListMPopP with ListMoveManyP
