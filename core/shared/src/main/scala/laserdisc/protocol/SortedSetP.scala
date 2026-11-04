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

object SortedSetP {
  sealed trait Aggregate
  object Aggregate {
    object sum extends Aggregate
    object min extends Aggregate
    object max extends Aggregate

    final val count: AggregateCount = new AggregateCount

    implicit val aggregateShow: Show[Aggregate] = Show.instance {
      case `sum` => "SUM"
      case `min` => "MIN"
      case `max` => "MAX"
    }
  }

  /** AGGREGATE COUNT, which Redis added in 8.8 */
  final class AggregateCount private[SortedSetP] ()
  object AggregateCount {
    implicit val aggregateCountShow: Show[AggregateCount] = Show.const("COUNT")
  }

  /** NX, XX, GT and LT, for ZADD */
  sealed trait Flag { def params: List[String] }
  object Flag       {
    private final class Of(override final val params: List[String]) extends Flag

    final val nx: Flag   = new Of(List("NX"))
    final val xx: Flag   = new Of(List("XX"))
    final val gt: Flag   = new Of(List("GT"))
    final val lt: Flag   = new Of(List("LT"))
    final val xxGt: Flag = new Of(List("XX", "GT"))
    final val xxLt: Flag = new Of(List("XX", "LT"))
  }

  /** MIN and MAX, for ZMPOP and BZMPOP */
  sealed trait Pop
  object Pop {
    object min extends Pop
    object max extends Pop

    implicit val popShow: Show[Pop] = Show.instance {
      case `min` => "MIN"
      case `max` => "MAX"
    }
  }

  sealed trait LexRange { def min: String; def max: String }
  object LexRange       {
    private def open(s: String)  = s"($s"
    private def close(s: String) = s"[$s"
    private final val posInf     = "+"
    private final val negInf     = "-"

    sealed abstract class Open private[LexRange] (private val minimum: String, private val maximum: String) extends LexRange {
      override final val min: String = open(minimum)
      override final val max: String = open(maximum)
    }
    sealed abstract class OpenClosed private[LexRange] (private val minimum: String, private val maximum: String) extends LexRange {
      override final val min: String = open(minimum)
      override final val max: String = close(maximum)
    }
    sealed abstract class ClosedOpen private[LexRange] (private val minimum: String, private val maximum: String) extends LexRange {
      override final val min: String = close(minimum)
      override final val max: String = open(maximum)
    }
    sealed abstract class Closed private[LexRange] (private val minimum: String, private val maximum: String) extends LexRange {
      override final val min: String = close(minimum)
      override final val max: String = close(maximum)
    }
    sealed abstract class OpenInf private[LexRange] (private val minimum: String) extends LexRange {
      override final val min: String = open(minimum)
      override final val max: String = posInf
    }
    sealed abstract class ClosedInf private[LexRange] (private val minimum: String) extends LexRange {
      override final val min: String = close(minimum)
      override final val max: String = posInf
    }
    sealed abstract class InfOpen private[LexRange] (private val maximum: String) extends LexRange {
      override final val min: String = negInf
      override final val max: String = open(maximum)
    }
    sealed abstract class InfClosed private[LexRange] (private val maximum: String) extends LexRange {
      override final val min: String = negInf
      override final val max: String = close(maximum)
    }

    def open(min: Key, max: Key): LexRange       = new Open(min.value, max.value) {}
    def openClosed(min: Key, max: Key): LexRange = new OpenClosed(min.value, max.value) {}
    def closedOpen(min: Key, max: Key): LexRange = new ClosedOpen(min.value, max.value) {}
    def closed(min: Key, max: Key): LexRange     = new Closed(min.value, max.value) {}
    def openInf(min: Key): LexRange              = new OpenInf(min.value) {}
    def closedInf(min: Key): LexRange            = new ClosedInf(min.value) {}
    def infOpen(max: Key): LexRange              = new InfOpen(max.value) {}
    def infClosed(max: Key): LexRange            = new InfClosed(max.value) {}
  }

  sealed trait ScoreRange { def min: String; def max: String }
  object ScoreRange       {
    import Show.validDoubleShow

    private def open(d: ValidDouble)  = s"(${validDoubleShow.show(d)}"
    private def close(d: ValidDouble) = validDoubleShow.show(d)

    sealed abstract class Open private[ScoreRange] (private val minimum: ValidDouble, private val maximum: ValidDouble) extends ScoreRange {
      override final val min: String = open(minimum)
      override final val max: String = open(maximum)
    }
    sealed abstract class OpenClosed private[ScoreRange] (private val minimum: ValidDouble, private val maximum: ValidDouble)
        extends ScoreRange {
      override final val min: String = open(minimum)
      override final val max: String = close(maximum)
    }
    sealed abstract class ClosedOpen private[ScoreRange] (private val minimum: ValidDouble, private val maximum: ValidDouble)
        extends ScoreRange {
      override final val min: String = close(minimum)
      override final val max: String = open(maximum)
    }
    sealed abstract class Closed private[ScoreRange] (private val minimum: ValidDouble, private val maximum: ValidDouble)
        extends ScoreRange {
      override final val min: String = close(minimum)
      override final val max: String = close(maximum)
    }

    def open(min: ValidDouble, max: ValidDouble): ScoreRange       = new Open(min, max) {}
    def openClosed(min: ValidDouble, max: ValidDouble): ScoreRange = new OpenClosed(min, max) {}
    def closedOpen(min: ValidDouble, max: ValidDouble): ScoreRange = new ClosedOpen(min, max) {}
    def closed(min: ValidDouble, max: ValidDouble): ScoreRange     = new Closed(min, max) {}
  }

  private[protocol] final def firstScored[A: Read[Bulk, _]]: RESPRead.Aux[Arr.OfPairs[Bulk, Bulk], Option[(A, Double)]] =
    RESPRead.instance(Read[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]].map(_.headOption))

  private[protocol] final def keyedScored[
      A: Read[Bulk, _]
  ]: RESPRead.Aux[OrNilArr[Arr.AsTuple[Bulk *: Bulk *: Bulk *: EmptyTuple]], Option[KV[(A, Double)]]] = {
    implicit val scored: Read[Arr.AsTuple[Bulk *: Bulk *: Bulk *: EmptyTuple], KV[(A, Double)]] =
      Read[Arr.AsTuple[Bulk *: Bulk *: Bulk *: EmptyTuple], Key *: A *: Double *: EmptyTuple].map { case k *: a *: score *: _ =>
        KV(k, a -> score)
      }
    RESPRead[OrNilArr[Arr.AsTuple[Bulk *: Bulk *: Bulk *: EmptyTuple]], Option[KV[(A, Double)]]]
  }

  private[protocol] final def keyedScoredSeq[
      A: Read[Bulk, _]
  ]: RESPRead.Aux[OrNilArr[Arr.AsTuple[Bulk *: Arr.Of[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple]] *: EmptyTuple]], Option[KV[Seq[(A, Double)]]]] =
    RESPRead[OrNilArr[Arr.AsTuple[Bulk *: Arr.Of[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple]] *: EmptyTuple]], Option[KV[Seq[(A, Double)]]]]

  private[protocol] final val rankWithScore: RESPRead.Aux[OrNilArr[Arr.AsTuple[Num *: Bulk *: EmptyTuple]], Option[(NonNegInt, Double)]] =
    RESPRead[OrNilArr[Arr.AsTuple[Num *: Bulk *: EmptyTuple]], Option[(NonNegInt, Double)]]
}

trait SortedSetBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "ZADD: CH GT INCR LT NX XX",
      "ZCARD",
      "ZCOUNT",
      "ZDIFF: WITHSCORES",
      "ZDIFFSTORE",
      "ZINCRBY",
      "ZINTER: AGGREGATE MAX MIN SUM WEIGHTS WITHSCORES",
      "ZINTERSTORE: AGGREGATE MAX MIN SUM WEIGHTS",
      "ZLEXCOUNT",
      "ZMSCORE",
      "ZPOPMAX",
      "ZPOPMIN",
      "ZRANDMEMBER: WITHSCORES",
      "ZRANGE: WITHSCORES",
      "ZRANGEBYLEX: LIMIT",
      "ZRANGEBYSCORE: LIMIT WITHSCORES",
      "ZRANGESTORE: BYLEX BYSCORE LIMIT REV",
      "ZRANK",
      "ZREM",
      "ZREMRANGEBYLEX",
      "ZREMRANGEBYRANK",
      "ZREMRANGEBYSCORE",
      "ZREVRANGE: WITHSCORES",
      "ZREVRANGEBYLEX: LIMIT",
      "ZREVRANGEBYSCORE: LIMIT WITHSCORES",
      "ZREVRANK",
      "ZSCAN: COUNT MATCH",
      "ZSCORE",
      "ZUNION: AGGREGATE MAX MIN SUM WEIGHTS WITHSCORES",
      "ZUNIONSTORE: AGGREGATE MAX MIN SUM WEIGHTS"
    )

  import SortedSetP.{Aggregate, Flag, LexRange, Pop, ScoreRange, firstScored}

  private final val zeroIsNone = RESPRead.instance(Read.numZeroIsNone[PosInt])

  object sortedsettypes {
    final val aggregate  = Aggregate
    final val flag       = Flag
    final val lexrange   = LexRange
    final val pop        = Pop
    final val scorerange = ScoreRange
  }

  final def zadd[A: Show](key: Key, scoredMembers: OneOrMore[(A, ValidDouble)]): Single.Aux[NonNegInt] =
    Protocol("ZADD", key *: scoredMembers.value.map(_.swap) *: EmptyTuple).as[Num, NonNegInt]
  final def zadd[A: Show](key: Key, flag: Flag, scoredMembers: OneOrMore[(A, ValidDouble)]): Single.Aux[NonNegInt] =
    Protocol("ZADD", key *: flag.params *: scoredMembers.value.map(_.swap) *: EmptyTuple).as[Num, NonNegInt]

  final def zaddch[A: Show](key: Key, scoredMembers: OneOrMore[(A, ValidDouble)]): Single.Aux[NonNegInt] =
    Protocol("ZADD", key *: "CH" *: scoredMembers.value.map(_.swap) *: EmptyTuple).as[Num, NonNegInt]
  final def zaddch[A: Show](key: Key, flag: Flag, scoredMembers: OneOrMore[(A, ValidDouble)]): Single.Aux[NonNegInt] =
    Protocol("ZADD", key *: flag.params *: "CH" *: scoredMembers.value.map(_.swap) *: EmptyTuple).as[Num, NonNegInt]

  final def zaddincr[A: Show](key: Key, member: A, increment: NonZeroDouble): Single.Aux[Double] =
    Protocol("ZADD", key *: "INCR" *: increment *: member *: EmptyTuple).as[Bulk, Double]
  final def zaddincr[A: Show](key: Key, flag: Flag, member: A, increment: NonZeroDouble): Single.Aux[Option[Double]] =
    Protocol("ZADD", key *: flag.params *: "INCR" *: increment *: member *: EmptyTuple).as[OrNullBulk[Bulk], Option[Double]]

  final def zaddchincr[A: Show](key: Key, member: A, increment: NonZeroDouble): Single.Aux[Double] =
    Protocol("ZADD", key *: "CH" *: "INCR" *: increment *: member *: EmptyTuple).as[Bulk, Double]
  final def zaddchincr[A: Show](key: Key, flag: Flag, member: A, increment: NonZeroDouble): Single.Aux[Option[Double]] =
    Protocol("ZADD", key *: flag.params *: "CH" *: "INCR" *: increment *: member *: EmptyTuple).as[OrNullBulk[Bulk], Option[Double]]

  final def zcard(key: Key): Single.Aux[Option[PosInt]] = Protocol("ZCARD", key *: EmptyTuple).using(zeroIsNone)

  final def zcount(key: Key, range: ScoreRange): Single.Aux[NonNegInt] =
    Protocol("ZCOUNT", key *: range.min *: range.max *: EmptyTuple).as[Num, NonNegInt]

  final def zdiff[A: Read[Bulk, _]](keys: TwoOrMoreKeys): Single.Aux[Seq[A]] =
    Protocol("ZDIFF", keys.value.length *: keys.value *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def zdiffwithscores[A: Read[Bulk, _]](keys: TwoOrMoreKeys): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZDIFF", keys.value.length *: keys.value *: "WITHSCORES" *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]

  final def zdiffstore(keys: TwoOrMoreKeys, destinationKey: Key): Single.Aux[NonNegInt] =
    Protocol("ZDIFFSTORE", destinationKey *: keys.value.length *: keys.value *: EmptyTuple).as[Num, NonNegInt]

  final def zincrby[A: Show](key: Key, member: A, increment: ValidDouble): Single.Aux[Double] =
    Protocol("ZINCRBY", key *: increment *: member *: EmptyTuple).as[Bulk, Double]

  final def zinter[A: Read[Bulk, _]](keys: TwoOrMoreKeys): Single.Aux[Seq[A]] =
    Protocol("ZINTER", keys.value.length *: keys.value *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def zinter[A: Read[Bulk, _]](keys: TwoOrMoreKeys, aggregate: Aggregate): Single.Aux[Seq[A]] =
    Protocol("ZINTER", keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]

  final def zinterweighted[A: Read[Bulk, _]](weightedKeys: TwoOrMoreWeightedKeys): Single.Aux[Seq[A]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZINTER", keys.length *: keys *: "WEIGHTS" *: weights *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  }
  final def zinterweighted[A: Read[Bulk, _]](weightedKeys: TwoOrMoreWeightedKeys, aggregate: Aggregate): Single.Aux[Seq[A]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZINTER", keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  }

  final def zinterwithscores[A: Read[Bulk, _]](keys: TwoOrMoreKeys): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZINTER", keys.value.length *: keys.value *: "WITHSCORES" *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  final def zinterwithscores[A: Read[Bulk, _]](keys: TwoOrMoreKeys, aggregate: Aggregate): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZINTER", keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: "WITHSCORES" *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]

  final def zinterweightedwithscores[A: Read[Bulk, _]](weightedKeys: TwoOrMoreWeightedKeys): Single.Aux[Seq[(A, Double)]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZINTER", keys.length *: keys *: "WEIGHTS" *: weights *: "WITHSCORES" *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  }
  final def zinterweightedwithscores[A: Read[Bulk, _]](
      weightedKeys: TwoOrMoreWeightedKeys,
      aggregate: Aggregate
  ): Single.Aux[Seq[(A, Double)]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZINTER", keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: "WITHSCORES" *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  }

  final def zinterstore(keys: TwoOrMoreKeys, destinationKey: Key): Single.Aux[NonNegInt] =
    Protocol("ZINTERSTORE", destinationKey *: keys.value.length *: keys.value *: EmptyTuple).as[Num, NonNegInt]

  final def zinterstoreweighted(weightedKeys: TwoOrMoreWeightedKeys, destinationKey: Key): Single.Aux[NonNegInt] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZINTERSTORE", destinationKey *: keys.length *: keys *: "WEIGHTS" *: weights *: EmptyTuple).as[Num, NonNegInt]
  }

  final def zinterstore(keys: TwoOrMoreKeys, destinationKey: Key, aggregate: Aggregate): Single.Aux[NonNegInt] =
    Protocol("ZINTERSTORE", destinationKey *: keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Num, NonNegInt]

  final def zinterstoreweighted(weightedKeys: TwoOrMoreWeightedKeys, destinationKey: Key, aggregate: Aggregate): Single.Aux[NonNegInt] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZINTERSTORE", destinationKey *: keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: EmptyTuple)
      .as[Num, NonNegInt]
  }

  final def zlexcount(key: Key, range: LexRange): Single.Aux[NonNegInt] =
    Protocol("ZLEXCOUNT", key *: range.min *: range.max *: EmptyTuple).as[Num, NonNegInt]

  final def zmscore[A: Show](key: Key, members: OneOrMore[A]): Single.Aux[Seq[Option[Double]]] =
    Protocol("ZMSCORE", key *: members.value *: EmptyTuple).as[Arr.Of[OrNullBulk[Bulk]], Seq[Option[Double]]]

  final def zpopmax[A: Read[Bulk, _]](key: Key): Single.Aux[Option[(A, Double)]] = Protocol("ZPOPMAX", key).using(firstScored[A])
  final def zpopmax[A: Read[Bulk, _]](key: Key, count: PosInt): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZPOPMAX", key *: count *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]

  final def zpopmin[A: Read[Bulk, _]](key: Key): Single.Aux[Option[(A, Double)]] = Protocol("ZPOPMIN", key).using(firstScored[A])
  final def zpopmin[A: Read[Bulk, _]](key: Key, count: PosInt): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZPOPMIN", key *: count *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]

  final def zrandmember[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]] = Protocol("ZRANDMEMBER", key).as[OrNullBulk[Bulk], Option[A]]
  final def zrandmembers[A: Read[Bulk, _]](key: Key, count: NonZeroInt): Single.Aux[Seq[A]] =
    Protocol("ZRANDMEMBER", key *: count *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def zrandmemberswithscores[A: Read[Bulk, _]](key: Key, count: NonZeroInt): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZRANDMEMBER", key *: count *: "WITHSCORES" *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]

  final def zrange[A: Read[Bulk, _]](key: Key, start: Index, stop: Index): Single.Aux[Seq[A]] =
    Protocol("ZRANGE", key *: start *: stop *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def zrangewithscores[A: Read[Bulk, _]](key: Key, start: Index, stop: Index): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZRANGE", key *: start *: stop *: "WITHSCORES" *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]

  final def zrangebylex[A: Read[Bulk, _]](key: Key, range: LexRange): Single.Aux[Seq[A]] =
    Protocol("ZRANGEBYLEX", key *: range.min *: range.max *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]

  final def zrangebylex[A: Read[Bulk, _]](key: Key, range: LexRange, offset: NonNegLong, count: PosLong): Single.Aux[Seq[A]] =
    Protocol("ZRANGEBYLEX", key *: range.min *: range.max *: "LIMIT" *: offset *: count *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]

  final def zrangebyscore[A: Read[Bulk, _]](key: Key, range: ScoreRange): Single.Aux[Seq[A]] =
    Protocol("ZRANGEBYSCORE", key *: range.min *: range.max *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def zrangebyscore[A: Read[Bulk, _]](key: Key, range: ScoreRange, offset: NonNegLong, count: PosLong): Single.Aux[Seq[A]] =
    Protocol("ZRANGEBYSCORE", key *: range.min *: range.max *: "LIMIT" *: offset *: count *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]

  final def zrangebyscorewithscores[A: Read[Bulk, _]](key: Key, range: ScoreRange): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZRANGEBYSCORE", key *: range.min *: range.max *: "WITHSCORES" *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  final def zrangebyscorewithscores[A: Read[Bulk, _]](
      key: Key,
      range: ScoreRange,
      offset: NonNegLong,
      count: PosLong
  ): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZRANGEBYSCORE", key *: range.min *: range.max *: "WITHSCORES" *: "LIMIT" *: offset *: count *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]

  final def zrangestore(source: Key, destination: Key, start: Index, stop: Index): Single.Aux[NonNegInt] =
    Protocol("ZRANGESTORE", destination *: source *: start *: stop *: EmptyTuple).as[Num, NonNegInt]
  final def zrangestore(source: Key, destination: Key, range: ScoreRange): Single.Aux[NonNegInt] =
    Protocol("ZRANGESTORE", destination *: source *: range.min *: range.max *: "BYSCORE" *: EmptyTuple).as[Num, NonNegInt]
  final def zrangestore(source: Key, destination: Key, range: ScoreRange, offset: NonNegLong, count: PosLong): Single.Aux[NonNegInt] =
    Protocol("ZRANGESTORE", destination *: source *: range.min *: range.max *: "BYSCORE" *: "LIMIT" *: offset *: count *: EmptyTuple)
      .as[Num, NonNegInt]
  final def zrangestore(source: Key, destination: Key, range: LexRange): Single.Aux[NonNegInt] =
    Protocol("ZRANGESTORE", destination *: source *: range.min *: range.max *: "BYLEX" *: EmptyTuple).as[Num, NonNegInt]
  final def zrangestore(source: Key, destination: Key, range: LexRange, offset: NonNegLong, count: PosLong): Single.Aux[NonNegInt] =
    Protocol("ZRANGESTORE", destination *: source *: range.min *: range.max *: "BYLEX" *: "LIMIT" *: offset *: count *: EmptyTuple)
      .as[Num, NonNegInt]

  final def zrank(key: Key, member: Key): Single.Aux[Option[NonNegInt]] =
    Protocol("ZRANK", key :: member :: Nil).as[OrNullBulk[Num], Option[NonNegInt]]

  final def zrem[A: Show](key: Key, members: OneOrMore[A]): Single.Aux[NonNegInt] =
    Protocol("ZREM", key *: members.value *: EmptyTuple).as[Num, NonNegInt]

  final def zremrangebylex(key: Key, range: LexRange): Single.Aux[NonNegInt] =
    Protocol("ZREMRANGEBYLEX", key *: range.min *: range.max *: EmptyTuple).as[Num, NonNegInt]

  final def zremrangebyrank(key: Key, start: Index, stop: Index): Single.Aux[NonNegInt] =
    Protocol("ZREMRANGEBYRANK", key *: start *: stop *: EmptyTuple).as[Num, NonNegInt]

  final def zremrangebyscore(key: Key, range: ScoreRange): Single.Aux[NonNegInt] =
    Protocol("ZREMRANGEBYSCORE", key *: range.min *: range.max *: EmptyTuple).as[Num, NonNegInt]

  final def zrevrange[A: Read[Bulk, _]](key: Key, start: Index, stop: Index): Single.Aux[Seq[A]] =
    Protocol("ZREVRANGE", key *: start *: stop *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def zrevrangewithscores[A: Read[Bulk, _]](key: Key, start: Index, stop: Index): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZREVRANGE", key *: start *: stop *: "WITHSCORES" *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]

  final def zrevrangebylex[A: Read[Bulk, _]](key: Key, range: LexRange): Single.Aux[Seq[A]] =
    Protocol("ZREVRANGEBYLEX", key *: range.max *: range.min *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]

  final def zrevrangebylex[A: Read[Bulk, _]](key: Key, range: LexRange, offset: NonNegLong, count: PosLong): Single.Aux[Seq[A]] =
    Protocol("ZREVRANGEBYLEX", key *: range.max *: range.min *: "LIMIT" *: offset *: count *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]

  final def zrevrangebyscore[A: Read[Bulk, _]](key: Key, range: ScoreRange): Single.Aux[Seq[A]] =
    Protocol("ZREVRANGEBYSCORE", key *: range.max *: range.min *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def zrevrangebyscore[A: Read[Bulk, _]](key: Key, range: ScoreRange, offset: NonNegLong, count: PosLong): Single.Aux[Seq[A]] =
    Protocol("ZREVRANGEBYSCORE", key *: range.max *: range.min *: "LIMIT" *: offset *: count *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]

  final def zrevrangebyscorewithscores[A: Read[Bulk, _]](key: Key, range: ScoreRange): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZREVRANGEBYSCORE", key *: range.max *: range.min *: "WITHSCORES" *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  final def zrevrangebyscorewithscores[A: Read[Bulk, _]](
      key: Key,
      range: ScoreRange,
      offset: NonNegLong,
      count: PosLong
  ): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZREVRANGEBYSCORE", key *: range.max *: range.min *: "WITHSCORES" *: "LIMIT" *: offset *: count *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]

  final def zrevrangestore(source: Key, destination: Key, start: Index, stop: Index): Single.Aux[NonNegInt] =
    Protocol("ZRANGESTORE", destination *: source *: start *: stop *: "REV" *: EmptyTuple).as[Num, NonNegInt]
  final def zrevrangestore(source: Key, destination: Key, range: ScoreRange): Single.Aux[NonNegInt] =
    Protocol("ZRANGESTORE", destination *: source *: range.max *: range.min *: "BYSCORE" *: "REV" *: EmptyTuple).as[Num, NonNegInt]
  final def zrevrangestore(source: Key, destination: Key, range: ScoreRange, offset: NonNegLong, count: PosLong): Single.Aux[NonNegInt] =
    Protocol(
      "ZRANGESTORE",
      destination *: source *: range.max *: range.min *: "BYSCORE" *: "REV" *: "LIMIT" *: offset *: count *: EmptyTuple
    )
      .as[Num, NonNegInt]
  final def zrevrangestore(source: Key, destination: Key, range: LexRange): Single.Aux[NonNegInt] =
    Protocol("ZRANGESTORE", destination *: source *: range.max *: range.min *: "BYLEX" *: "REV" *: EmptyTuple).as[Num, NonNegInt]
  final def zrevrangestore(source: Key, destination: Key, range: LexRange, offset: NonNegLong, count: PosLong): Single.Aux[NonNegInt] =
    Protocol("ZRANGESTORE", destination *: source *: range.max *: range.min *: "BYLEX" *: "REV" *: "LIMIT" *: offset *: count *: EmptyTuple)
      .as[Num, NonNegInt]

  final def zrevrank(key: Key, member: Key): Single.Aux[Option[NonNegInt]] =
    Protocol("ZREVRANK", key *: member *: EmptyTuple).as[OrNullBulk[Num], Option[NonNegInt]]

  final def zscan[A: Read[Bulk, _]](key: Key, cursor: NonNegLong): Single.Aux[Scan[(A, Double)]] =
    Protocol("ZSCAN", key *: cursor *: EmptyTuple).as[ScanOf[Arr.OfPairs[Bulk, Bulk]], Scan[(A, Double)]]
  final def zscan[A: Read[Bulk, _]](key: Key, cursor: NonNegLong, pattern: GlobPattern): Single.Aux[Scan[(A, Double)]] =
    Protocol("ZSCAN", key *: cursor *: "MATCH" *: pattern *: EmptyTuple).as[ScanOf[Arr.OfPairs[Bulk, Bulk]], Scan[(A, Double)]]
  final def zscan[A: Read[Bulk, _]](key: Key, cursor: NonNegLong, count: PosInt): Single.Aux[Scan[(A, Double)]] =
    Protocol("ZSCAN", key *: cursor *: "COUNT" *: count *: EmptyTuple).as[ScanOf[Arr.OfPairs[Bulk, Bulk]], Scan[(A, Double)]]
  final def zscan[A: Read[Bulk, _]](key: Key, cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Single.Aux[Scan[(A, Double)]] =
    Protocol("ZSCAN", key *: cursor *: "MATCH" *: pattern *: "COUNT" *: count *: EmptyTuple)
      .as[ScanOf[Arr.OfPairs[Bulk, Bulk]], Scan[(A, Double)]]

  /** ZSCAN from cursor 0 until the server is done, as a [[Loop]]. A member may come more than once */
  final def zscanAll[A: Read[Bulk, _]](key: Key): Loop[(A, Double)] = Loop.cursor(zscan[A](key, _))(scanned[(A, Double)])
  final def zscanAll[A: Read[Bulk, _]](key: Key, pattern: GlobPattern): Loop[(A, Double)] =
    Loop.cursor(zscan[A](key, _, pattern))(scanned[(A, Double)])
  final def zscanAll[A: Read[Bulk, _]](key: Key, count: PosInt): Loop[(A, Double)] =
    Loop.cursor(zscan[A](key, _, count))(scanned[(A, Double)])
  final def zscanAll[A: Read[Bulk, _]](key: Key, pattern: GlobPattern, count: PosInt): Loop[(A, Double)] =
    Loop.cursor(zscan[A](key, _, pattern, count))(scanned[(A, Double)])

  final def zscore[A: Show](key: Key, member: A): Single.Aux[Option[Double]] =
    Protocol("ZSCORE", key *: member *: EmptyTuple).as[OrNullBulk[Bulk], Option[Double]]

  final def zunion[A: Read[Bulk, _]](keys: TwoOrMoreKeys): Single.Aux[Seq[A]] =
    Protocol("ZUNION", keys.value.length *: keys.value *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def zunion[A: Read[Bulk, _]](keys: TwoOrMoreKeys, aggregate: Aggregate): Single.Aux[Seq[A]] =
    Protocol("ZUNION", keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]

  final def zunionweighted[A: Read[Bulk, _]](weightedKeys: TwoOrMoreWeightedKeys): Single.Aux[Seq[A]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZUNION", keys.length *: keys *: "WEIGHTS" *: weights *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  }
  final def zunionweighted[A: Read[Bulk, _]](weightedKeys: TwoOrMoreWeightedKeys, aggregate: Aggregate): Single.Aux[Seq[A]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZUNION", keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  }

  final def zunionwithscores[A: Read[Bulk, _]](keys: TwoOrMoreKeys): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZUNION", keys.value.length *: keys.value *: "WITHSCORES" *: EmptyTuple).as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  final def zunionwithscores[A: Read[Bulk, _]](keys: TwoOrMoreKeys, aggregate: Aggregate): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZUNION", keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: "WITHSCORES" *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]

  final def zunionweightedwithscores[A: Read[Bulk, _]](weightedKeys: TwoOrMoreWeightedKeys): Single.Aux[Seq[(A, Double)]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZUNION", keys.length *: keys *: "WEIGHTS" *: weights *: "WITHSCORES" *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  }
  final def zunionweightedwithscores[A: Read[Bulk, _]](
      weightedKeys: TwoOrMoreWeightedKeys,
      aggregate: Aggregate
  ): Single.Aux[Seq[(A, Double)]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZUNION", keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: "WITHSCORES" *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  }

  final def zunionstore(keys: TwoOrMoreKeys, destinationKey: Key): Single.Aux[NonNegInt] =
    Protocol("ZUNIONSTORE", destinationKey *: keys.value.length *: keys.value *: EmptyTuple).as[Num, NonNegInt]

  final def zunionstoreweighted(weightedKeys: TwoOrMoreWeightedKeys, destinationKey: Key): Single.Aux[NonNegInt] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZUNIONSTORE", destinationKey *: keys.length *: keys *: "WEIGHTS" *: weights *: EmptyTuple).as[Num, NonNegInt]
  }

  final def zunionstore(keys: TwoOrMoreKeys, destinationKey: Key, aggregate: Aggregate): Single.Aux[NonNegInt] =
    Protocol("ZUNIONSTORE", destinationKey *: keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Num, NonNegInt]

  final def zunionstoreweighted(weightedKeys: TwoOrMoreWeightedKeys, destinationKey: Key, aggregate: Aggregate): Single.Aux[NonNegInt] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZUNIONSTORE", destinationKey *: keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: EmptyTuple)
      .as[Num, NonNegInt]
  }
}

/** ZINTERCARD: Redis 7.0 */
trait SortedSetInterCardP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("ZINTERCARD: LIMIT")

  final def zintercard(keys: TwoOrMoreKeys): Single.Aux[NonNegInt] =
    Protocol("ZINTERCARD", keys.value.length *: keys.value *: EmptyTuple).as[Num, NonNegInt]
  final def zintercard(keys: TwoOrMoreKeys, limit: PosInt): Single.Aux[NonNegInt] =
    Protocol("ZINTERCARD", keys.value.length *: keys.value *: "LIMIT" *: limit *: EmptyTuple).as[Num, NonNegInt]
}

/** ZMPOP: Redis 7.0 */
trait SortedSetMPopP extends Features {
  import SortedSetP.{Pop, keyedScoredSeq}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("ZMPOP: COUNT MAX MIN")

  final def zmpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, pop: Pop): Single.Aux[Option[KV[Seq[(A, Double)]]]] =
    Protocol("ZMPOP", keys.value.length *: keys.value *: pop *: EmptyTuple).using(keyedScoredSeq[A])
  final def zmpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, pop: Pop, count: PosInt): Single.Aux[Option[KV[Seq[(A, Double)]]]] =
    Protocol("ZMPOP", keys.value.length *: keys.value *: pop *: "COUNT" *: count *: EmptyTuple).using(keyedScoredSeq[A])
}

/** ZRANK and ZREVRANK with WITHSCORE: Redis 7.2 */
trait SortedSetRankWithScoreP extends Features {
  import SortedSetP.rankWithScore

  override private[laserdisc] def supported: List[String] = super.supported ++ List("ZRANK: WITHSCORE", "ZREVRANK: WITHSCORE")

  final def zrankwithscore[A: Show](key: Key, member: A): Single.Aux[Option[(NonNegInt, Double)]] =
    Protocol("ZRANK", key *: member *: "WITHSCORE" *: EmptyTuple).using(rankWithScore)

  final def zrevrankwithscore[A: Show](key: Key, member: A): Single.Aux[Option[(NonNegInt, Double)]] =
    Protocol("ZREVRANK", key *: member *: "WITHSCORE" *: EmptyTuple).using(rankWithScore)
}

/** ZSCAN with NOSCORES: Valkey 8.0 */
trait SortedSetScanNoScoresP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("ZSCAN: NOSCORES")

  final def zscannoscores[A: Read[Bulk, _]](key: Key, cursor: NonNegLong): Single.Aux[Scan[A]] =
    Protocol("ZSCAN", key *: cursor *: "NOSCORES" *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[A]]
  final def zscannoscores[A: Read[Bulk, _]](key: Key, cursor: NonNegLong, pattern: GlobPattern): Single.Aux[Scan[A]] =
    Protocol("ZSCAN", key *: cursor *: "MATCH" *: pattern *: "NOSCORES" *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[A]]
  final def zscannoscores[A: Read[Bulk, _]](key: Key, cursor: NonNegLong, count: PosInt): Single.Aux[Scan[A]] =
    Protocol("ZSCAN", key *: cursor *: "COUNT" *: count *: "NOSCORES" *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[A]]
  final def zscannoscores[A: Read[Bulk, _]](key: Key, cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Single.Aux[Scan[A]] =
    Protocol("ZSCAN", key *: cursor *: "MATCH" *: pattern *: "COUNT" *: count *: "NOSCORES" *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[A]]

  /** ZSCAN with NOSCORES from cursor 0 until the server is done, as a [[Loop]]. A member may come more than once */
  final def zscannoscoresAll[A: Read[Bulk, _]](key: Key): Loop[A]                       = Loop.cursor(zscannoscores[A](key, _))(scanned[A])
  final def zscannoscoresAll[A: Read[Bulk, _]](key: Key, pattern: GlobPattern): Loop[A] =
    Loop.cursor(zscannoscores[A](key, _, pattern))(scanned[A])
  final def zscannoscoresAll[A: Read[Bulk, _]](key: Key, count: PosInt): Loop[A] = Loop.cursor(zscannoscores[A](key, _, count))(scanned[A])
  final def zscannoscoresAll[A: Read[Bulk, _]](key: Key, pattern: GlobPattern, count: PosInt): Loop[A] =
    Loop.cursor(zscannoscores[A](key, _, pattern, count))(scanned[A])
}

/** AGGREGATE COUNT for ZINTER, ZINTERSTORE, ZUNION and ZUNIONSTORE: Redis 8.8 */
trait SortedSetAggregateCountP extends Features {
  import SortedSetP.AggregateCount

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "ZINTER: AGGREGATE COUNT",
      "ZINTERSTORE: AGGREGATE COUNT",
      "ZUNION: AGGREGATE COUNT",
      "ZUNIONSTORE: AGGREGATE COUNT"
    )

  final def zinter[A: Read[Bulk, _]](keys: TwoOrMoreKeys, aggregate: AggregateCount): Single.Aux[Seq[A]] =
    Protocol("ZINTER", keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def zinterweighted[A: Read[Bulk, _]](weightedKeys: TwoOrMoreWeightedKeys, aggregate: AggregateCount): Single.Aux[Seq[A]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZINTER", keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  }
  final def zinterwithscores[A: Read[Bulk, _]](keys: TwoOrMoreKeys, aggregate: AggregateCount): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZINTER", keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: "WITHSCORES" *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  final def zinterweightedwithscores[A: Read[Bulk, _]](
      weightedKeys: TwoOrMoreWeightedKeys,
      aggregate: AggregateCount
  ): Single.Aux[Seq[(A, Double)]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZINTER", keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: "WITHSCORES" *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  }

  final def zinterstore(keys: TwoOrMoreKeys, destinationKey: Key, aggregate: AggregateCount): Single.Aux[NonNegInt] =
    Protocol("ZINTERSTORE", destinationKey *: keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Num, NonNegInt]
  final def zinterstoreweighted(
      weightedKeys: TwoOrMoreWeightedKeys,
      destinationKey: Key,
      aggregate: AggregateCount
  ): Single.Aux[NonNegInt] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZINTERSTORE", destinationKey *: keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: EmptyTuple)
      .as[Num, NonNegInt]
  }

  final def zunion[A: Read[Bulk, _]](keys: TwoOrMoreKeys, aggregate: AggregateCount): Single.Aux[Seq[A]] =
    Protocol("ZUNION", keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def zunionweighted[A: Read[Bulk, _]](weightedKeys: TwoOrMoreWeightedKeys, aggregate: AggregateCount): Single.Aux[Seq[A]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZUNION", keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  }
  final def zunionwithscores[A: Read[Bulk, _]](keys: TwoOrMoreKeys, aggregate: AggregateCount): Single.Aux[Seq[(A, Double)]] =
    Protocol("ZUNION", keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: "WITHSCORES" *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  final def zunionweightedwithscores[A: Read[Bulk, _]](
      weightedKeys: TwoOrMoreWeightedKeys,
      aggregate: AggregateCount
  ): Single.Aux[Seq[(A, Double)]] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZUNION", keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: "WITHSCORES" *: EmptyTuple)
      .as[Arr.OfPairs[Bulk, Bulk], Seq[(A, Double)]]
  }

  final def zunionstore(keys: TwoOrMoreKeys, destinationKey: Key, aggregate: AggregateCount): Single.Aux[NonNegInt] =
    Protocol("ZUNIONSTORE", destinationKey *: keys.value.length *: keys.value *: "AGGREGATE" *: aggregate *: EmptyTuple).as[Num, NonNegInt]
  final def zunionstoreweighted(
      weightedKeys: TwoOrMoreWeightedKeys,
      destinationKey: Key,
      aggregate: AggregateCount
  ): Single.Aux[NonNegInt] = {
    val (keys, weights) = weightedKeys.value.unzip
    Protocol("ZUNIONSTORE", destinationKey *: keys.length *: keys *: "WEIGHTS" *: weights *: "AGGREGATE" *: aggregate *: EmptyTuple)
      .as[Num, NonNegInt]
  }
}

trait SortedSetP
    extends SortedSetBaseP
    with SortedSetExtP
    with SortedSetInterCardP
    with SortedSetMPopP
    with SortedSetRankWithScoreP
    with SortedSetScanNoScoresP
    with SortedSetAggregateCountP
