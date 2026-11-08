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

object SetP {

  /** APPROX, for SUNIONCARD */
  final class Approx private[SetP] ()
  private[laserdisc] object Approx {
    final val approx: Approx = new Approx

    implicit val approxShow: Show[Approx] = Show.const("APPROX")
  }

  // the builders each feature's commands take, and the types of their replies
  trait SetTypes          {}
  trait SetUnionCardTypes {
    final val approx: SetP.Approx = SetP.Approx.approx
  }

  // the same names for laserdisc's own code, which can't read the members a version overrides while it's being built
  private[protocol] object internal extends SetTypes with SetUnionCardTypes
}

trait SetBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "SADD",
      "SCARD",
      "SDIFF",
      "SDIFFSTORE",
      "SINTER",
      "SINTERSTORE",
      "SISMEMBER",
      "SMEMBERS",
      "SMISMEMBER",
      "SMOVE",
      "SPOP",
      "SRANDMEMBER",
      "SREM",
      "SSCAN: COUNT MATCH",
      "SUNION",
      "SUNIONSTORE"
    )

  private final val zeroIsNone = RESPRead.instance(Read.numZeroIsNone[PosInt])

  /** The builders of this family's commands, and the types of their replies */
  val settypes: SetP.SetTypes = new SetP.SetTypes {}

  final def sadd[A: Show](key: Key, members: OneOrMore[A]): Single.Aux[NonNegInt] =
    Protocol("SADD", key *: members.value *: EmptyTuple).as[Num, NonNegInt]

  final def scard(key: Key): Single.Aux[Option[PosInt]] = Protocol("SCARD", key).using(zeroIsNone)

  final def sdiff[A: Read[Bulk, _]](keys: TwoOrMoreKeys): Single.Aux[Seq[A]] = Protocol("SDIFF", keys.value).as[Arr.Of[Bulk], Seq[A]]

  final def sdiffstore(keys: TwoOrMoreKeys, destinationKey: Key): Single.Aux[NonNegInt] =
    Protocol("SDIFFSTORE", destinationKey :: keys.value).as[Num, NonNegInt]

  final def sinter[A: Read[Bulk, _]](keys: TwoOrMoreKeys): Single.Aux[Seq[A]] = Protocol("SINTER", keys.value).as[Arr.Of[Bulk], Seq[A]]

  final def sinterstore(keys: TwoOrMoreKeys, destinationKey: Key): Single.Aux[NonNegInt] =
    Protocol("SINTERSTORE", destinationKey :: keys.value).as[Num, NonNegInt]

  final def sismember[A: Show](key: Key, member: A): Single.Aux[Boolean] =
    Protocol("SISMEMBER", key *: member *: EmptyTuple).as[Num, Boolean]

  final def smembers[A: Read[Bulk, _]](key: Key): Single.Aux[Seq[A]] = Protocol("SMEMBERS", key).as[Arr.Of[Bulk], Seq[A]]

  final def smismember[A: Show](key: Key, members: OneOrMore[A]): Single.Aux[Seq[Boolean]] =
    Protocol("SMISMEMBER", key *: members.value *: EmptyTuple).as[Arr.Of[Num], Seq[Boolean]]
  final def smismember[A: Show](key: Key, member: A, members: A*): Single.Aux[Seq[Boolean]] =
    smismember(key, OneOrMore.unsafeFrom(member :: members.toList))

  final def smove[A: Show](source: Key, destination: Key, member: A): Single.Aux[Boolean] =
    Protocol("SMOVE", source *: destination *: member *: EmptyTuple).as[Num, Boolean]

  final def spop[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]]             = Protocol("SPOP", key).as[OrNullBulk[Bulk], Option[A]]
  final def spop[A: Read[Bulk, _]](key: Key, count: PosInt): Single.Aux[Seq[A]] =
    Protocol("SPOP", key *: count *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]

  final def srandmember[A: Read[Bulk, _]](key: Key): Single.Aux[Option[A]] =
    Protocol("SRANDMEMBER", key).as[OrNullBulk[Bulk], Option[A]]
  final def srandmembers[A: Read[Bulk, _]](key: Key, count: NonZeroInt): Single.Aux[Seq[A]] =
    Protocol("SRANDMEMBER", key *: count *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]

  final def srem[A: Show](key: Key, members: OneOrMore[A]): Single.Aux[NonNegInt] =
    Protocol("SREM", key *: members.value *: EmptyTuple).as[Num, NonNegInt]

  final def sscan[A: Read[Bulk, _]](key: Key, cursor: NonNegLong): Single.Aux[Scan[A]] =
    Protocol("SSCAN", key *: cursor *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[A]]
  final def sscan[A: Read[Bulk, _]](key: Key, cursor: NonNegLong, pattern: GlobPattern): Single.Aux[Scan[A]] =
    Protocol("SSCAN", key *: cursor *: "MATCH" *: pattern *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[A]]
  final def sscan[A: Read[Bulk, _]](key: Key, cursor: NonNegLong, count: PosInt): Single.Aux[Scan[A]] =
    Protocol("SSCAN", key *: cursor *: "COUNT" *: count *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[A]]
  final def sscan[A: Read[Bulk, _]](key: Key, cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Single.Aux[Scan[A]] =
    Protocol("SSCAN", key *: cursor *: "MATCH" *: pattern *: "COUNT" *: count *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[A]]

  /** SSCAN from cursor 0 until the server is done, as a [[Loop]]. A member may come more than once */
  final def sscanAll[A: Read[Bulk, _]](key: Key): Loop[A]                       = Loop.cursor(sscan[A](key, _))(scanned[A])
  final def sscanAll[A: Read[Bulk, _]](key: Key, pattern: GlobPattern): Loop[A] = Loop.cursor(sscan[A](key, _, pattern))(scanned[A])
  final def sscanAll[A: Read[Bulk, _]](key: Key, count: PosInt): Loop[A]        = Loop.cursor(sscan[A](key, _, count))(scanned[A])
  final def sscanAll[A: Read[Bulk, _]](key: Key, pattern: GlobPattern, count: PosInt): Loop[A] =
    Loop.cursor(sscan[A](key, _, pattern, count))(scanned[A])

  final def sunion[A: Read[Bulk, _]](keys: TwoOrMoreKeys): Single.Aux[Seq[A]] = Protocol("SUNION", keys.value).as[Arr.Of[Bulk], Seq[A]]

  final def sunionstore(keys: TwoOrMoreKeys, destinationKey: Key): Single.Aux[NonNegInt] =
    Protocol("SUNIONSTORE", destinationKey :: keys.value).as[Num, NonNegInt]
}

/** SINTERCARD: Redis 7.0 */
trait SetInterCardP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("SINTERCARD: LIMIT")

  final def sintercard(keys: TwoOrMoreKeys): Single.Aux[NonNegInt] =
    Protocol("SINTERCARD", keys.value.length *: keys.value *: EmptyTuple).as[Num, NonNegInt]
  final def sintercard(key1: Key, key2: Key, keys: Key*): Single.Aux[NonNegInt] =
    sintercard(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: keys.toList))
  final def sintercard(keys: TwoOrMoreKeys, limit: PosInt): Single.Aux[NonNegInt] =
    Protocol("SINTERCARD", keys.value.length *: keys.value *: "LIMIT" *: limit *: EmptyTuple).as[Num, NonNegInt]
}

/** SDIFFCARD: Redis 8.10 */
trait SetDiffCardP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("SDIFFCARD: LIMIT")

  final def sdiffcard(keys: TwoOrMoreKeys): Single.Aux[NonNegInt] =
    Protocol("SDIFFCARD", keys.value.length *: keys.value *: EmptyTuple).as[Num, NonNegInt]
  final def sdiffcard(key1: Key, key2: Key, keys: Key*): Single.Aux[NonNegInt] =
    sdiffcard(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: keys.toList))
  final def sdiffcard(keys: TwoOrMoreKeys, limit: PosInt): Single.Aux[NonNegInt] =
    Protocol("SDIFFCARD", keys.value.length *: keys.value *: "LIMIT" *: limit *: EmptyTuple).as[Num, NonNegInt]
}

/** SUNIONCARD: Redis 8.10 */
trait SetUnionCardP extends Features {
  import SetP.Approx

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SUNIONCARD: APPROX LIMIT")

  final def sunioncard(keys: TwoOrMoreKeys): Single.Aux[NonNegInt] =
    Protocol("SUNIONCARD", keys.value.length *: keys.value *: EmptyTuple).as[Num, NonNegInt]
  final def sunioncard(key1: Key, key2: Key, keys: Key*): Single.Aux[NonNegInt] =
    sunioncard(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: keys.toList))
  final def sunioncard(keys: TwoOrMoreKeys, limit: PosInt): Single.Aux[NonNegInt] =
    Protocol("SUNIONCARD", keys.value.length *: keys.value *: "LIMIT" *: limit *: EmptyTuple).as[Num, NonNegInt]
  final def sunioncard(keys: TwoOrMoreKeys, approx: Approx): Single.Aux[NonNegInt] =
    Protocol("SUNIONCARD", keys.value.length *: keys.value *: approx *: EmptyTuple).as[Num, NonNegInt]
  final def sunioncard(keys: TwoOrMoreKeys, approx: Approx, limit: PosInt): Single.Aux[NonNegInt] =
    Protocol("SUNIONCARD", keys.value.length *: keys.value *: approx *: "LIMIT" *: limit *: EmptyTuple).as[Num, NonNegInt]
}

trait SetP extends SetBaseP with SetExtP with SetInterCardP with SetDiffCardP with SetUnionCardP {
  override val settypes: SetP.SetTypes with SetP.SetUnionCardTypes = new SetP.SetTypes with SetP.SetUnionCardTypes {}
}
