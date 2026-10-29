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

trait SetBaseP {
  private[this] final val zeroIsNone = RESPRead.instance(Read.numZeroIsNone[PosInt])

  final def sadd[A: Show](key: Key, members: OneOrMore[A]): Protocol.Aux[NonNegInt] =
    Protocol("SADD", key *: members.value *: EmptyTuple).as[Num, NonNegInt]

  final def scard(key: Key): Protocol.Aux[Option[PosInt]] = Protocol("SCARD", key).using(zeroIsNone)

  final def sdiff[A: ReadArrToSeq](keys: TwoOrMoreKeys): Protocol.Aux[Seq[A]] = Protocol("SDIFF", keys.value).as[Arr, Seq[A]]

  final def sdiffstore(keys: TwoOrMoreKeys, destinationKey: Key): Protocol.Aux[NonNegInt] =
    Protocol("SDIFFSTORE", destinationKey :: keys.value).as[Num, NonNegInt]

  final def sinter[A: ReadArrToSeq](keys: TwoOrMoreKeys): Protocol.Aux[Seq[A]] = Protocol("SINTER", keys.value).as[Arr, Seq[A]]

  final def sinterstore(keys: TwoOrMoreKeys, destinationKey: Key): Protocol.Aux[NonNegInt] =
    Protocol("SINTERSTORE", destinationKey :: keys.value).as[Num, NonNegInt]

  final def sismember[A: Show](key: Key, member: A): Protocol.Aux[Boolean] =
    Protocol("SISMEMBER", key *: member *: EmptyTuple).as[Num, Boolean]

  final def smembers[A: Read[Bulk, _]](key: Key): Protocol.Aux[Seq[A]] = Protocol("SMEMBERS", key).as[Arr, Seq[A]]

  final def smove[A: Show](source: Key, destination: Key, member: A): Protocol.Aux[Boolean] =
    Protocol("SMOVE", source *: destination *: member *: EmptyTuple).as[Num, Boolean]

  final def spop[A: Read[Bulk, _]](key: Key): Protocol.Aux[Option[A]]             = Protocol("SPOP", key).asOpt[Bulk, A]
  final def spop[A: Read[Bulk, _]](key: Key, count: PosInt): Protocol.Aux[Seq[A]] =
    Protocol("SPOP", key *: count *: EmptyTuple).as[Arr, Seq[A]]

  final def srandmember[A: Read[Bulk, _]](key: Key): Protocol.Aux[Option[A]] =
    Protocol("SRANDMEMBER", key).asOpt[Bulk, A]
  final def srandmembers[A: Read[Bulk, _]](key: Key, count: NonZeroInt): Protocol.Aux[Seq[A]] =
    Protocol("SRANDMEMBER", key *: count *: EmptyTuple).as[Arr, Seq[A]]

  final def srem[A: Show](key: Key, members: OneOrMore[A]): Protocol.Aux[NonNegInt] =
    Protocol("SREM", key *: members.value *: EmptyTuple).as[Num, NonNegInt]

  final def sscan[A: ReadArrToSeq](key: Key, cursor: NonNegLong): Protocol.Aux[Scan[A]] =
    Protocol("SSCAN", key *: cursor *: EmptyTuple).as[Arr, Scan[A]]
  final def sscan[A: ReadArrToSeq](key: Key, cursor: NonNegLong, pattern: GlobPattern): Protocol.Aux[Scan[A]] =
    Protocol("SSCAN", key *: cursor *: "MATCH" *: pattern *: EmptyTuple).as[Arr, Scan[A]]
  final def sscan[A: ReadArrToSeq](key: Key, cursor: NonNegLong, count: PosInt): Protocol.Aux[Scan[A]] =
    Protocol("SSCAN", key *: cursor *: "COUNT" *: count *: EmptyTuple).as[Arr, Scan[A]]
  final def sscan[A: ReadArrToSeq](key: Key, cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Protocol.Aux[Scan[A]] =
    Protocol("SSCAN", key *: cursor *: "MATCH" *: pattern *: "COUNT" *: count *: EmptyTuple).as[Arr, Scan[A]]

  final def sunion[A: ReadArrToSeq](keys: TwoOrMoreKeys): Protocol.Aux[Seq[A]] = Protocol("SUNION", keys.value).as[Arr, Seq[A]]

  final def sunionstore(keys: TwoOrMoreKeys, destinationKey: Key): Protocol.Aux[NonNegInt] =
    Protocol("SUNIONSTORE", destinationKey :: keys.value).as[Num, NonNegInt]
}

trait SetP extends SetBaseP with SetExtP
