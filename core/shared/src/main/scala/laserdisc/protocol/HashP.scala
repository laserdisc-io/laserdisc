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

trait HashBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "HDEL",
      "HEXISTS",
      "HGET",
      "HGETALL",
      "HINCRBY",
      "HINCRBYFLOAT",
      "HKEYS",
      "HLEN",
      "HMGET",
      "HMSET",
      "HSCAN: COUNT MATCH",
      "HSET",
      "HSETNX",
      "HSTRLEN",
      "HVALS"
    )

  final def hdel(key: Key, fields: OneOrMoreKeys): Single.Aux[NonNegInt] = Protocol("HDEL", key :: fields.value).as[Num, NonNegInt]

  final def hexists(key: Key, field: Key): Single.Aux[Boolean] = Protocol("HEXISTS", key :: field :: Nil).as[Num, Boolean]

  final def hget[A: Read[Bulk, _]](key: Key, field: Key): Single.Aux[Option[A]] =
    Protocol("HGET", key :: field :: Nil).asOpt[Bulk, A]

  final def hgetall[A: Read[Arr, _]](key: Key): Single.Aux[A] = Protocol("HGETALL", key).as[Arr, A]

  final def hincrby(key: Key, field: Key, increment: NonZeroLong): Single.Aux[Long] =
    Protocol("HINCRBY", key *: field *: increment *: EmptyTuple).as[Num, Long]
  final def hincrby(key: Key, field: Key, increment: NonZeroDouble): Single.Aux[Double] =
    Protocol("HINCRBYFLOAT", key *: field *: increment *: EmptyTuple).as[Bulk, Double]

  final def hkeys(key: Key): Single.Aux[Seq[Key]] = Protocol("HKEYS", key).as[Arr, Seq[Key]]

  final def hlen(key: Key): Single.Aux[NonNegInt] = Protocol("HLEN", key).as[Num, NonNegInt]

  final def hmget[L <: Tuple: Read[Arr, _]](key: Key, fields: OneOrMoreKeys): Single.Aux[L] =
    Protocol("HMGET", key :: fields.value).as[Arr, L]

  final def hmset[L <: NonEmptyTuple: RESPParamWrite: LUBConstraint[_, (Key, ?)]](key: Key, l: L): Single.Aux[OK] =
    Protocol("HMSET", key *: l).as[Str, OK]
  final def hmset[P <: Product: RESPParamWrite: <:!<[EmptyTuple, _]](key: Key, product: P): Single.Aux[OK] =
    Protocol("HMSET", key *: product *: EmptyTuple).as[Str, OK]

  final def hscan(key: Key, cursor: NonNegLong): Single.Aux[ScanKV] = Protocol("HSCAN", key *: cursor *: EmptyTuple).as[Arr, ScanKV]
  final def hscan(key: Key, cursor: NonNegLong, pattern: GlobPattern): Single.Aux[ScanKV] =
    Protocol("HSCAN", key *: cursor *: "MATCH" *: pattern *: EmptyTuple).as[Arr, ScanKV]
  final def hscan(key: Key, cursor: NonNegLong, count: PosInt): Single.Aux[ScanKV] =
    Protocol("HSCAN", key *: cursor *: "COUNT" *: count *: EmptyTuple).as[Arr, ScanKV]
  final def hscan(key: Key, cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Single.Aux[ScanKV] =
    Protocol("HSCAN", key *: cursor *: "MATCH" *: pattern *: "COUNT" *: count *: EmptyTuple).as[Arr, ScanKV]

  final def hset[A: Show](key: Key, field: Key, value: A): Single.Aux[Boolean] =
    Protocol("HSET", key *: field *: value *: EmptyTuple).as[Num, Boolean]

  final def hsetnx[A: Show](key: Key, field: Key, value: A): Single.Aux[Boolean] =
    Protocol("HSETNX", key *: field *: value *: EmptyTuple).as[Num, Boolean]

  final def hstrlen(key: Key, field: Key): Single.Aux[NonNegInt] = Protocol("HSTRLEN", key :: field :: Nil).as[Num, NonNegInt]

  final def hvals[L <: Tuple: Read[Arr, _]](key: Key): Single.Aux[L] = Protocol("HVALS", key).as[Arr, L]
}

trait HashP extends HashBaseP with HashExtP
