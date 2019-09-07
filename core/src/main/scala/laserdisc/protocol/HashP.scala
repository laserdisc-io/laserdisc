package laserdisc
package protocol

trait HashBaseP {
  import shapeless._
  import shapeless.labelled.FieldType
  import shapeless.nat._1
  import shapeless.ops.hlist.Length
  import shapeless.ops.nat.GTEq.>=

  final def hdel(key: Key, fields: OneOrMoreKeys): Protocol.Aux[NonNegInt] = Protocol("HDEL", key :: fields.value).as[Num, NonNegInt]

  final def hexists(key: Key, field: Key): Protocol.Aux[Boolean] = Protocol("HEXISTS", key :: field :: Nil).as[Num, Boolean]

  final def hget[A: Bulk ==> *](key: Key, field: Key): Protocol.Aux[Option[A]] =
    Protocol("HGET", key :: field :: Nil).opt[GenBulk].as[A]

  final def hgetall[A: Arr ==> *](key: Key): Protocol.Aux[A] = Protocol("HGETALL", key).as[Arr, A]

  final def hincrby(key: Key, field: Key, increment: NonZeroLong): Protocol.Aux[Long] =
    Protocol("HINCRBY", key :: field :: increment :: HNil).as[Num, Long]
  final def hincrby(key: Key, field: Key, increment: NonZeroDouble): Protocol.Aux[Double] =
    Protocol("HINCRBYFLOAT", key :: field :: increment :: HNil).as[Bulk, Double]

  final def hkeys(key: Key): Protocol.Aux[Seq[Key]] = Protocol("HKEYS", key).as[Arr, Seq[Key]]

  final def hlen(key: Key): Protocol.Aux[NonNegInt] = Protocol("HLEN", key).as[Num, NonNegInt]

  final def hmget[L <: HList: Arr ==> *](key: Key, fields: OneOrMoreKeys): Protocol.Aux[L] =
    Protocol("HMGET", key :: fields.value).as[Arr, L]

  final def hmset[L <: HList: RESPParamWrite: LUBConstraint[*, (Key, _)], N <: Nat](key: Key, l: L)(
      implicit ev0: Length.Aux[L, N],
      ev1: N >= _1
  ): Protocol.Aux[OK] = Protocol("HMSET", key :: l).as[Str, OK]
  final def hmset[P <: Product, L <: HList, N <: Nat](key: Key, product: P)(
      implicit gen: LabelledGeneric.Aux[P, L],
      ev0: Length.Aux[L, N],
      ev1: N >= _1,
      ev2: LUBConstraint[L, FieldType[_, _]],
      ev3: RESPParamWrite[L]
  ): Protocol.Aux[OK] = Protocol("HMSET", key :: gen.to(product)).as[Str, OK]

  final def hscan(key: Key, cursor: NonNegLong): Protocol.Aux[ScanKV] = Protocol("HSCAN", key :: cursor :: HNil).as[Arr, ScanKV]
  final def hscan(key: Key, cursor: NonNegLong, pattern: GlobPattern): Protocol.Aux[ScanKV] =
    Protocol("HSCAN", key :: cursor :: "MATCH" :: pattern :: HNil).as[Arr, ScanKV]
  final def hscan(key: Key, cursor: NonNegLong, count: PosInt): Protocol.Aux[ScanKV] =
    Protocol("HSCAN", key :: cursor :: "COUNT" :: count :: HNil).as[Arr, ScanKV]
  final def hscan(key: Key, cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Protocol.Aux[ScanKV] =
    Protocol("HSCAN", key :: cursor :: "MATCH" :: pattern :: "COUNT" :: count :: HNil).as[Arr, ScanKV]

  final def hset[A: Show](key: Key, field: Key, value: A): Protocol.Aux[Boolean] =
    Protocol("HSET", key :: field :: value :: HNil).as[Num, Boolean]

  final def hsetnx[A: Show](key: Key, field: Key, value: A): Protocol.Aux[Boolean] =
    Protocol("HSETNX", key :: field :: value :: HNil).as[Num, Boolean]

  final def hstrlen(key: Key, field: Key): Protocol.Aux[NonNegInt] = Protocol("HSTRLEN", key :: field :: Nil).as[Num, NonNegInt]

  final def hvals[L <: HList: Arr ==> *](key: Key): Protocol.Aux[L] = Protocol("HVALS", key).as[Arr, L]
}

trait HashP extends HashBaseP with HashExtP
