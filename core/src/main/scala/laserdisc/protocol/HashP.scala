package laserdisc
package protocol

trait HashP {
  import Read.==>
  import shapeless._
  import shapeless.labelled.FieldType
  import shapeless.nat._1
  import shapeless.ops.hlist.Length
  import shapeless.ops.nat.GTEq.>=

  final def hdel(key: Key, fields: OneOrMoreKeys): Protocol.Aux[NonNegInt] =
    Protocol("HDEL", key :: fields.value).as[Integer, NonNegInt]

  final def hexists(key: Key, field: Key): Protocol.Aux[Boolean] =
    Protocol("HEXISTS", key :: field :: Nil).as[Integer, Boolean]

  final def hget[A](key: Key, field: Key)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Option[A]] =
    Protocol("HGET", key :: field :: Nil).asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[A]]

  final def hgetall[A](key: Key)(
      implicit ev: NonNilArray ==> A
  ): Protocol.Aux[A] = Protocol("HGETALL", key).as[NonNilArray, A]

  final def hincrby(key: Key, field: Key, increment: NonZeroLong): Protocol.Aux[Long] =
    Protocol("HINCRBY", key :: field :: increment :: HNil).as[Integer, Long]

  final def hincrbyfloat(key: Key, field: Key, increment: NonZeroDouble): Protocol.Aux[Double] =
    Protocol("HINCRBYFLOAT", key :: field :: increment :: HNil).as[NonNullBulkString, Double]

  final def hkeys(key: Key): Protocol.Aux[Seq[Key]] = Protocol("HKEYS", key).as[NonNilArray, Seq[Key]]

  final def hlen(key: Key): Protocol.Aux[NonNegInt] = Protocol("HLEN", key).as[Integer, NonNegInt]

  final def hmget[L <: HList](key: Key, fields: OneOrMoreKeys)(
      implicit ev: NonNilArray ==> L
  ): Protocol.Aux[L] = Protocol("HMGET", key :: fields.value).as[NonNilArray, L]

  final def hmset[L <: HList: RESPParamWrite, N <: Nat](key: Key, l: L)(
      implicit ev0: Length.Aux[L, N],
      ev1: N >= _1,
      ev2: LUBConstraint[L, FieldType[_, _]]
  ): Protocol.Aux[OK] = Protocol("HMSET", key :: l).as[SimpleString, OK]

  final def hmset[P <: Product, L <: HList, N <: Nat](key: Key, product: P)(
      implicit gen: LabelledGeneric.Aux[P, L],
      ev0: Length.Aux[L, N],
      ev1: N >= _1,
      ev2: LUBConstraint[L, FieldType[_, _]],
      ev3: RESPParamWrite[L]
  ): Protocol.Aux[OK] = Protocol("HMSET", key :: gen.to(product)).as[SimpleString, OK]

  final def hscan(key: Key, cursor: NonNegLong): Protocol.Aux[ScanKV] =
    Protocol("HSCAN", key :: cursor :: HNil).as[NonNilArray, ScanKV]

  final def hscan(key: Key, cursor: NonNegLong, pattern: GlobPattern): Protocol.Aux[ScanKV] =
    Protocol("HSCAN", key :: cursor :: "MATCH" :: pattern :: HNil).as[NonNilArray, ScanKV]

  final def hscan(key: Key, cursor: NonNegLong, count: PosInt): Protocol.Aux[ScanKV] =
    Protocol("HSCAN", key :: cursor :: "COUNT" :: count :: HNil).as[NonNilArray, ScanKV]

  final def hscan(key: Key, cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Protocol.Aux[ScanKV] =
    Protocol("HSCAN", key :: cursor :: "MATCH" :: pattern :: "COUNT" :: count :: HNil).as[NonNilArray, ScanKV]

  final def hset[A: Show](key: Key, field: Key, value: A): Protocol.Aux[Boolean] =
    Protocol("HSET", key :: field :: value :: HNil).as[Integer, Boolean]

  final def hsetnx[A: Show](key: Key, field: Key, value: A): Protocol.Aux[Boolean] =
    Protocol("HSETNX", key :: field :: value :: HNil).as[Integer, Boolean]

  final def hstrlen(key: Key, field: Key): Protocol.Aux[NonNegInt] =
    Protocol("HSTRLEN", key :: field :: Nil).as[Integer, NonNegInt]

  final def hvals[L <: HList](key: Key)(
      implicit ev: NonNilArray ==> L
  ): Protocol.Aux[L] = Protocol("HVALS", key).as[NonNilArray, L]
}

trait AllHashP extends HashP with HashPExtra
