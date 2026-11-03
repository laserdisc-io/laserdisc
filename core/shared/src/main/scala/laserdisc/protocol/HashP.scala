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

object HashP {

  /** NX, XX, GT and LT, for HEXPIRE, HPEXPIRE, HEXPIREAT and HPEXPIREAT */
  sealed trait ExpireCondition
  object ExpireCondition {
    object nx extends ExpireCondition
    object xx extends ExpireCondition
    object gt extends ExpireCondition
    object lt extends ExpireCondition

    implicit val expireConditionShow: Show[ExpireCondition] = Show.instance {
      case `nx` => "NX"
      case `xx` => "XX"
      case `gt` => "GT"
      case `lt` => "LT"
    }
  }

  /** FNX and FXX, for HSETEX: set the fields only if none of them exist, or only if all of them do */
  sealed trait FieldsCondition
  object FieldsCondition {
    object fnx extends FieldsCondition
    object fxx extends FieldsCondition

    implicit val fieldsConditionShow: Show[FieldsCondition] = Show.instance {
      case `fnx` => "FNX"
      case `fxx` => "FXX"
    }
  }

  /** NX and XX, for HSETEX: set the fields only if the hash doesn't exist, or only if it does. Valkey 9.1 */
  sealed trait KeyCondition
  object KeyCondition {
    object nx extends KeyCondition
    object xx extends KeyCondition

    implicit val keyConditionShow: Show[KeyCondition] = Show.instance {
      case `nx` => "NX"
      case `xx` => "XX"
    }
  }

  /** What HEXPIRE, HPEXPIRE, HEXPIREAT and HPEXPIREAT did to each field */
  sealed trait FieldExpireResponse

  /** What HTTL and HPTTL reply for each field */
  sealed trait FieldTTLResponse

  /** What HEXPIRETIME and HPEXPIRETIME reply for each field */
  sealed trait FieldExpireTimeResponse

  /** What HPERSIST did to each field */
  sealed trait FieldPersistResponse

  case object NoField  extends FieldExpireResponse with FieldTTLResponse with FieldExpireTimeResponse with FieldPersistResponse
  case object NoExpire extends FieldTTLResponse with FieldExpireTimeResponse with FieldPersistResponse

  object FieldExpireResponse {

    /** Not set, because of the condition */
    case object NotSet extends FieldExpireResponse
    case object Set    extends FieldExpireResponse

    /** Deleted, because the time given has already passed */
    case object Deleted extends FieldExpireResponse

    implicit final val num2FieldExpireResponseRead: Read[Num, FieldExpireResponse] = Read.instance {
      case Num(-2)    => Right(NoField)
      case Num(0)     => Right(NotSet)
      case Num(1)     => Right(Set)
      case Num(2)     => Right(Deleted)
      case Num(other) => Left(RESPDecErr(s"Unexpected hash field expire reply. Was $other"))
    }
  }

  object FieldTTLResponse {
    final case class ExpireAfter(ttl: NonNegLong) extends FieldTTLResponse

    implicit final val num2FieldTTLResponseRead: Read[Num, FieldTTLResponse] = Read.instance {
      case Num(-2)          => Right(NoField)
      case Num(-1)          => Right(NoExpire)
      case Num(l) if l >= 0 => Right(ExpireAfter(NonNegLong.unsafeFrom(l)))
      case Num(other)       => Left(RESPDecErr(s"Unexpected hash field TTL. Was $other"))
    }
  }

  object FieldExpireTimeResponse {
    final case class ExpireAt(timestamp: NonNegLong) extends FieldExpireTimeResponse

    implicit final val num2FieldExpireTimeResponseRead: Read[Num, FieldExpireTimeResponse] = Read.instance {
      case Num(-2)          => Right(NoField)
      case Num(-1)          => Right(NoExpire)
      case Num(l) if l >= 0 => Right(ExpireAt(NonNegLong.unsafeFrom(l)))
      case Num(other)       => Left(RESPDecErr(s"Unexpected hash field expire time. Was $other"))
    }
  }

  object FieldPersistResponse {
    case object Persisted extends FieldPersistResponse

    implicit final val num2FieldPersistResponseRead: Read[Num, FieldPersistResponse] = Read.instance {
      case Num(-2)    => Right(NoField)
      case Num(-1)    => Right(NoExpire)
      case Num(1)     => Right(Persisted)
      case Num(other) => Left(RESPDecErr(s"Unexpected hash field persist reply. Was $other"))
    }
  }

  private[protocol] final def fields(fields: OneOrMoreKeys): List[String] =
    "FIELDS" :: fields.value.length.toString :: fields.value.map(_.value)

  private[protocol] final def fieldsAndValues[A: Show](fieldValues: OneOrMore[(Key, A)]): List[String] =
    "FIELDS" :: fieldValues.value.length.toString :: fieldValues.value.flatMap { case (f, v) => List(f.value, Show[A].show(v)) }
}

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
      "HRANDFIELD: WITHVALUES",
      "HSCAN: COUNT MATCH",
      "HSET",
      "HSETNX",
      "HSTRLEN",
      "HVALS"
    )

  object hashtypes {
    final type HashExpireCondition         = HashP.ExpireCondition
    final type HashFieldsCondition         = HashP.FieldsCondition
    final type HashKeyCondition            = HashP.KeyCondition
    final type HashFieldExpireResponse     = HashP.FieldExpireResponse
    final type HashFieldTTLResponse        = HashP.FieldTTLResponse
    final type HashFieldExpireTimeResponse = HashP.FieldExpireTimeResponse
    final type HashFieldPersistResponse    = HashP.FieldPersistResponse

    final val HashExpireCondition  = HashP.ExpireCondition
    final val HashFieldsCondition  = HashP.FieldsCondition
    final val HashKeyCondition     = HashP.KeyCondition
    final val HashNoField          = HashP.NoField
    final val HashNoExpire         = HashP.NoExpire
    final val HashFieldNotSet      = HashP.FieldExpireResponse.NotSet
    final val HashFieldSet         = HashP.FieldExpireResponse.Set
    final val HashFieldDeleted     = HashP.FieldExpireResponse.Deleted
    final val HashFieldExpireAfter = HashP.FieldTTLResponse.ExpireAfter
    final val HashFieldExpireAt    = HashP.FieldExpireTimeResponse.ExpireAt
    final val HashFieldPersisted   = HashP.FieldPersistResponse.Persisted
  }

  final def hdel(key: Key, fields: OneOrMoreKeys): Single.Aux[NonNegInt] = Protocol("HDEL", key :: fields.value).as[Num, NonNegInt]

  final def hexists(key: Key, field: Key): Single.Aux[Boolean] = Protocol("HEXISTS", key :: field :: Nil).as[Num, Boolean]

  final def hget[A: Read[Bulk, _]](key: Key, field: Key): Single.Aux[Option[A]] =
    Protocol("HGET", key :: field :: Nil).as[Bulk, Option[A]]

  final def hgetall[A: Read[Arr, _]](key: Key): Single.Aux[A] = Protocol("HGETALL", key).as[Arr, A]

  final def hincrby(key: Key, field: Key, increment: NonZeroLong): Single.Aux[Long] =
    Protocol("HINCRBY", key *: field *: increment *: EmptyTuple).as[Num, Long]
  final def hincrby(key: Key, field: Key, increment: NonZeroDouble): Single.Aux[Double] =
    Protocol("HINCRBYFLOAT", key *: field *: increment *: EmptyTuple).as[Bulk, Double]

  final def hkeys(key: Key): Single.Aux[Seq[Key]] = Protocol("HKEYS", key).as[Arr.Of[Bulk], Seq[Key]]

  final def hlen(key: Key): Single.Aux[NonNegInt] = Protocol("HLEN", key).as[Num, NonNegInt]

  final def hmget[L <: Tuple: Read[Arr, _]](key: Key, fields: OneOrMoreKeys): Single.Aux[L] =
    Protocol("HMGET", key :: fields.value).as[Arr, L]

  final def hmset[L <: NonEmptyTuple: RESPParamWrite: LUBConstraint[_, (Key, ?)]](key: Key, l: L): Single.Aux[OK] =
    Protocol("HMSET", key *: l).as[Str, OK]
  final def hmset[P <: Product: RESPParamWrite: <:!<[EmptyTuple, _]](key: Key, product: P): Single.Aux[OK] =
    Protocol("HMSET", key *: product *: EmptyTuple).as[Str, OK]

  final def hrandfield(key: Key): Single.Aux[Option[Key]]                  = Protocol("HRANDFIELD", key).as[Bulk, Option[Key]]
  final def hrandfields(key: Key, count: NonZeroInt): Single.Aux[Seq[Key]] =
    Protocol("HRANDFIELD", key *: count *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def hrandfieldswithvalues[A: Read[Bulk, _]](key: Key, count: NonZeroInt): Single.Aux[Seq[(Key, A)]] =
    Protocol("HRANDFIELD", key *: count *: "WITHVALUES" *: EmptyTuple).as[Arr.Pairs[Bulk, Bulk], Seq[(Key, A)]]

  final def hscan(key: Key, cursor: NonNegLong): Single.Aux[ScanKV] = Protocol("HSCAN", key *: cursor *: EmptyTuple).as[Arr, ScanKV]
  final def hscan(key: Key, cursor: NonNegLong, pattern: GlobPattern): Single.Aux[ScanKV] =
    Protocol("HSCAN", key *: cursor *: "MATCH" *: pattern *: EmptyTuple).as[Arr, ScanKV]
  final def hscan(key: Key, cursor: NonNegLong, count: PosInt): Single.Aux[ScanKV] =
    Protocol("HSCAN", key *: cursor *: "COUNT" *: count *: EmptyTuple).as[Arr, ScanKV]
  final def hscan(key: Key, cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Single.Aux[ScanKV] =
    Protocol("HSCAN", key *: cursor *: "MATCH" *: pattern *: "COUNT" *: count *: EmptyTuple).as[Arr, ScanKV]

  /** HSCAN from cursor 0 until the server is done, as a [[Loop]]. A field may come more than once */
  final def hscanAll(key: Key): Loop[KV[String]]                                      = Loop.cursor(hscan(key, _))(scannedKV)
  final def hscanAll(key: Key, pattern: GlobPattern): Loop[KV[String]]                = Loop.cursor(hscan(key, _, pattern))(scannedKV)
  final def hscanAll(key: Key, count: PosInt): Loop[KV[String]]                       = Loop.cursor(hscan(key, _, count))(scannedKV)
  final def hscanAll(key: Key, pattern: GlobPattern, count: PosInt): Loop[KV[String]] =
    Loop.cursor(hscan(key, _, pattern, count))(scannedKV)

  final def hset[A: Show](key: Key, field: Key, value: A): Single.Aux[Boolean] =
    Protocol("HSET", key *: field *: value *: EmptyTuple).as[Num, Boolean]
  final def hset[A: Show](key: Key, fieldValues: OneOrMore[(Key, A)]): Single.Aux[NonNegInt] =
    Protocol("HSET", key *: fieldValues.value *: EmptyTuple).as[Num, NonNegInt]

  final def hsetnx[A: Show](key: Key, field: Key, value: A): Single.Aux[Boolean] =
    Protocol("HSETNX", key *: field *: value *: EmptyTuple).as[Num, Boolean]

  final def hstrlen(key: Key, field: Key): Single.Aux[NonNegInt] = Protocol("HSTRLEN", key :: field :: Nil).as[Num, NonNegInt]

  final def hvals[L <: Tuple: Read[Arr, _]](key: Key): Single.Aux[L] = Protocol("HVALS", key).as[Arr, L]
}

/** HSCAN with NOVALUES: Redis 7.4, Valkey 8.0 */
trait HashScanNoValuesP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("HSCAN: NOVALUES")

  final def hscannovalues(key: Key, cursor: NonNegLong): Single.Aux[Scan[Key]] =
    Protocol("HSCAN", key *: cursor *: "NOVALUES" *: EmptyTuple).as[Arr, Scan[Key]]
  final def hscannovalues(key: Key, cursor: NonNegLong, pattern: GlobPattern): Single.Aux[Scan[Key]] =
    Protocol("HSCAN", key *: cursor *: "MATCH" *: pattern *: "NOVALUES" *: EmptyTuple).as[Arr, Scan[Key]]
  final def hscannovalues(key: Key, cursor: NonNegLong, count: PosInt): Single.Aux[Scan[Key]] =
    Protocol("HSCAN", key *: cursor *: "COUNT" *: count *: "NOVALUES" *: EmptyTuple).as[Arr, Scan[Key]]
  final def hscannovalues(key: Key, cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Single.Aux[Scan[Key]] =
    Protocol("HSCAN", key *: cursor *: "MATCH" *: pattern *: "COUNT" *: count *: "NOVALUES" *: EmptyTuple).as[Arr, Scan[Key]]

  /** HSCAN with NOVALUES from cursor 0 until the server is done, as a [[Loop]]. A field may come more than once */
  final def hscannovaluesAll(key: Key): Loop[Key]                       = Loop.cursor(hscannovalues(key, _))(scanned)
  final def hscannovaluesAll(key: Key, pattern: GlobPattern): Loop[Key] = Loop.cursor(hscannovalues(key, _, pattern))(scanned)
  final def hscannovaluesAll(key: Key, count: PosInt): Loop[Key]        = Loop.cursor(hscannovalues(key, _, count))(scanned)
  final def hscannovaluesAll(key: Key, pattern: GlobPattern, count: PosInt): Loop[Key] =
    Loop.cursor(hscannovalues(key, _, pattern, count))(scanned)
}

/** Expiry of single fields: Redis 7.4, Valkey 9.0 */
trait HashFieldExpiryP extends Features {
  import HashP.{ExpireCondition, FieldExpireResponse, FieldExpireTimeResponse, FieldPersistResponse, FieldTTLResponse}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "HEXPIRE: FIELDS GT LT NX XX",
      "HEXPIREAT: FIELDS GT LT NX XX",
      "HEXPIRETIME: FIELDS",
      "HPERSIST: FIELDS",
      "HPEXPIRE: FIELDS GT LT NX XX",
      "HPEXPIREAT: FIELDS GT LT NX XX",
      "HPEXPIRETIME: FIELDS",
      "HPTTL: FIELDS",
      "HTTL: FIELDS"
    )

  private final val expireResponses     = RESPRead[Arr.Of[Num], Seq[FieldExpireResponse]]
  private final val ttlResponses        = RESPRead[Arr.Of[Num], Seq[FieldTTLResponse]]
  private final val expireTimeResponses = RESPRead[Arr.Of[Num], Seq[FieldExpireTimeResponse]]
  private final val persistResponses    = RESPRead[Arr.Of[Num], Seq[FieldPersistResponse]]

  final def hexpire(key: Key, seconds: NonNegLong, fields: OneOrMoreKeys): Single.Aux[Seq[FieldExpireResponse]] =
    Protocol("HEXPIRE", key *: seconds *: HashP.fields(fields) *: EmptyTuple).using(expireResponses)
  final def hexpire(
      key: Key,
      seconds: NonNegLong,
      condition: ExpireCondition,
      fields: OneOrMoreKeys
  ): Single.Aux[Seq[FieldExpireResponse]] =
    Protocol("HEXPIRE", key *: seconds *: condition *: HashP.fields(fields) *: EmptyTuple).using(expireResponses)

  final def hexpireat(key: Key, seconds: NonNegLong, fields: OneOrMoreKeys): Single.Aux[Seq[FieldExpireResponse]] =
    Protocol("HEXPIREAT", key *: seconds *: HashP.fields(fields) *: EmptyTuple).using(expireResponses)
  final def hexpireat(
      key: Key,
      seconds: NonNegLong,
      condition: ExpireCondition,
      fields: OneOrMoreKeys
  ): Single.Aux[Seq[FieldExpireResponse]] =
    Protocol("HEXPIREAT", key *: seconds *: condition *: HashP.fields(fields) *: EmptyTuple).using(expireResponses)

  final def hpexpire(key: Key, milliseconds: NonNegLong, fields: OneOrMoreKeys): Single.Aux[Seq[FieldExpireResponse]] =
    Protocol("HPEXPIRE", key *: milliseconds *: HashP.fields(fields) *: EmptyTuple).using(expireResponses)
  final def hpexpire(
      key: Key,
      milliseconds: NonNegLong,
      condition: ExpireCondition,
      fields: OneOrMoreKeys
  ): Single.Aux[Seq[FieldExpireResponse]] =
    Protocol("HPEXPIRE", key *: milliseconds *: condition *: HashP.fields(fields) *: EmptyTuple).using(expireResponses)

  final def hpexpireat(key: Key, milliseconds: NonNegLong, fields: OneOrMoreKeys): Single.Aux[Seq[FieldExpireResponse]] =
    Protocol("HPEXPIREAT", key *: milliseconds *: HashP.fields(fields) *: EmptyTuple).using(expireResponses)
  final def hpexpireat(
      key: Key,
      milliseconds: NonNegLong,
      condition: ExpireCondition,
      fields: OneOrMoreKeys
  ): Single.Aux[Seq[FieldExpireResponse]] =
    Protocol("HPEXPIREAT", key *: milliseconds *: condition *: HashP.fields(fields) *: EmptyTuple).using(expireResponses)

  final def hexpiretime(key: Key, fields: OneOrMoreKeys): Single.Aux[Seq[FieldExpireTimeResponse]] =
    Protocol("HEXPIRETIME", key *: HashP.fields(fields) *: EmptyTuple).using(expireTimeResponses)

  final def hpersist(key: Key, fields: OneOrMoreKeys): Single.Aux[Seq[FieldPersistResponse]] =
    Protocol("HPERSIST", key *: HashP.fields(fields) *: EmptyTuple).using(persistResponses)

  final def hpexpiretime(key: Key, fields: OneOrMoreKeys): Single.Aux[Seq[FieldExpireTimeResponse]] =
    Protocol("HPEXPIRETIME", key *: HashP.fields(fields) *: EmptyTuple).using(expireTimeResponses)

  final def hpttl(key: Key, fields: OneOrMoreKeys): Single.Aux[Seq[FieldTTLResponse]] =
    Protocol("HPTTL", key *: HashP.fields(fields) *: EmptyTuple).using(ttlResponses)

  final def httl(key: Key, fields: OneOrMoreKeys): Single.Aux[Seq[FieldTTLResponse]] =
    Protocol("HTTL", key *: HashP.fields(fields) *: EmptyTuple).using(ttlResponses)
}

/** HGETEX and HSETEX: Redis 8.0, Valkey 9.0 */
trait HashGetSetExpiryP extends Features {
  import HashP.FieldsCondition
  import StringP.{GetExpiry, SetExpiry}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("HGETEX: EX EXAT FIELDS PERSIST PX PXAT", "HSETEX: EX EXAT FIELDS FNX FXX KEEPTTL PX PXAT")

  final def hgetex[A: Read[Bulk, _]](key: Key, fields: OneOrMoreKeys): Single.Aux[Seq[Option[A]]] =
    Protocol("HGETEX", key *: HashP.fields(fields) *: EmptyTuple).as[Arr.Of[Option[Bulk]], Seq[Option[A]]]
  final def hgetex[A: Read[Bulk, _]](key: Key, expiry: GetExpiry, fields: OneOrMoreKeys): Single.Aux[Seq[Option[A]]] =
    Protocol("HGETEX", key *: expiry.params *: HashP.fields(fields) *: EmptyTuple).as[Arr.Of[Option[Bulk]], Seq[Option[A]]]

  final def hsetex[A: Show](key: Key, fieldValues: OneOrMore[(Key, A)]): Single.Aux[Boolean] =
    Protocol("HSETEX", key *: HashP.fieldsAndValues(fieldValues) *: EmptyTuple).as[Num, Boolean]
  final def hsetex[A: Show](key: Key, condition: FieldsCondition, fieldValues: OneOrMore[(Key, A)]): Single.Aux[Boolean] =
    Protocol("HSETEX", key *: condition *: HashP.fieldsAndValues(fieldValues) *: EmptyTuple).as[Num, Boolean]
  final def hsetex[A: Show](key: Key, expiry: SetExpiry, fieldValues: OneOrMore[(Key, A)]): Single.Aux[Boolean] =
    Protocol("HSETEX", key *: expiry.params *: HashP.fieldsAndValues(fieldValues) *: EmptyTuple).as[Num, Boolean]
  final def hsetex[A: Show](
      key: Key,
      condition: FieldsCondition,
      expiry: SetExpiry,
      fieldValues: OneOrMore[(Key, A)]
  ): Single.Aux[Boolean] =
    Protocol("HSETEX", key *: condition *: expiry.params *: HashP.fieldsAndValues(fieldValues) *: EmptyTuple).as[Num, Boolean]
}

/** HSETEX with NX and XX on the hash itself: Valkey 9.1 */
trait HashSetExKeyConditionP extends Features {
  import HashP.{FieldsCondition, KeyCondition}
  import StringP.SetExpiry

  override private[laserdisc] def supported: List[String] = super.supported ++ List("HSETEX: NX XX")

  final def hsetex[A: Show](key: Key, keyCondition: KeyCondition, fieldValues: OneOrMore[(Key, A)]): Single.Aux[Boolean] =
    Protocol("HSETEX", key *: keyCondition *: HashP.fieldsAndValues(fieldValues) *: EmptyTuple).as[Num, Boolean]
  final def hsetex[A: Show](
      key: Key,
      keyCondition: KeyCondition,
      condition: FieldsCondition,
      fieldValues: OneOrMore[(Key, A)]
  ): Single.Aux[Boolean] =
    Protocol("HSETEX", key *: keyCondition *: condition *: HashP.fieldsAndValues(fieldValues) *: EmptyTuple).as[Num, Boolean]
  final def hsetex[A: Show](
      key: Key,
      keyCondition: KeyCondition,
      expiry: SetExpiry,
      fieldValues: OneOrMore[(Key, A)]
  ): Single.Aux[Boolean] =
    Protocol("HSETEX", key *: keyCondition *: expiry.params *: HashP.fieldsAndValues(fieldValues) *: EmptyTuple).as[Num, Boolean]
  final def hsetex[A: Show](
      key: Key,
      keyCondition: KeyCondition,
      condition: FieldsCondition,
      expiry: SetExpiry,
      fieldValues: OneOrMore[(Key, A)]
  ): Single.Aux[Boolean] =
    Protocol("HSETEX", key *: keyCondition *: condition *: expiry.params *: HashP.fieldsAndValues(fieldValues) *: EmptyTuple)
      .as[Num, Boolean]
}

/** HGETDEL: Redis 8.0, Valkey 9.1 */
trait HashGetDelP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("HGETDEL: FIELDS")

  final def hgetdel[A: Read[Bulk, _]](key: Key, fields: OneOrMoreKeys): Single.Aux[Seq[Option[A]]] =
    Protocol("HGETDEL", key *: HashP.fields(fields) *: EmptyTuple).as[Arr.Of[Option[Bulk]], Seq[Option[A]]]
}

trait HashP
    extends HashBaseP
    with HashExtP
    with HashScanNoValuesP
    with HashFieldExpiryP
    with HashGetSetExpiryP
    with HashSetExKeyConditionP
    with HashGetDelP
