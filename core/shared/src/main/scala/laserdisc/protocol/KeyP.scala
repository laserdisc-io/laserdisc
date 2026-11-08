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

object KeyP {

  /** What OBJECT ENCODING replies. `ziplist` is from Redis 6.2 and KeyDB, `listpack` from Redis 7.0 and Valkey, `slicedArray` is an
    * array's, from Redis 8.8, and `other` is any encoding a later server adds
    */
  sealed trait Encoding
  object Encoding {
    case object raw                   extends Encoding
    case object int                   extends Encoding
    case object embstr                extends Encoding
    case object ziplist               extends Encoding
    case object listpack              extends Encoding
    case object quicklist             extends Encoding
    case object intset                extends Encoding
    case object hashtable             extends Encoding
    case object skiplist              extends Encoding
    case object stream                extends Encoding
    case object slicedArray           extends Encoding
    final case class other(name: Key) extends Encoding

    implicit final val bulk2EncodingRead: Read[Bulk, Encoding] = Read.instance {
      case Bulk("raw")          => Right(raw)
      case Bulk("int")          => Right(int)
      case Bulk("embstr")       => Right(embstr)
      case Bulk("ziplist")      => Right(ziplist)
      case Bulk("listpack")     => Right(listpack)
      case Bulk("quicklist")    => Right(quicklist)
      case Bulk("intset")       => Right(intset)
      case Bulk("hashtable")    => Right(hashtable)
      case Bulk("skiplist")     => Right(skiplist)
      case Bulk("stream")       => Right(stream)
      case Bulk("sliced-array") => Right(slicedArray)
      case Bulk(other)          => Read[Bulk, Key].read(Bulk(other)).map(Encoding.other(_))
    }
  }

  /** AUTH and AUTH2, for MIGRATE */
  sealed trait MigrateAuth  { def params: List[String] }
  trait MigrateAuthBuilders {
    def password(password: String): MigrateAuth
    def userAndPassword(username: String, password: String): MigrateAuth
  }
  private[laserdisc] object MigrateAuth extends MigrateAuthBuilders {
    private final class Of(override final val params: List[String]) extends MigrateAuth

    def password(password: String): MigrateAuth                          = new Of(List("AUTH", password))
    def userAndPassword(username: String, password: String): MigrateAuth = new Of(List("AUTH2", username, password))
  }

  /** REPLACE, for COPY */
  sealed trait Replace
  object Replace extends Replace {
    implicit val replaceShow: Show[Replace] = Show.const("REPLACE")
  }

  /** BY, LIMIT, ASC, DESC and ALPHA, for SORT and SORT_RO. Patterns are key names with `*`, and `->field` for a hash field */
  final class Sort private (val params: List[String]) {
    def by(pattern: Key): Sort                          = new Sort(params ++ List("BY", pattern.value))
    def noSort: Sort                                    = new Sort(params ++ List("BY", "nosort"))
    def limit(offset: NonNegLong, count: PosLong): Sort =
      new Sort(params ++ List("LIMIT", Show[NonNegLong].show(offset), Show[PosLong].show(count)))
    def asc: Sort   = new Sort(params :+ "ASC")
    def desc: Sort  = new Sort(params :+ "DESC")
    def alpha: Sort = new Sort(params :+ "ALPHA")
  }
  object Sort {
    final val default: Sort = new Sort(Nil)

    def by(pattern: Key): Sort                          = default.by(pattern)
    def noSort: Sort                                    = default.noSort
    def limit(offset: NonNegLong, count: PosLong): Sort = default.limit(offset, count)
    def asc: Sort                                       = default.asc
    def desc: Sort                                      = default.desc
    def alpha: Sort                                     = default.alpha
  }

  /** GET, for SORT and SORT_RO: `#` for the element itself, or a key name with `*` and `->field` for a hash field */
  final class SortGet private (val params: List[String])
  object SortGet {
    def apply(pattern: Key, more: Key*): SortGet = new SortGet((pattern :: more.toList).flatMap(p => List("GET", p.value)))
  }

  /** NX, XX, GT and LT, for EXPIRE, PEXPIRE, EXPIREAT and PEXPIREAT */
  sealed trait ExpireCondition  { def params: List[String] }
  trait ExpireConditionBuilders {
    val nx: ExpireCondition
    val xx: ExpireCondition
    val gt: ExpireCondition
    val lt: ExpireCondition
    val xxGt: ExpireCondition
    val xxLt: ExpireCondition
  }
  private[laserdisc] object ExpireCondition extends ExpireConditionBuilders {
    private final class Of(override final val params: List[String]) extends ExpireCondition

    final val nx: ExpireCondition   = new Of(List("NX"))
    final val xx: ExpireCondition   = new Of(List("XX"))
    final val gt: ExpireCondition   = new Of(List("GT"))
    final val lt: ExpireCondition   = new Of(List("LT"))
    final val xxGt: ExpireCondition = new Of(List("XX", "GT"))
    final val xxLt: ExpireCondition = new Of(List("XX", "LT"))
  }

  sealed trait MigrateMode  { def params: List[String] }
  trait MigrateModeBuilders {
    val copy: MigrateMode
    val replace: MigrateMode
    val both: MigrateMode
  }
  private[laserdisc] object MigrateMode extends MigrateModeBuilders {
    object copy    extends MigrateMode { override final val params: List[String] = List("COPY")            }
    object replace extends MigrateMode { override final val params: List[String] = List("REPLACE")         }
    object both    extends MigrateMode { override final val params: List[String] = List("COPY", "REPLACE") }
  }

  sealed trait RestoreEviction { def param: String; def seconds: NonNegInt }
  object RestoreEviction       {
    final case class IdleTime(override final val seconds: NonNegInt) extends RestoreEviction {
      override final val param: String = "IDLETIME"
    }
    final case class Frequency(override final val seconds: NonNegInt) extends RestoreEviction {
      override final val param: String = "FREQ"
    }
  }

  sealed trait RestoreMode { def params: List[String] }
  object RestoreMode       {
    case object replace     extends RestoreMode { override final val params: List[String] = List("REPLACE")           }
    case object absolutettl extends RestoreMode { override final val params: List[String] = List("ABSTTL")            }
    case object both        extends RestoreMode { override final val params: List[String] = List("REPLACE", "ABSTTL") }
  }

  /** What TYPE replies, and what SCAN can look for. `array` is from Redis 8.8, and `other` is any type of a server's module, like
    * `vectorset`
    */
  sealed trait Type
  object Type {
    case object string                extends Type
    case object list                  extends Type
    case object set                   extends Type
    case object zset                  extends Type
    case object hash                  extends Type
    case object stream                extends Type
    case object array                 extends Type
    final case class other(name: Key) extends Type

    implicit final val typeShow: Show[Type] = Show.instance {
      case `string` => "string"
      case `list`   => "list"
      case `set`    => "set"
      case `zset`   => "zset"
      case `hash`   => "hash"
      case `stream` => "stream"
      case `array`  => "array"
      case other(n) => n.value
    }
  }

  sealed trait TTLResponse

  /** The reply of EXPIRETIME and PEXPIRETIME */
  sealed trait ExpireTimeResponse

  object TTLResponse {
    case object NoKey                             extends TTLResponse with ExpireTimeResponse
    case object NoExpire                          extends TTLResponse with ExpireTimeResponse
    final case class ExpireAfter(ttl: NonNegLong) extends TTLResponse

    implicit final val num2TTLResponseRead: Read[Num, TTLResponse] = Read.instance {
      case Num(-2)          => Right(NoKey)
      case Num(-1)          => Right(NoExpire)
      case Num(l) if l >= 0 => Right(ExpireAfter(NonNegLong.unsafeFrom(l)))
      case Num(other)       => Left(RESPDecErr(s"Unexpected key TTL. Was $other"))
    }
  }

  private[protocol] final val localAndReplicas: Read[Arr.AsTuple[Num *: Num *: EmptyTuple], (NonNegInt, NonNegInt)] =
    Read[Arr.AsTuple[Num *: Num *: EmptyTuple], (NonNegInt, NonNegInt)]

  object ExpireTimeResponse {
    final case class ExpireAt(timestamp: NonNegLong) extends ExpireTimeResponse

    implicit final val num2ExpireTimeResponseRead: Read[Num, ExpireTimeResponse] = Read.instance {
      case Num(-2)          => Right(TTLResponse.NoKey)
      case Num(-1)          => Right(TTLResponse.NoExpire)
      case Num(l) if l >= 0 => Right(ExpireAt(NonNegLong.unsafeFrom(l)))
      case Num(other)       => Left(RESPDecErr(s"Unexpected key expire time. Was $other"))
    }
  }

  // the builders each feature's commands take, and the types of their replies
  trait KeyTypes {
    final type KeyReplace         = KeyP.Replace
    final type KeySort            = KeyP.Sort
    final type KeySortGet         = KeyP.SortGet
    final type KeyRestoreEviction = KeyP.RestoreEviction
    final type KeyRestoreMode     = KeyP.RestoreMode
    final type KeyType            = KeyP.Type
    final type KeyTTLResponse     = KeyP.TTLResponse
    final val KeyReplace                = KeyP.Replace
    final val KeySort                   = KeyP.Sort
    final val KeySortGet                = KeyP.SortGet
    final val KeyIdleTimeEviction       = KeyP.RestoreEviction.IdleTime
    final val KeyFrequencyEviction      = KeyP.RestoreEviction.Frequency
    final val KeyRestoreMode            = KeyP.RestoreMode
    final val KeyType                   = KeyP.Type
    final val KeyNoKeyTTLResponse       = KeyP.TTLResponse.NoKey
    final val KeyNoExpireTTLResponse    = KeyP.TTLResponse.NoExpire
    final val KeyExpireAfterTTLResponse = KeyP.TTLResponse.ExpireAfter
  }
  trait KeyObjectTypes {
    final type KeyEncoding = KeyP.Encoding
    final val KeyEncoding = KeyP.Encoding
  }
  trait KeyExpireTypes {
    final type KeyExpireCondition = KeyP.ExpireCondition
    final type KeyExpireTime      = KeyP.ExpireTimeResponse
    final val KeyExpireCondition: KeyP.ExpireConditionBuilders = KeyP.ExpireCondition
    final val KeyExpireAt                                      = KeyP.ExpireTimeResponse.ExpireAt
  }
  trait KeyMigrateTypes {
    final type KeyMigrateAuth = KeyP.MigrateAuth
    final type KeyMigrateMode = KeyP.MigrateMode
    final val KeyMigrateAuth: KeyP.MigrateAuthBuilders = KeyP.MigrateAuth
    final val KeyMigrateMode: KeyP.MigrateModeBuilders = KeyP.MigrateMode
  }

  // the same names for laserdisc's own code, which can't read the members a version overrides while it's being built
  private[protocol] object internal extends KeyTypes with KeyObjectTypes with KeyExpireTypes with KeyMigrateTypes
}

trait KeyBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "COPY: REPLACE",
      "DEL",
      "DUMP",
      "EXISTS",
      "EXPIRE",
      "EXPIREAT",
      "KEYS",
      "MOVE",
      "PERSIST",
      "PEXPIRE",
      "PEXPIREAT",
      "PTTL",
      "RANDOMKEY",
      "RENAME",
      "RENAMENX",
      "RESTORE: ABSTTL FREQ IDLETIME REPLACE",
      "SCAN: COUNT MATCH TYPE",
      "SORT: ALPHA ASC BY DESC GET LIMIT STORE",
      "TOUCH",
      "TTL",
      "TYPE",
      "UNLINK",
      "WAIT"
    )

  /** The builders of this family's commands, and the types of their replies */
  val keytypes: KeyP.KeyTypes = new KeyP.KeyTypes {}

  import KeyP.internal.*

  private final val zeroIsNone = RESPRead.instance(Read.numZeroIsNone[PosInt])

  private implicit final val str2OptionType: Read[Str, Option[KeyType]] = Read.instance {
    case Str("string") => Right(Some(KeyType.string))
    case Str("list")   => Right(Some(KeyType.list))
    case Str("set")    => Right(Some(KeyType.set))
    case Str("zset")   => Right(Some(KeyType.zset))
    case Str("hash")   => Right(Some(KeyType.hash))
    case Str("stream") => Right(Some(KeyType.stream))
    case Str("array")  => Right(Some(KeyType.array))
    case Str("none")   => Right(None)
    case Str(other)    => Read[Bulk, Key].read(Bulk(other)).map(name => Some(KeyType.other(name)))
  }

  final def copy(source: Key, destination: Key): Single.Aux[Boolean] =
    Protocol("COPY", source *: destination *: EmptyTuple).as[Num, Boolean]
  final def copy(source: Key, destination: Key, replace: KeyReplace): Single.Aux[Boolean] =
    Protocol("COPY", source *: destination *: replace *: EmptyTuple).as[Num, Boolean]

  final def del(keys: OneOrMoreKeys): Single.Aux[NonNegInt] = Protocol("DEL", keys.value).as[Num, NonNegInt]

  final def dump(key: Key): Single.Aux[Option[Bulk]] = Protocol("DUMP", key).as[OrNullBulk[Bulk], Option[Bulk]]

  final def exists(keys: OneOrMoreKeys): Single.Aux[Option[PosInt]] = Protocol("EXISTS", keys.value).using(zeroIsNone)

  // TODO check if we must support deletions via timeout < 0
  final def expire(key: Key, seconds: NonNegInt): Single.Aux[Boolean] = Protocol("EXPIRE", key *: seconds *: EmptyTuple).as[Num, Boolean]

  final def expireat(key: Key, seconds: NonNegInt): Single.Aux[Boolean] =
    Protocol("EXPIREAT", key *: seconds *: EmptyTuple).as[Num, Boolean]

  final def keys(pattern: GlobPattern): Single.Aux[Seq[Key]] = Protocol("KEYS", pattern *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]

  final def move(key: Key, db: DbIndex): Single.Aux[Boolean] = Protocol("MOVE", key *: db *: EmptyTuple).as[Num, Boolean]

  final def persist(key: Key): Single.Aux[Boolean] = Protocol("PERSIST", key).as[Num, Boolean]

  // TODO check if we must support deletions via timeout < 0
  final def pexpire(key: Key, milliseconds: NonNegLong): Single.Aux[Boolean] =
    Protocol("PEXPIRE", key *: milliseconds *: EmptyTuple).as[Num, Boolean]

  final def pexpireat(key: Key, millisecondsTimestamp: NonNegLong): Single.Aux[Boolean] =
    Protocol("PEXPIREAT", key *: millisecondsTimestamp *: EmptyTuple).as[Num, Boolean]

  final def pttl(key: Key): Single.Aux[KeyTTLResponse] = Protocol("PTTL", key).as[Num, KeyTTLResponse]

  final val randomkey: Single.Aux[Option[Key]] = Protocol("RANDOMKEY", EmptyTuple).as[OrNullBulk[Bulk], Option[Key]]

  final def rename(key: Key, newKey: Key): Single.Aux[OK] = Protocol("RENAME", key :: newKey :: Nil).as[Str, OK]

  final def renamenx(key: Key, newKey: Key): Single.Aux[Boolean] = Protocol("RENAMENX", key :: newKey :: Nil).as[Num, Boolean]

  final def restore(key: Key, ttl: NonNegLong, serializedValue: Bulk): Single.Aux[OK] =
    Protocol("RESTORE", key *: ttl *: serializedValue *: EmptyTuple).as[Str, OK]
  final def restore(key: Key, ttl: NonNegLong, serializedValue: Bulk, mode: KeyRestoreMode): Single.Aux[OK] =
    Protocol("RESTORE", key *: ttl *: serializedValue *: mode.params *: EmptyTuple).as[Str, OK]
  final def restore(key: Key, ttl: NonNegLong, serializedValue: Bulk, eviction: KeyRestoreEviction): Single.Aux[OK] =
    Protocol("RESTORE", key *: ttl *: serializedValue *: eviction.param *: eviction.seconds *: EmptyTuple).as[Str, OK]
  final def restore(
      key: Key,
      ttl: NonNegLong,
      serializedValue: Bulk,
      mode: KeyRestoreMode,
      eviction: KeyRestoreEviction
  ): Single.Aux[OK] =
    Protocol("RESTORE", key *: ttl *: serializedValue *: mode.params *: eviction.param *: eviction.seconds *: EmptyTuple).as[Str, OK]

  final def scan(cursor: NonNegLong): Single.Aux[Scan[Key]] = Protocol("SCAN", cursor).as[ScanOf[Arr.Of[Bulk]], Scan[Key]]
  final def scan(cursor: NonNegLong, pattern: GlobPattern): Single.Aux[Scan[Key]] =
    Protocol("SCAN", cursor *: "MATCH" *: pattern *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[Key]]
  final def scan(cursor: NonNegLong, count: PosInt): Single.Aux[Scan[Key]] =
    Protocol("SCAN", cursor *: "COUNT" *: count *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[Key]]
  final def scan(cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Single.Aux[Scan[Key]] =
    Protocol("SCAN", cursor *: "MATCH" *: pattern *: "COUNT" *: count *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[Key]]
  final def scan(cursor: NonNegLong, keyType: KeyType): Single.Aux[Scan[Key]] =
    Protocol("SCAN", cursor *: "TYPE" *: keyType *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[Key]]
  final def scan(cursor: NonNegLong, pattern: GlobPattern, keyType: KeyType): Single.Aux[Scan[Key]] =
    Protocol("SCAN", cursor *: "MATCH" *: pattern *: "TYPE" *: keyType *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[Key]]
  final def scan(cursor: NonNegLong, count: PosInt, keyType: KeyType): Single.Aux[Scan[Key]] =
    Protocol("SCAN", cursor *: "COUNT" *: count *: "TYPE" *: keyType *: EmptyTuple).as[ScanOf[Arr.Of[Bulk]], Scan[Key]]
  final def scan(cursor: NonNegLong, pattern: GlobPattern, count: PosInt, keyType: KeyType): Single.Aux[Scan[Key]] =
    Protocol("SCAN", cursor *: "MATCH" *: pattern *: "COUNT" *: count *: "TYPE" *: keyType *: EmptyTuple)
      .as[ScanOf[Arr.Of[Bulk]], Scan[Key]]

  /** SCAN from cursor 0 until the server is done, as a [[Loop]]. A key may come more than once */
  final def scanAll: Loop[Key]                                                        = Loop.cursor(scan(_))(scanned)
  final def scanAll(pattern: GlobPattern): Loop[Key]                                  = Loop.cursor(scan(_, pattern))(scanned)
  final def scanAll(count: PosInt): Loop[Key]                                         = Loop.cursor(scan(_, count))(scanned)
  final def scanAll(pattern: GlobPattern, count: PosInt): Loop[Key]                   = Loop.cursor(scan(_, pattern, count))(scanned)
  final def scanAll(keyType: KeyType): Loop[Key]                                      = Loop.cursor(scan(_, keyType))(scanned)
  final def scanAll(pattern: GlobPattern, keyType: KeyType): Loop[Key]                = Loop.cursor(scan(_, pattern, keyType))(scanned)
  final def scanAll(count: PosInt, keyType: KeyType): Loop[Key]                       = Loop.cursor(scan(_, count, keyType))(scanned)
  final def scanAll(pattern: GlobPattern, count: PosInt, keyType: KeyType): Loop[Key] =
    Loop.cursor(scan(_, pattern, count, keyType))(scanned)

  final def sort[A: Read[Bulk, _]](key: Key): Single.Aux[Seq[A]]                       = Protocol("SORT", key).as[Arr.Of[Bulk], Seq[A]]
  final def sort[A: Read[Bulk, _]](key: Key, pattern: GlobPattern): Single.Aux[Seq[A]] =
    Protocol("SORT", key *: "BY" *: pattern *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def sort[A: Read[Bulk, _]](key: Key, offset: NonNegLong, count: PosLong): Single.Aux[Seq[A]] =
    Protocol("SORT", key *: "LIMIT" *: offset *: count *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def sort[A: Read[Bulk, _]](key: Key, direction: Direction): Single.Aux[Seq[A]] =
    Protocol("SORT", key *: direction *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def sort(key: Key, destination: Key): Single.Aux[NonNegInt] =
    Protocol("SORT", key.value :: "STORE" :: destination.value :: Nil).as[Num, NonNegInt]
  final def sort[A: Read[Bulk, _]](key: Key, options: KeySort): Single.Aux[Seq[A]] =
    Protocol("SORT", key *: options.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def sort[A: Read[Bulk, _]](key: Key, options: KeySort, get: KeySortGet): Single.Aux[Seq[Option[A]]] =
    Protocol("SORT", key *: options.params *: get.params *: EmptyTuple).as[Arr.Of[OrNullBulk[Bulk]], Seq[Option[A]]]
  final def sort(key: Key, options: KeySort, destination: Key): Single.Aux[NonNegInt] =
    Protocol("SORT", key *: options.params *: "STORE" *: destination *: EmptyTuple).as[Num, NonNegInt]
  final def sort(key: Key, options: KeySort, get: KeySortGet, destination: Key): Single.Aux[NonNegInt] =
    Protocol("SORT", key *: options.params *: get.params *: "STORE" *: destination *: EmptyTuple).as[Num, NonNegInt]

  final def touch(keys: OneOrMoreKeys): Single.Aux[NonNegInt] = Protocol("TOUCH", keys.value).as[Num, NonNegInt]

  final def ttl(key: Key): Single.Aux[KeyTTLResponse] = Protocol("TTL", key).as[Num, KeyTTLResponse]

  final def typeof(key: Key): Single.Aux[Option[KeyType]] = Protocol("TYPE", key).as[OrNullBulk[Str], Option[KeyType]]

  final def unlink(keys: OneOrMoreKeys): Single.Aux[NonNegInt] = Protocol("UNLINK", keys.value).as[Num, NonNegInt]

  final def wait(replicas: PosInt): Single.Aux[PosInt]                   = Protocol("WAIT", replicas *: 0 *: EmptyTuple).as[Num, PosInt]
  final def wait(replicas: PosInt, timeout: PosLong): Single.Aux[PosInt] =
    Protocol("WAIT", replicas *: timeout *: EmptyTuple).as[Num, PosInt]
}

/** NX, XX, GT and LT for EXPIRE, PEXPIRE, EXPIREAT and PEXPIREAT, and EXPIRETIME and PEXPIRETIME: Redis 7.0 */
trait KeyExpireConditionP extends Features {
  import KeyP.{ExpireCondition, ExpireTimeResponse}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "EXPIRE: GT LT NX XX",
      "EXPIREAT: GT LT NX XX",
      "EXPIRETIME",
      "PEXPIRE: GT LT NX XX",
      "PEXPIREAT: GT LT NX XX",
      "PEXPIRETIME"
    )

  final def expire(key: Key, seconds: NonNegInt, condition: ExpireCondition): Single.Aux[Boolean] =
    Protocol("EXPIRE", key *: seconds *: condition.params *: EmptyTuple).as[Num, Boolean]

  final def expireat(key: Key, seconds: NonNegInt, condition: ExpireCondition): Single.Aux[Boolean] =
    Protocol("EXPIREAT", key *: seconds *: condition.params *: EmptyTuple).as[Num, Boolean]

  final def expiretime(key: Key): Single.Aux[ExpireTimeResponse] = Protocol("EXPIRETIME", key).as[Num, ExpireTimeResponse]

  final def pexpire(key: Key, milliseconds: NonNegLong, condition: ExpireCondition): Single.Aux[Boolean] =
    Protocol("PEXPIRE", key *: milliseconds *: condition.params *: EmptyTuple).as[Num, Boolean]

  final def pexpireat(key: Key, millisecondsTimestamp: NonNegLong, condition: ExpireCondition): Single.Aux[Boolean] =
    Protocol("PEXPIREAT", key *: millisecondsTimestamp *: condition.params *: EmptyTuple).as[Num, Boolean]

  final def pexpiretime(key: Key): Single.Aux[ExpireTimeResponse] = Protocol("PEXPIRETIME", key).as[Num, ExpireTimeResponse]
}

/** SORT_RO: Redis 7.0 */
trait KeySortReadOnlyP extends Features {
  import KeyP.{Sort, SortGet}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SORT_RO: ALPHA ASC BY DESC GET LIMIT")

  final def sortro[A: Read[Bulk, _]](key: Key, options: Sort): Single.Aux[Seq[A]] =
    Protocol("SORT_RO", key *: options.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[A]]
  final def sortro[A: Read[Bulk, _]](key: Key, options: Sort, get: SortGet): Single.Aux[Seq[Option[A]]] =
    Protocol("SORT_RO", key *: options.params *: get.params *: EmptyTuple).as[Arr.Of[OrNullBulk[Bulk]], Seq[Option[A]]]
}

/** WAITAOF: Redis 7.2 */
trait KeyWaitAofP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("WAITAOF")

  /** Waits until the writes before it are on disk locally (when `local` is 1) and on `replicas` replicas, or until `timeout` milliseconds
    * have passed (0 waits forever). Replies with how many of each it reached
    */
  final def waitaof(local: NonNegInt, replicas: NonNegInt, timeout: NonNegLong): Single.Aux[(NonNegInt, NonNegInt)] =
    Protocol("WAITAOF", local *: replicas *: timeout *: EmptyTuple).using(RESPRead.instance(KeyP.localAndReplicas))
}

/** COPY to another database */
trait KeyCopyDbP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("COPY: DB")

  final def copy(source: Key, destination: Key, db: DbIndex): Single.Aux[Boolean] =
    Protocol("COPY", source *: destination *: "DB" *: db *: EmptyTuple).as[Num, Boolean]
  final def copy(source: Key, destination: Key, db: DbIndex, replace: KeyP.Replace): Single.Aux[Boolean] =
    Protocol("COPY", source *: destination *: "DB" *: db *: replace *: EmptyTuple).as[Num, Boolean]
}

/** MIGRATE */
trait KeyMigrateP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("MIGRATE: AUTH AUTH2 COPY KEYS REPLACE")

  private implicit final val str2NOKEYOrOK: Read[Str, Either[NOKEY, OK]] = Read.instance {
    case Str("NOKEY") => Right(Left(NOKEY))
    case Str("OK")    => Right(Right(OK))
    case Str(other)   => Left(RESPDecErr(s"Unexpected string for Read[Str, (NOKEY | OK)]. Was $other"))
  }

  final def migrate(key: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single.Aux[Either[NOKEY, OK]] =
    Protocol("MIGRATE", host *: port *: key *: dbIndex *: timeout *: EmptyTuple).as[Str, Either[NOKEY, OK]]
  final def migrate(keys: TwoOrMoreKeys, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single.Aux[Either[NOKEY, OK]] =
    Protocol("MIGRATE", host *: port *: "" *: dbIndex *: timeout *: "KEYS" *: keys.value *: EmptyTuple).as[Str, Either[NOKEY, OK]]
  final def migrate(
      key: Key,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt,
      mode: KeyP.MigrateMode
  ): Single.Aux[Either[NOKEY, OK]] =
    Protocol("MIGRATE", host *: port *: key *: dbIndex *: timeout *: mode.params *: EmptyTuple).as[Str, Either[NOKEY, OK]]
  final def migrate(
      keys: TwoOrMoreKeys,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt,
      mode: KeyP.MigrateMode
  ): Single.Aux[Either[NOKEY, OK]] =
    Protocol("MIGRATE", host *: port *: "" *: dbIndex *: timeout *: mode.params *: "KEYS" *: keys.value *: EmptyTuple)
      .as[Str, Either[NOKEY, OK]]
  final def migrate(
      key: Key,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt,
      auth: KeyP.MigrateAuth
  ): Single.Aux[Either[NOKEY, OK]] =
    Protocol("MIGRATE", host *: port *: key *: dbIndex *: timeout *: auth.params *: EmptyTuple).as[Str, Either[NOKEY, OK]]
  final def migrate(
      keys: TwoOrMoreKeys,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt,
      auth: KeyP.MigrateAuth
  ): Single.Aux[Either[NOKEY, OK]] =
    Protocol("MIGRATE", host *: port *: "" *: dbIndex *: timeout *: auth.params *: "KEYS" *: keys.value *: EmptyTuple)
      .as[Str, Either[NOKEY, OK]]
  final def migrate(
      key: Key,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt,
      mode: KeyP.MigrateMode,
      auth: KeyP.MigrateAuth
  ): Single.Aux[Either[NOKEY, OK]] =
    Protocol("MIGRATE", host *: port *: key *: dbIndex *: timeout *: mode.params *: auth.params *: EmptyTuple).as[Str, Either[NOKEY, OK]]
  final def migrate(
      keys: TwoOrMoreKeys,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt,
      mode: KeyP.MigrateMode,
      auth: KeyP.MigrateAuth
  ): Single.Aux[Either[NOKEY, OK]] =
    Protocol("MIGRATE", host *: port *: "" *: dbIndex *: timeout *: mode.params *: auth.params *: "KEYS" *: keys.value *: EmptyTuple)
      .as[Str, Either[NOKEY, OK]]
}

/** OBJECT */
trait KeyObjectP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("OBJECT ENCODING", "OBJECT FREQ", "OBJECT HELP", "OBJECT IDLETIME", "OBJECT REFCOUNT")

  object obj {
    val help: Single.Aux[Seq[String]] = Protocol.help("OBJECT")

    def encoding(key: Key): Single.Aux[Option[KeyP.Encoding]] =
      Protocol("OBJECT", "ENCODING" :: key.value :: Nil).as[OrNullBulk[Bulk], Option[KeyP.Encoding]]

    def freq(key: Key): Single.Aux[NonNegInt] = Protocol("OBJECT", "FREQ" :: key.value :: Nil).as[Num, NonNegInt]

    def idletime(key: Key): Single.Aux[NonNegInt] = Protocol("OBJECT", "IDLETIME" :: key.value :: Nil).as[Num, NonNegInt]

    def refcount(key: Key): Single.Aux[NonNegInt] = Protocol("OBJECT", "REFCOUNT" :: key.value :: Nil).as[Num, NonNegInt]
  }
}

trait KeyP
    extends KeyBaseP
    with KeyExtP
    with KeyCopyDbP
    with KeyMigrateP
    with KeyMigrateExtP
    with KeyObjectP
    with KeyExpireConditionP
    with KeySortReadOnlyP
    with KeyWaitAofP {
  override val keytypes: KeyP.KeyTypes with KeyP.KeyObjectTypes with KeyP.KeyExpireTypes with KeyP.KeyMigrateTypes = new KeyP.KeyTypes
    with KeyP.KeyObjectTypes
    with KeyP.KeyExpireTypes
    with KeyP.KeyMigrateTypes {}
}
