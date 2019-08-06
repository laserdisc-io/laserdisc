package laserdisc
package protocol

object KeyP {
  sealed trait Encoding
  final object Encoding {
    final case object raw        extends Encoding
    final case object int        extends Encoding
    final case object ziplist    extends Encoding
    final case object linkedlist extends Encoding
    final case object intset     extends Encoding
    final case object hashtable  extends Encoding
    final case object skiplist   extends Encoding

    implicit final val bulk2EncodingRead: Bulk ==> Encoding = Read.instancePF {
      case Bulk("raw")        => raw
      case Bulk("int")        => int
      case Bulk("ziplist")    => ziplist
      case Bulk("linkedlist") => linkedlist
      case Bulk("intset")     => intset
      case Bulk("hashtable")  => hashtable
      case Bulk("skiplist")   => skiplist
    }
  }

  sealed trait MigrateMode { def params: List[String] }
  final object MigrateMode {
    final object copy    extends MigrateMode { override final val params: List[String] = List("COPY")            }
    final object replace extends MigrateMode { override final val params: List[String] = List("REPLACE")         }
    final object both    extends MigrateMode { override final val params: List[String] = List("COPY", "REPLACE") }
  }

  sealed trait Type
  final object Type {
    final case object string extends Type
    final case object list   extends Type
    final case object set    extends Type
    final case object zset   extends Type
    final case object hash   extends Type
  }

  sealed trait TTLResponse
  object TTLResponse {
    final case object NoKey                       extends TTLResponse
    final case object NoExpire                    extends TTLResponse
    final case class ExpireAfter(ttl: NonNegLong) extends TTLResponse

    implicit final val num2TTLResponseRead: Num ==> TTLResponse = Read.instancePF {
      case Num(-2)          => NoKey
      case Num(-1)          => NoExpire
      case Num(l) if l >= 0 => ExpireAfter(NonNegLong.unsafeFrom(l))
    }
  }
}

trait KeyBaseP {
  import shapeless._

  final object keytypes {
    final type KeyEncoding    = KeyP.Encoding
    final type KeyMigrateMode = KeyP.MigrateMode
    final type KeyType        = KeyP.Type
    final type KeyTTLResponse = KeyP.TTLResponse

    final val KeyEncoding    = KeyP.Encoding
    final val KeyMigrateMode = KeyP.MigrateMode
    final val KeyType        = KeyP.Type
    final val KeyTTLResponse = KeyP.TTLResponse
  }

  import keytypes._

  private[this] implicit final val str2NOKEYOrOK: Str ==> (NOKEY | OK) = Read.instancePF {
    case Str("NOKEY") => Left(NOKEY)
    case Str("OK")    => Right(OK)
  }

  private[this] final val zeroIsNone = RESPRead.instance(Read.numZeroIsNone[PosInt])

  private[this] implicit final val str2OptionType: Str ==> Option[KeyType] = Read.instancePF {
    case Str("none")   => None
    case Str("string") => Some(KeyType.string)
    case Str("list")   => Some(KeyType.list)
    case Str("set")    => Some(KeyType.set)
    case Str("zset")   => Some(KeyType.zset)
    case Str("hash")   => Some(KeyType.hash)
  }

  final def del(keys: OneOrMoreKeys): Protocol.Aux[NonNegInt] = Protocol("DEL", keys.value).as[Num, NonNegInt]

  final def dump(key: Key): Protocol.Aux[Option[Bulk]] = Protocol("DUMP", key).opt[GenBulk].as[Bulk]

  final def exists(keys: OneOrMoreKeys): Protocol.Aux[Option[PosInt]] = Protocol("EXISTS", keys.value).using(zeroIsNone)

  //TODO check if we must support deletions via timeout < 0
  final def expire(key: Key, seconds: NonNegInt): Protocol.Aux[Boolean] = Protocol("EXPIRE", key :: seconds :: HNil).as[Num, Boolean]

  final def expireat(key: Key, seconds: NonNegInt): Protocol.Aux[Boolean] = Protocol("EXPIREAT", key :: seconds :: HNil).as[Num, Boolean]

  final def keys(pattern: GlobPattern): Protocol.Aux[Seq[Key]] = Protocol("KEYS", pattern :: HNil).as[Arr, Seq[Key]]

  final def migrate(key: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    Protocol("MIGRATE", host :: port :: key :: dbIndex :: timeout :: HNil).as[Str, NOKEY | OK]
  final def migrate(keys: TwoOrMoreKeys, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    Protocol("MIGRATE", host :: port :: "" :: dbIndex :: timeout :: "KEYS" :: keys.value :: HNil).as[Str, NOKEY | OK]
  final def migrate(
      key: Key,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt,
      mode: KeyMigrateMode
  ): Protocol.Aux[NOKEY | OK] = Protocol("MIGRATE", host :: port :: key :: dbIndex :: timeout :: mode.params :: HNil).as[Str, NOKEY | OK]
  final def migrate(
      keys: TwoOrMoreKeys,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt,
      mode: KeyMigrateMode
  ): Protocol.Aux[NOKEY | OK] =
    Protocol("MIGRATE", host :: port :: "" :: dbIndex :: timeout :: mode.params :: "KEYS" :: keys.value :: HNil)
      .as[Str, NOKEY | OK]

  final def move(key: Key, db: DbIndex): Protocol.Aux[Boolean] = Protocol("MOVE", key :: db :: HNil).as[Num, Boolean]

  final object obj {
    def encoding(key: Key): Protocol.Aux[Option[KeyEncoding]] =
      Protocol("OBJECT", "ENCODING" :: key.value :: Nil).opt[GenBulk].as[KeyEncoding]

    def freq(key: Key): Protocol.Aux[NonNegLong] = Protocol("OBJECT", "FREQ" :: key.value :: Nil).as[Num, NonNegLong]

    def idletime(key: Key): Protocol.Aux[Option[NonNegInt]] = Protocol("OBJECT", "IDLETIME" :: key.value :: Nil).opt[GenBulk].as[NonNegInt]

    def refcount(key: Key): Protocol.Aux[NonNegInt] = Protocol("OBJECT", "REFCOUNT" :: key.value :: Nil).as[Num, NonNegInt]
  }

  final def persist(key: Key): Protocol.Aux[Boolean] = Protocol("PERSIST", key).as[Num, Boolean]

  //TODO check if we must support deletions via timeout < 0
  final def pexpire(key: Key, milliseconds: NonNegLong): Protocol.Aux[Boolean] =
    Protocol("PEXPIRE", key :: milliseconds :: HNil).as[Num, Boolean]

  final def pexpireat(key: Key, millisecondsTimestamp: NonNegLong): Protocol.Aux[Boolean] =
    Protocol("PEXPIREAT", key :: millisecondsTimestamp :: HNil).as[Num, Boolean]

  final def pttl(key: Key): Protocol.Aux[KeyTTLResponse] = Protocol("PTTL", key).as[Num, KeyTTLResponse]

  final val randomKey: Protocol.Aux[Option[Key]] = Protocol("RANDOMKEY", Nil).opt[GenBulk].as[Key]

  final def rename(key: Key, newKey: Key): Protocol.Aux[Key] = Protocol("RENAME", key :: newKey :: Nil).as[Str, Key]

  final def renamenx(key: Key, newKey: Key): Protocol.Aux[Boolean] = Protocol("RENAMENX", key :: newKey :: Nil).as[Num, Boolean]

  final def restore(key: Key, ttl: NonNegLong, serializedValue: Bulk): Protocol.Aux[OK] =
    Protocol("RESTORE", key :: ttl :: serializedValue :: HNil).as[Str, OK]

  final def restorereplace(key: Key, ttl: NonNegLong, serializedValue: Bulk): Protocol.Aux[OK] =
    Protocol("RESTORE", key :: ttl :: serializedValue :: "REPLACE" :: HNil).as[Str, OK]

  final def scan(cursor: NonNegLong): Protocol.Aux[Scan[Key]] = Protocol("SCAN", cursor).as[Arr, Scan[Key]]
  final def scan(cursor: NonNegLong, pattern: GlobPattern): Protocol.Aux[Scan[Key]] =
    Protocol("SCAN", cursor :: "MATCH" :: pattern :: HNil).as[Arr, Scan[Key]]
  final def scan(cursor: NonNegLong, count: PosInt): Protocol.Aux[Scan[Key]] =
    Protocol("SCAN", cursor :: "COUNT" :: count :: HNil).as[Arr, Scan[Key]]
  final def scan(cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Protocol.Aux[Scan[Key]] =
    Protocol("SCAN", cursor :: "MATCH" :: pattern :: "COUNT" :: count :: HNil).as[Arr, Scan[Key]]

  //FIXME sort has many more combinations
  final def sort[A: Bulk ==> ?](key: Key): Protocol.Aux[Seq[A]] = Protocol("SORT", key).as[Arr, Seq[A]]
  final def sort[A: Bulk ==> ?](key: Key, pattern: GlobPattern): Protocol.Aux[Seq[A]] =
    Protocol("SORT", key :: "BY" :: pattern :: HNil).as[Arr, Seq[A]]
  final def sort[A: Bulk ==> ?](key: Key, offset: NonNegLong, count: PosLong): Protocol.Aux[Seq[A]] =
    Protocol("SORT", key :: "LIMIT" :: offset :: count :: HNil).as[Arr, Seq[A]]
  final def sort[A: Bulk ==> ?](key: Key, direction: Direction): Protocol.Aux[Seq[A]] =
    Protocol("SORT", key :: direction :: HNil).as[Arr, Seq[A]]

  final def sortstore(key: Key, destination: Key): Protocol.Aux[NonNegInt] =
    Protocol("SORT", key.value :: "STORE" :: destination.value :: Nil).as[Num, NonNegInt]

  final def touch(keys: OneOrMoreKeys): Protocol.Aux[NonNegInt] = Protocol("TOUCH", keys.value).as[Num, NonNegInt]

  final def ttl(key: Key): Protocol.Aux[KeyTTLResponse] = Protocol("TTL", key).as[Num, KeyTTLResponse]

  final def typeof(key: Key): Protocol.Aux[Option[KeyType]] = Protocol("TYPE", key).as[Str, Option[KeyType]]

  final def unlink(keys: OneOrMoreKeys): Protocol.Aux[NonNegInt] = Protocol("UNLINK", keys.value).as[Num, NonNegInt]

  final def waitFor(numSlaves: PosInt): Protocol.Aux[PosInt] = Protocol("WAIT", numSlaves :: 0 :: HNil).as[Num, PosInt]

  final def waitFor(numSlaves: PosInt, timeout: PosLong): Protocol.Aux[PosInt] =
    Protocol("WAIT", numSlaves :: timeout :: HNil).as[Num, PosInt]
}

trait KeyP extends KeyBaseP with KeyExtP
