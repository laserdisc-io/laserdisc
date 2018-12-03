package laserdisc
package protocol

object KeyP {
  import Read.==>

  sealed trait Direction
  final object Direction {
    final object asc  extends Direction
    final object desc extends Direction

    implicit val directionShow: Show[Direction] = Show.instance {
      case `asc`  => "ASC"
      case `desc` => "DESC"
    }
  }

  sealed trait Encoding
  object Encoding {
    final case object raw        extends Encoding
    final case object int        extends Encoding
    final case object ziplist    extends Encoding
    final case object linkedlist extends Encoding
    final case object intset     extends Encoding
    final case object hashtable  extends Encoding
    final case object skiplist   extends Encoding

    implicit final val nonEmptyBulkString2EncodingRead: NonNullBulkString ==> Encoding = Read.instancePF {
      case NonNullBulkString("raw")        => raw
      case NonNullBulkString("int")        => int
      case NonNullBulkString("ziplist")    => ziplist
      case NonNullBulkString("linkedlist") => linkedlist
      case NonNullBulkString("intset")     => intset
      case NonNullBulkString("hashtable")  => hashtable
      case NonNullBulkString("skiplist")   => skiplist
    }
  }

  sealed trait Type
  object Type {
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

    implicit final val integer2TTLResponseRead: Integer ==> TTLResponse = Read.instancePF {
      case Integer(-2)          => NoKey
      case Integer(-1)          => NoExpire
      case Integer(l) if l >= 0 => ExpireAfter(NonNegLong.unsafeFrom(l))
    }
  }
}

trait KeyP {
  import KeyP.{Direction, Encoding, Type, TTLResponse}
  import Read.==>
  import shapeless._

  private[this] implicit final val simpleString2NOKEYOrOK: SimpleString ==> (NOKEY | OK) = Read.instancePF {
    case SimpleString("NOKEY") => Left(NOKEY)
    case SimpleString("OK")    => Right(OK)
  }

  private[this] final val zeroIsNone = RESPRead.instance(Read.integerZeroIsNone[PosInt])

  private[this] implicit final val simpleString2OptionType: SimpleString ==> Option[Type] = Read.instancePF {
    case SimpleString("none")   => None
    case SimpleString("string") => Some(Type.string)
    case SimpleString("list")   => Some(Type.list)
    case SimpleString("set")    => Some(Type.set)
    case SimpleString("zset")   => Some(Type.zset)
    case SimpleString("hash")   => Some(Type.hash)
  }

  final object keys {
    final val direction = Direction
  }

  final def del(keys: OneOrMoreKeys): Protocol.Aux[NonNegInt] = Protocol("DEL", keys.value).as[Integer, NonNegInt]

  final def dump(key: Key): Protocol.Aux[Option[NonNullBulkString]] =
    Protocol("DUMP", key).asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[NonNullBulkString]]

  final def exists(keys: OneOrMoreKeys): Protocol.Aux[Option[PosInt]] = Protocol("EXISTS", keys.value).using(zeroIsNone)

  //TODO check if we must support deletions via timeout < 0
  final def expire(key: Key, seconds: NonNegInt): Protocol.Aux[Boolean] =
    Protocol("EXPIRE", key :: seconds :: HNil).as[Integer, Boolean]

  final def expireat(key: Key, seconds: NonNegInt): Protocol.Aux[Boolean] =
    Protocol("EXPIREAT", key :: seconds :: HNil).as[Integer, Boolean]

  final def keys(pattern: GlobPattern): Protocol.Aux[Seq[Key]] =
    Protocol("KEYS", pattern :: HNil).as[NonNilArray, Seq[Key]]

  final def migrate(key: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    Protocol("MIGRATE", host :: port :: key :: dbIndex :: timeout :: HNil).as[SimpleString, NOKEY | OK]

  final def migrate(
      keys: TwoOrMoreKeys,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt
  ): Protocol.Aux[NOKEY | OK] =
    Protocol("MIGRATE", host :: port :: "\"\"" :: dbIndex :: timeout :: "KEYS" :: keys.value :: HNil)
      .as[SimpleString, NOKEY | OK]

  final def migratecopy(
      key: Key,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt
  ): Protocol.Aux[NOKEY | OK] =
    Protocol("MIGRATE", host :: port :: key :: dbIndex :: timeout :: "COPY" :: HNil).as[SimpleString, NOKEY | OK]

  final def migratecopy(
      keys: TwoOrMoreKeys,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt
  ): Protocol.Aux[NOKEY | OK] =
    Protocol("MIGRATE", host :: port :: "\"\"" :: dbIndex :: timeout :: "COPY" :: "KEYS" :: keys.value :: HNil)
      .as[SimpleString, NOKEY | OK]

  final def migratereplace(
      key: Key,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt
  ): Protocol.Aux[NOKEY | OK] =
    Protocol("MIGRATE", host :: port :: key :: dbIndex :: timeout :: "REPLACE" :: HNil).as[SimpleString, NOKEY | OK]

  final def migratereplace(
      keys: TwoOrMoreKeys,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt
  ): Protocol.Aux[NOKEY | OK] =
    Protocol("MIGRATE", host :: port :: "\"\"" :: dbIndex :: timeout :: "REPLACE" :: "KEYS" :: keys.value :: HNil)
      .as[SimpleString, NOKEY | OK]

  final def migratecopyreplace(
      key: Key,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt
  ): Protocol.Aux[NOKEY | OK] =
    Protocol("MIGRATE", host :: port :: key :: dbIndex :: timeout :: "COPY" :: "REPLACE" :: HNil)
      .as[SimpleString, NOKEY | OK]

  final def migratecopyreplace(
      keys: TwoOrMoreKeys,
      host: Host,
      port: Port,
      dbIndex: DbIndex,
      timeout: NonNegInt
  ): Protocol.Aux[NOKEY | OK] =
    Protocol(
      "MIGRATE",
      host :: port :: "\"\"" :: dbIndex :: timeout :: "COPY" :: "REPLACE" :: "KEYS" :: keys.value :: HNil
    ).as[SimpleString, NOKEY | OK]

  final def move(key: Key, db: DbIndex): Protocol.Aux[Boolean] =
    Protocol("MOVE", key :: db :: HNil).as[Integer, Boolean]

  final object obj {
    def refcount(key: Key): Protocol.Aux[NonNegInt] =
      Protocol("OBJECT", "REFCOUNT" :: key.value :: Nil).as[Integer, NonNegInt]

    def encoding(key: Key): Protocol.Aux[Option[Encoding]] =
      Protocol("OBJECT", "ENCODING" :: key.value :: Nil)
        .asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[Encoding]]

    //FIXME add freq

    def idletime(key: Key): Protocol.Aux[Option[NonNegInt]] =
      Protocol("OBJECT", "IDLETIME" :: key.value :: Nil).asC[NullBulkString :+: Integer :+: CNil, Option[NonNegInt]]
  }

  final def persist(key: Key): Protocol.Aux[Boolean] = Protocol("PERSIST", key).as[Integer, Boolean]

  //TODO check if we must support deletions via timeout < 0
  final def pexpire(key: Key, milliseconds: NonNegLong): Protocol.Aux[Boolean] =
    Protocol("PEXPIRE", key :: milliseconds :: HNil).as[Integer, Boolean]

  final def pexpireat(key: Key, millisecondsTimestamp: NonNegLong): Protocol.Aux[Boolean] =
    Protocol("PEXPIREAT", key :: millisecondsTimestamp :: HNil).as[Integer, Boolean]

  final def pttl(key: Key): Protocol.Aux[TTLResponse] = Protocol("PTTL", key).as[Integer, TTLResponse]

  final val randomKey: Protocol.Aux[Option[Key]] =
    Protocol("RANDOMKEY", Nil).asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[Key]]

  final def rename(key: Key, newKey: Key): Protocol.Aux[Key] =
    Protocol("RENAME", key :: newKey :: Nil).as[SimpleString, Key]

  final def renamenx(key: Key, newKey: Key): Protocol.Aux[Boolean] =
    Protocol("RENAMENX", key :: newKey :: Nil).as[Integer, Boolean]

  final def restore(key: Key, ttl: NonNegLong, serializedValue: NonNullBulkString): Protocol.Aux[OK] =
    Protocol("RESTORE", key :: ttl :: serializedValue :: HNil).as[SimpleString, OK]

  final def restorereplace(key: Key, ttl: NonNegLong, serializedValue: NonNullBulkString): Protocol.Aux[OK] =
    Protocol("RESTORE", key :: ttl :: serializedValue :: "REPLACE" :: HNil).as[SimpleString, OK]

  final def scan(cursor: NonNegLong): Protocol.Aux[Scan[Key]] = Protocol("SCAN", cursor).as[NonNilArray, Scan[Key]]

  final def scan(cursor: NonNegLong, pattern: GlobPattern): Protocol.Aux[Scan[Key]] =
    Protocol("SCAN", cursor :: "MATCH" :: pattern :: HNil).as[NonNilArray, Scan[Key]]

  final def scan(cursor: NonNegLong, count: PosInt): Protocol.Aux[Scan[Key]] =
    Protocol("SCAN", cursor :: "COUNT" :: count :: HNil).as[NonNilArray, Scan[Key]]

  final def scan(cursor: NonNegLong, pattern: GlobPattern, count: PosInt): Protocol.Aux[Scan[Key]] =
    Protocol("SCAN", cursor :: "MATCH" :: pattern :: "COUNT" :: count :: HNil).as[NonNilArray, Scan[Key]]

  //FIXME sort has many more combinations
  final def sort[A](key: Key)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Seq[A]] = Protocol("SORT", key).as[NonNilArray, Seq[A]]

  final def sort[A](key: Key, pattern: GlobPattern)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Seq[A]] = Protocol("SORT", key :: "BY" :: pattern :: HNil).as[NonNilArray, Seq[A]]

  final def sort[A](key: Key, offset: NonNegLong, count: PosLong)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Seq[A]] = Protocol("SORT", key :: "LIMIT" :: offset :: count :: HNil).as[NonNilArray, Seq[A]]

  final def sort[A](key: Key, direction: Direction)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Seq[A]] = Protocol("SORT", key :: direction :: HNil).as[NonNilArray, Seq[A]]

  final def sortstore(key: Key, destination: Key): Protocol.Aux[NonNegInt] =
    Protocol("SORT", key.value :: "STORE" :: destination.value :: Nil).as[Integer, NonNegInt]

  final def touch(keys: OneOrMoreKeys): Protocol.Aux[NonNegInt] = Protocol("TOUCH", keys.value).as[Integer, NonNegInt]

  final def ttl(key: Key): Protocol.Aux[TTLResponse] = Protocol("TTL", key).as[Integer, TTLResponse]

  final def typeof(key: Key): Protocol.Aux[Option[Type]] = Protocol("TYPE", key).as[SimpleString, Option[Type]]

  final def unlink(keys: OneOrMoreKeys): Protocol.Aux[NonNegInt] = Protocol("UNLINK", keys.value).as[Integer, NonNegInt]

  final def waitFor(numSlaves: PosInt): Protocol.Aux[PosInt] =
    Protocol("WAIT", numSlaves :: 0 :: HNil).as[Integer, PosInt]

  final def waitFor(numSlaves: PosInt, timeout: PosLong): Protocol.Aux[PosInt] =
    Protocol("WAIT", numSlaves :: timeout :: HNil).as[Integer, PosInt]
}

trait AllKeyP extends KeyP with KeyPExtra
