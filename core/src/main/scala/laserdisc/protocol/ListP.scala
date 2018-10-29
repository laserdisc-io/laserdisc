package laserdisc
package protocol

object ListP {
  sealed trait Position
  final object Position {
    final object before extends Position
    final object after  extends Position

    implicit val positionShow: Show[Position] = Show.instance {
      case `before` => "BEFORE"
      case `after`  => "AFTER"
    }
  }
}

trait ListP {
  import ListP.Position
  import Read.==>
  import shapeless._

  private[this] final val minusOneIsNone = RESPRead.instance(Read.integerMinusOneIsNone[PosInt])
  private[this] final val zeroIsNone     = RESPRead.instance(Read.integerZeroIsNone[PosInt])

  final object lists {
    final val position = Position
  }

  final def lindex[A](key: Key, index: Index)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Option[A]] =
    Protocol("LINDEX", key :: index :: HNil).asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[A]]

  final def linsert[A: Show](key: Key, position: Position, pivot: A, value: A): Protocol.Aux[Option[PosInt]] =
    Protocol("LINSERT", key :: position :: pivot :: value :: HNil).using(minusOneIsNone)

  final def llen(key: Key): Protocol.Aux[NonNegInt] = Protocol("LLEN", key).as[Integer, NonNegInt]

  final def lpop[A](key: Key)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Option[A]] = Protocol("LPOP", key).asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[A]]

  final def lpush[A: Show](key: Key, values: OneOrMore[A]): Protocol.Aux[PosInt] =
    Protocol("LPUSH", key :: values.value :: HNil).as[Integer, PosInt]

  final def lpushx[A: Show](key: Key, value: A): Protocol.Aux[Option[PosInt]] =
    Protocol("LPUSHX", key :: value :: HNil).using(zeroIsNone)

  final def lrange[A](key: Key, start: Index, end: Index)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Seq[A]] = Protocol("LRANGE", key :: start :: end :: HNil).as[NonNilArray, Seq[A]]

  final def lrem[A: Show](key: Key, count: Index, value: A): Protocol.Aux[NonNegInt] =
    Protocol("LREM", key :: count :: value :: HNil).as[Integer, NonNegInt]

  final def lset[A: Show](key: Key, index: Index, value: A): Protocol.Aux[OK] =
    Protocol("LSET", key :: index :: value :: HNil).as[SimpleString, OK]

  final def ltrim(key: Key, start: Index, stop: Index): Protocol.Aux[OK] =
    Protocol("LTRIM", key :: start :: stop :: HNil).as[SimpleString, OK]

  final def rpop[A](key: Key)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Option[A]] = Protocol("RPOP", key).asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[A]]

  final def rpoplpush[A](source: Key, destination: Key)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Option[A]] =
    Protocol("RPOPLPUSH", source :: destination :: Nil).asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[A]]

  final def rpush[A: Show](key: Key, values: OneOrMore[A]): Protocol.Aux[PosInt] =
    Protocol("RPUSH", key :: values.value :: HNil).as[Integer, PosInt]

  final def rpushx[A: Show](key: Key, value: A): Protocol.Aux[Option[PosInt]] =
    Protocol("RPUSHX", key :: value :: HNil).using(zeroIsNone)
}

trait AllListP extends ListP with ListPExtra
