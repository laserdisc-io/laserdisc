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

trait ListBaseP {
  import shapeless._

  final object listtypes {
    final type ListPosition = ListP.Position

    final val ListPosition = ListP.Position
  }

  import listtypes._

  private[this] final val minusOneIsNone = RESPRead.instance(Read.numMinusOneIsNone[PosInt])
  private[this] final val zeroIsNone     = RESPRead.instance(Read.numZeroIsNone[PosInt])

  final def lindex[A: Bulk ==> *](key: Key, index: Index): Protocol.Aux[Option[A]] =
    Protocol("LINDEX", key :: index :: HNil).opt[GenBulk].as[A]

  final def linsert[A: Show](key: Key, position: ListPosition, pivot: A, value: A): Protocol.Aux[Option[PosInt]] =
    Protocol("LINSERT", key :: position :: pivot :: value :: HNil).using(minusOneIsNone)

  final def llen(key: Key): Protocol.Aux[NonNegInt] = Protocol("LLEN", key).as[Num, NonNegInt]

  final def lpop[A: Bulk ==> *](key: Key): Protocol.Aux[Option[A]] = Protocol("LPOP", key).opt[GenBulk].as[A]

  final def lpush[A: Show](key: Key, values: OneOrMore[A]): Protocol.Aux[PosInt] =
    Protocol("LPUSH", key :: values.value :: HNil).as[Num, PosInt]

  final def lpushx[A: Show](key: Key, value: A): Protocol.Aux[Option[PosInt]] = Protocol("LPUSHX", key :: value :: HNil).using(zeroIsNone)

  final def lrange[A: Bulk ==> *](key: Key, start: Index, end: Index): Protocol.Aux[Seq[A]] =
    Protocol("LRANGE", key :: start :: end :: HNil).as[Arr, Seq[A]]

  final def lrem[A: Show](key: Key, count: Index, value: A): Protocol.Aux[NonNegInt] =
    Protocol("LREM", key :: count :: value :: HNil).as[Num, NonNegInt]

  final def lset[A: Show](key: Key, index: Index, value: A): Protocol.Aux[OK] = Protocol("LSET", key :: index :: value :: HNil).as[Str, OK]

  final def ltrim(key: Key, start: Index, stop: Index): Protocol.Aux[OK] = Protocol("LTRIM", key :: start :: stop :: HNil).as[Str, OK]

  final def rpop[A: Bulk ==> *](key: Key): Protocol.Aux[Option[A]] = Protocol("RPOP", key).opt[GenBulk].as[A]

  final def rpoplpush[A: Bulk ==> *](source: Key, destination: Key): Protocol.Aux[Option[A]] =
    Protocol("RPOPLPUSH", source :: destination :: Nil).opt[GenBulk].as[A]

  final def rpush[A: Show](key: Key, values: OneOrMore[A]): Protocol.Aux[PosInt] =
    Protocol("RPUSH", key :: values.value :: HNil).as[Num, PosInt]

  final def rpushx[A: Show](key: Key, value: A): Protocol.Aux[Option[PosInt]] = Protocol("RPUSHX", key :: value :: HNil).using(zeroIsNone)
}

trait ListP extends ListBaseP with ListExtP
