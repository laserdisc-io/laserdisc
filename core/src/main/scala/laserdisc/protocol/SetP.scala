package laserdisc
package protocol

trait SetP {
  import shapeless._

  private[this] final val zeroIsNone = RESPRead.instance(Read.numZeroIsNone[PosInt])

  final def sadd[A: Show](key: Key, members: OneOrMore[A]): Protocol.Aux[NonNegInt] =
    Protocol("SADD", key :: members.value :: HNil).as[Num, NonNegInt]

  final def scard(key: Key): Protocol.Aux[Option[PosInt]] = Protocol("SCARD", key).using(zeroIsNone)

  final def sdiff[A](keys: TwoOrMoreKeys)(
      implicit ev0: Arr ==> Seq[A]
  ): Protocol.Aux[Seq[A]] = Protocol("SDIFF", keys.value).as[Arr, Seq[A]]

  final def sdiffstore(keys: TwoOrMoreKeys, destinationKey: Key): Protocol.Aux[NonNegInt] =
    Protocol("SDIFFSTORE", destinationKey :: keys.value).as[Num, NonNegInt]

  final def sinter[A](keys: TwoOrMoreKeys)(
      implicit ev0: Arr ==> Seq[A]
  ): Protocol.Aux[Seq[A]] = Protocol("SINTER", keys.value).as[Arr, Seq[A]]

  final def sinterstore(keys: TwoOrMoreKeys, destinationKey: Key): Protocol.Aux[NonNegInt] =
    Protocol("SINTERSTORE", destinationKey :: keys.value).as[Num, NonNegInt]

  final def sismember[A: Show](key: Key, member: A): Protocol.Aux[Boolean] =
    Protocol("SISMEMBER", key :: member :: HNil).as[Num, Boolean]

  final def smembers[A](key: Key)(
      implicit ev: Bulk ==> A
  ): Protocol.Aux[Seq[A]] = Protocol("SMEMBERS", key).as[Arr, Seq[A]]

  final def smove[A: Show](source: Key, destination: Key, member: A): Protocol.Aux[Boolean] =
    Protocol("SMOVE", source :: destination :: member :: HNil).as[Num, Boolean]

  final def spop[A](key: Key)(
      implicit ev: Bulk ==> A
  ): Protocol.Aux[Option[A]] = Protocol("SPOP", key).asC[NullBulk :+: Bulk :+: CNil, Option[A]]

  final def spop[A](key: Key, count: PosInt)(
      implicit ev: Bulk ==> A
  ): Protocol.Aux[Seq[A]] = Protocol("SPOP", key :: count :: HNil).as[Arr, Seq[A]]

  final def srandmember[A](key: Key)(
      implicit ev: Bulk ==> A
  ): Protocol.Aux[Option[A]] =
    Protocol("SRANDMEMBER", key).asC[NullBulk :+: Bulk :+: CNil, Option[A]]

  final def srandmembers[A](key: Key, count: NonZeroInt)(
      implicit ev: Bulk ==> A
  ): Protocol.Aux[Seq[A]] = Protocol("SRANDMEMBER", key :: count :: HNil).as[Arr, Seq[A]]

  final def srem[A: Show](key: Key, members: OneOrMore[A]): Protocol.Aux[NonNegInt] =
    Protocol("SREM", key :: members.value :: HNil).as[Num, NonNegInt]

  final def sscan[A](key: Key, cursor: NonNegLong)(
      implicit ev: Arr ==> Seq[A]
  ): Protocol.Aux[Scan[A]] = Protocol("SSCAN", key :: cursor :: HNil).as[Arr, Scan[A]]

  final def sscan[A](key: Key, cursor: NonNegLong, pattern: GlobPattern)(
      implicit ev: Arr ==> Seq[A]
  ): Protocol.Aux[Scan[A]] = Protocol("SSCAN", key :: cursor :: "MATCH" :: pattern :: HNil).as[Arr, Scan[A]]

  final def sscan[A](key: Key, cursor: NonNegLong, count: PosInt)(
      implicit ev: Arr ==> Seq[A]
  ): Protocol.Aux[Scan[A]] = Protocol("SSCAN", key :: cursor :: "COUNT" :: count :: HNil).as[Arr, Scan[A]]

  final def sscan[A](key: Key, cursor: NonNegLong, pattern: GlobPattern, count: PosInt)(
      implicit ev: Arr ==> Seq[A]
  ): Protocol.Aux[Scan[A]] =
    Protocol("SSCAN", key :: cursor :: "MATCH" :: pattern :: "COUNT" :: count :: HNil).as[Arr, Scan[A]]

  final def sunion[A](keys: TwoOrMoreKeys)(
      implicit ev0: Arr ==> Seq[A]
  ): Protocol.Aux[Seq[A]] = Protocol("SUNION", keys.value).as[Arr, Seq[A]]

  final def sunionstore(keys: TwoOrMoreKeys, destinationKey: Key): Protocol.Aux[NonNegInt] =
    Protocol("SUNIONSTORE", destinationKey :: keys.value).as[Num, NonNegInt]
}

trait AllSetP extends SetP with SetPExtra
