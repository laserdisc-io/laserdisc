package laserdisc
package protocol

object GeoP {
  final case class Coordinates(latitude: Latitude, longitude: Longitude)
  final object Coordinates {
    implicit final val coordinatesRead: Arr ==> Coordinates = Read.instance {
      case Arr(Bulk(ToDouble(Longitude(long))) +: Bulk(ToDouble(Latitude(lat))) +: Seq()) => Right(Coordinates(lat, long))
      case Arr(other)                                                                     => Left(RESPDecErr(s"Unexpected coordinates encoding. Expected [longitude, latitude] but was $other"))
    }
  }

  final case class Position(member: Key, latitude: Latitude, longitude: Longitude)

  final case class KeyAndCoordinates(key: Key, coordinates: Coordinates)
  final case class KeyAndDistance(key: Key, distance: NonNegDouble)
  final case class KeyAndHash(key: Key, hash: NonNegLong)
  final case class KeyCoordinatesAndDistance(key: Key, coordinates: Coordinates, distance: NonNegDouble)
  final case class KeyCoordinatesAndHash(key: Key, coordinates: Coordinates, hash: NonNegLong)
  final case class KeyDistanceAndHash(key: Key, distance: NonNegDouble, hash: NonNegLong)
  final case class KeyCoordinatesDistanceAndHash(key: Key, coordinates: Coordinates, distance: NonNegDouble, hash: NonNegLong)

  sealed trait RadiusMode { type Res; def params: List[String]; def r: Arr ==> Res }
  object RadiusMode {
    final object coordinates extends RadiusMode {
      override final type Res = KeyAndCoordinates
      override final val params: List[String] = List("WITHCOORD")
      override final val r: Arr ==> Res       = radiusModeCoordinatesRead

      def &(d: distance.type): coordinatesAndDistance.type = { val _ = d; coordinatesAndDistance }
      def &(h: hash.type): coordinatesAndHash.type         = { val _ = h; coordinatesAndHash }
    }
    final object distance extends RadiusMode {
      override final type Res = KeyAndDistance
      override final val params: List[String] = List("WITHDIST")
      override final val r: Arr ==> Res       = radiusModeDistanceRead

      def &(c: coordinates.type): coordinatesAndDistance.type = { val _ = c; coordinatesAndDistance }
      def &(h: hash.type): distanceAndHash.type               = { val _ = h; distanceAndHash }
    }
    final object hash extends RadiusMode {
      override final type Res = KeyAndHash
      override final val params: List[String] = List("WITHHASH")
      override final val r: Arr ==> Res       = radiusModeHashRead

      def &(c: coordinates.type): coordinatesAndHash.type = { val _ = c; coordinatesAndHash }
      def &(c: distance.type): distanceAndHash.type       = { val _ = c; distanceAndHash }
    }
    final object coordinatesAndDistance extends RadiusMode {
      override final type Res = KeyCoordinatesAndDistance
      override final val params: List[String] = List("WITHCOORD", "WITHDIST")
      override final val r: Arr ==> Res       = radiusModeCoordinatesAndDistanceRead

      def &(h: hash.type): all.type = { val _ = h; all }
    }
    final object coordinatesAndHash extends RadiusMode {
      override final type Res = KeyCoordinatesAndHash
      override final val params: List[String] = List("WITHCOORD", "WITHHASH")
      override final val r: Arr ==> Res       = radiusModeCoordinatesAndHashRead

      def &(d: distance.type): all.type = { val _ = d; all }
    }
    final object distanceAndHash extends RadiusMode {
      override final type Res = KeyDistanceAndHash
      override final val params: List[String] = List("WITHDIST", "WITHHASH")
      override final val r: Arr ==> Res       = radiusModeDistanceAndHashRead

      def &(c: coordinates.type): all.type = { val _ = c; all }
    }
    final object all extends RadiusMode {
      override final type Res = KeyCoordinatesDistanceAndHash
      override final val params: List[String] = List("WITHCOORD", "WITHDIST", "WITHHASH")
      override final val r: Arr ==> Res       = radiusModeAllRead
    }
  }

  sealed trait StoreMode { def params: List[String] }
  object StoreMode {
    final case class Hash(key: Key)     extends StoreMode { override final val params: List[String] = List("STORE", key.value)     }
    final case class Distance(key: Key) extends StoreMode { override final val params: List[String] = List("STOREDIST", key.value) }
    final case class Both(hashKey: Key, distanceKey: Key) extends StoreMode {
      override final val params: List[String] = List("STORE", hashKey.value, "STOREDIST", distanceKey.value)
    }
  }

  sealed trait Unit
  final object Unit {
    final case object meters     extends Unit
    final case object kilometers extends Unit
    final case object miles      extends Unit
    final case object feet       extends Unit

    implicit val unitShow: Show[Unit] = Show.instance {
      case `meters`     => "m"
      case `kilometers` => "km"
      case `miles`      => "mi"
      case `feet`       => "ft"
    }
  }

  implicit final val radiusModeCoordinatesRead: Arr ==> RadiusMode.coordinates.Res = Read.instance {
    case Arr(Bulk(Key(k)) +: Arr(Bulk(ToDouble(Longitude(long))) +: Bulk(ToDouble(Latitude(lat))) +: Seq()) +: Seq()) =>
      Right(KeyAndCoordinates(k, Coordinates(lat, long)))
    case Arr(other) => Left(RESPDecErr(s"Unexpected radius mode encoding. Expected [key, [longitude, latitude]] but was $other"))
  }
  implicit final val radiusModeDistanceRead: Arr ==> RadiusMode.distance.Res = Read.instance {
    case Arr(Bulk(Key(k)) +: Bulk(ToDouble(NonNegDouble(d))) +: Seq()) => Right(KeyAndDistance(k, d))
    case Arr(other)                                                    => Left(RESPDecErr(s"Unexpected radius mode encoding. Expected [key, distance] but was $other"))
  }
  implicit final val radiusModeHashRead: Arr ==> RadiusMode.hash.Res = Read.instance {
    case Arr(Bulk(Key(k)) +: Num(NonNegLong(l)) +: Seq()) => Right(KeyAndHash(k, l))
    case Arr(other)                                       => Left(RESPDecErr(s"Unexpected radius mode encoding. Expected [key, hash] but was $other"))
  }
  implicit final val radiusModeCoordinatesAndDistanceRead: Arr ==> RadiusMode.coordinatesAndDistance.Res = Read.instance {
    case Arr(
        Bulk(Key(k)) +: Bulk(ToDouble(NonNegDouble(d))) +:
          Arr(Bulk(ToDouble(Longitude(long))) +: Bulk(ToDouble(Latitude(lat))) +: Seq()) +: Seq()
        ) =>
      Right(KeyCoordinatesAndDistance(k, Coordinates(lat, long), d))
    case Arr(other) =>
      Left(
        RESPDecErr(s"Unexpected encoding for key coordinates and distance. Expected [key, distance, [longitude, latitude]] but was $other")
      )
  }
  implicit final val radiusModeCoordinatesAndHashRead: Arr ==> RadiusMode.coordinatesAndHash.Res = Read.instance {
    case Arr(
        Bulk(Key(k)) +: Num(NonNegLong(l)) +:
          Arr(Bulk(ToDouble(Longitude(long))) +: Bulk(ToDouble(Latitude(lat))) +: Seq()) +: Seq()
        ) =>
      Right(KeyCoordinatesAndHash(k, Coordinates(lat, long), l))
    case Arr(other) =>
      Left(RESPDecErr(s"Unexpected encoding for key coordinates and hash. Expected [key, hash, [longitude, latitude]] but was $other"))
  }
  implicit final val radiusModeDistanceAndHashRead: Arr ==> RadiusMode.distanceAndHash.Res = Read.instance {
    case Arr(Bulk(Key(k)) +: Bulk(ToDouble(NonNegDouble(d))) +: Num(NonNegLong(l)) +: Seq()) => Right(KeyDistanceAndHash(k, d, l))
    case Arr(other)                                                                          => Left(RESPDecErr(s"Unexpected encoding for key coordinates and hash. Expected [key, distance, hash] but was $other"))
  }
  implicit final val radiusModeAllRead: Arr ==> RadiusMode.all.Res = Read.instance {
    case Arr(
        Bulk(Key(k)) +: Bulk(ToDouble(NonNegDouble(d))) +: Num(NonNegLong(l)) +:
          Arr(Bulk(ToDouble(Longitude(long))) +: Bulk(ToDouble(Latitude(lat))) +: Seq()) +: Seq()
        ) =>
      Right(KeyCoordinatesDistanceAndHash(k, Coordinates(lat, long), d, l))
    case Arr(other) =>
      Left(
        RESPDecErr(
          s"Unexpected encoding for key coordinates and hash. Expected [key, distance, hash, [longitude, latitude]] but was $other"
        )
      )
  }
}

trait GeoBaseP {
  import shapeless._

  final object geotypes {
    final type GeoCoordinates         = GeoP.Coordinates
    final type GeoKeyAndCoord         = GeoP.KeyAndCoordinates
    final type GeoKeyAndDist          = GeoP.KeyAndDistance
    final type GeoKeyAndHash          = GeoP.KeyAndHash
    final type GeoKeyCoordAndDist     = GeoP.KeyCoordinatesAndDistance
    final type GeoKeyCoordAndHash     = GeoP.KeyCoordinatesAndHash
    final type GeoKeyDistAndHash      = GeoP.KeyDistanceAndHash
    final type GeoKeyCoordDistAndHash = GeoP.KeyCoordinatesDistanceAndHash
    final type GeoPosition            = GeoP.Position
    final type GeoRadiusMode          = GeoP.RadiusMode
    final type GeoStoreMode           = GeoP.StoreMode
    final type GeoUnit                = GeoP.Unit

    final val GeoCoordinates         = GeoP.Coordinates
    final val GeoKeyAndCoord         = GeoP.KeyAndCoordinates
    final val GeoKeyAndDist          = GeoP.KeyAndDistance
    final val GeoKeyAndHash          = GeoP.KeyAndHash
    final val GeoKeyCoordAndDist     = GeoP.KeyCoordinatesAndDistance
    final val GeoKeyCoordAndHash     = GeoP.KeyCoordinatesAndHash
    final val GeoKeyDistAndHash      = GeoP.KeyDistanceAndHash
    final val GeoKeyCoordDistAndHash = GeoP.KeyCoordinatesDistanceAndHash
    final val GeoPosition            = GeoP.Position
    final val GeoRadiusMode          = GeoP.RadiusMode
    final val GeoStoreBoth           = GeoP.StoreMode.Both
    final val GeoStoreDistance       = GeoP.StoreMode.Distance
    final val GeoStoreHash           = GeoP.StoreMode.Hash
    final val GeoUnit                = GeoP.Unit
  }

  import geotypes._

  final def geoadd(key: Key, positions: OneOrMore[GeoPosition]): Protocol.Aux[NonNegInt] =
    Protocol("GEOADD", key :: positions.value.map { case GeoPosition(m, lat, long) => (long -> lat) -> m } :: HNil).as[Num, NonNegInt]

  final def geodist(key: Key, member1: Key, member2: Key): Protocol.Aux[Option[NonNegDouble]] =
    Protocol("GEODIST", key :: member1 :: member2 :: HNil).opt[GenBulk].as[NonNegDouble]
  final def geodist(key: Key, member1: Key, member2: Key, unit: GeoUnit): Protocol.Aux[Option[NonNegDouble]] =
    Protocol("GEODIST", key :: member1 :: member2 :: unit :: HNil).opt[GenBulk].as[NonNegDouble]

  final def geohash(key: Key, members: OneOrMoreKeys): Protocol.Aux[Seq[Option[GeoHash]]] =
    Protocol("GEOHASH", key :: members.value).as[Arr, Seq[Option[GeoHash]]]

  final def geopos(key: Key, members: OneOrMoreKeys): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    Protocol("GEOPOS", key :: members.value).as[Arr, Seq[Option[GeoCoordinates]]]

  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: HNil).as[Arr, Seq[Key]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: HNil)
      .as[Arr, Seq[Key]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, sort: Direction): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: HNil).as[Arr, Seq[Key]]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      sort: Direction
  ): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: HNil)
      .as[Arr, Seq[Key]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
      implicit ev: Arr ==> mode.Res
  ): Protocol.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: mode.params :: HNil)
      .as[Arr, Seq[mode.Res]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
      implicit ev: Arr ==> mode.Res
  ): Protocol.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: mode.params :: HNil)
      .as[Arr, Seq[mode.Res]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: Arr ==> mode.Res
  ): Protocol.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: mode.params :: HNil)
      .as[Arr, Seq[mode.Res]]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      sort: Direction,
      mode: GeoRadiusMode
  )(implicit ev: Arr ==> mode.Res): Protocol.Aux[Seq[mode.Res]] =
    Protocol(
      "GEORADIUS",
      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: mode.params :: HNil
    ).as[Arr, Seq[mode.Res]]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      store: GeoStoreMode
  ): Protocol.Aux[NonNegInt] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: store.params :: HNil).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      store: GeoStoreMode
  ): Protocol.Aux[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: store.params :: HNil
    ).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      sort: Direction,
      store: GeoStoreMode
  ): Protocol.Aux[NonNegInt] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: store.params :: HNil)
      .as[Num, NonNegInt]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      sort: Direction,
      store: GeoStoreMode
  ): Protocol.Aux[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: store.params :: HNil
    ).as[Num, NonNegInt]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: HNil).as[Arr, Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: "COUNT" :: limit :: HNil).as[Arr, Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: sort :: HNil).as[Arr, Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, sort: Direction): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: "COUNT" :: limit :: sort :: HNil).as[Arr, Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
      implicit ev: Arr ==> mode.Res
  ): Protocol.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: mode.params :: HNil).as[Arr, Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
      implicit ev: Arr ==> mode.Res
  ): Protocol.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: "COUNT" :: limit :: mode.params :: HNil).as[Arr, Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: Arr ==> mode.Res
  ): Protocol.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: sort :: mode.params :: HNil).as[Arr, Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: Arr ==> mode.Res
  ): Protocol.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: "COUNT" :: limit :: sort :: mode.params :: HNil).as[Arr, Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, store: GeoStoreMode): Protocol.Aux[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: store.params :: HNil).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      member: Key,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      store: GeoStoreMode
  ): Protocol.Aux[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: "COUNT" :: limit :: store.params :: HNil).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      member: Key,
      radius: NonNegDouble,
      unit: GeoUnit,
      sort: Direction,
      store: GeoStoreMode
  ): Protocol.Aux[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: sort :: store.params :: HNil).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      member: Key,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      sort: Direction,
      store: GeoStoreMode
  ): Protocol.Aux[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key :: member :: radius :: unit :: "COUNT" :: limit :: sort :: store.params :: HNil).as[Num, NonNegInt]

  final object ro {
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit): Protocol.Aux[Seq[Key]] =
      Protocol("GEORADIUS_RO", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: HNil).as[Arr, Seq[Key]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Protocol.Aux[Seq[Key]] =
      Protocol("GEORADIUS_RO", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: HNil)
        .as[Arr, Seq[Key]]
    final def georadius(
        key: Key,
        coordinates: GeoCoordinates,
        radius: NonNegDouble,
        unit: GeoUnit,
        sort: Direction
    ): Protocol.Aux[Seq[Key]] =
      Protocol("GEORADIUS_RO", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: HNil).as[Arr, Seq[Key]]
    final def georadius(
        key: Key,
        coordinates: GeoCoordinates,
        radius: NonNegDouble,
        unit: GeoUnit,
        limit: PosInt,
        sort: Direction
    ): Protocol.Aux[Seq[Key]] =
      Protocol("GEORADIUS_RO", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: HNil)
        .as[Arr, Seq[Key]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
        implicit ev: Arr ==> mode.Res
    ): Protocol.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUS_RO", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: mode.params :: HNil)
        .as[Arr, Seq[mode.Res]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
        implicit ev: Arr ==> mode.Res
    ): Protocol.Aux[Seq[mode.Res]] =
      Protocol(
        "GEORADIUS_RO",
        key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: mode.params :: HNil
      ).as[Arr, Seq[mode.Res]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
        implicit ev: Arr ==> mode.Res
    ): Protocol.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUS_RO", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: mode.params :: HNil)
        .as[Arr, Seq[mode.Res]]
    final def georadius(
        key: Key,
        coordinates: GeoCoordinates,
        radius: NonNegDouble,
        unit: GeoUnit,
        limit: PosInt,
        sort: Direction,
        mode: GeoRadiusMode
    )(implicit ev: Arr ==> mode.Res): Protocol.Aux[Seq[mode.Res]] =
      Protocol(
        "GEORADIUS_RO",
        key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: mode.params :: HNil
      ).as[Arr, Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit): Protocol.Aux[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key :: member :: radius :: unit :: HNil).as[Arr, Seq[Key]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Protocol.Aux[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key :: member :: radius :: unit :: "COUNT" :: limit :: HNil).as[Arr, Seq[Key]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction): Protocol.Aux[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key :: member :: radius :: unit :: sort :: HNil).as[Arr, Seq[Key]]
    final def georadius(
        key: Key,
        member: Key,
        radius: NonNegDouble,
        unit: GeoUnit,
        limit: PosInt,
        sort: Direction
    ): Protocol.Aux[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key :: member :: radius :: unit :: "COUNT" :: limit :: sort :: HNil).as[Arr, Seq[Key]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
        implicit ev: Arr ==> mode.Res
    ): Protocol.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key :: member :: radius :: unit :: mode.params :: HNil).as[Arr, Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
        implicit ev: Arr ==> mode.Res
    ): Protocol.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key :: member :: radius :: unit :: "COUNT" :: limit :: mode.params :: HNil).as[Arr, Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
        implicit ev: Arr ==> mode.Res
    ): Protocol.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key :: member :: radius :: unit :: sort :: mode.params :: HNil).as[Arr, Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, sort: Direction, mode: GeoRadiusMode)(
        implicit ev: Arr ==> mode.Res
    ): Protocol.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key :: member :: radius :: unit :: "COUNT" :: limit :: sort :: mode.params :: HNil)
        .as[Arr, Seq[mode.Res]]
  }
}

trait GeoP extends GeoBaseP with GeoExtP
