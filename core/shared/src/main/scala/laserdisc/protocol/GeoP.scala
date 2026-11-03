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

object GeoP {
  final case class Coordinates(latitude: Latitude, longitude: Longitude)
  object Coordinates {
    implicit final val coordinatesRead: Read[Arr, Coordinates] = Read.instance {
      case Arr(Bulk(ToDouble(Longitude(long))) +: Bulk(ToDouble(Latitude(lat))) +: Seq()) => Right(Coordinates(lat, long))
      case Arr(other) => Left(RESPDecErr(s"Unexpected coordinates encoding. Expected [longitude, latitude] but was $other"))
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

  sealed trait RadiusMode { type Res; def params: List[String]; def r: Read[Arr, Res] }
  object RadiusMode       {
    object coordinates extends RadiusMode {
      override final type Res = KeyAndCoordinates
      override final val params: List[String] = List("WITHCOORD")
      override final val r: Read[Arr, Res]    = radiusModeCoordinatesRead

      def &(d: distance.type): coordinatesAndDistance.type = { val _ = d; coordinatesAndDistance }
      def &(h: hash.type): coordinatesAndHash.type         = { val _ = h; coordinatesAndHash }
    }
    object distance extends RadiusMode {
      override final type Res = KeyAndDistance
      override final val params: List[String] = List("WITHDIST")
      override final val r: Read[Arr, Res]    = radiusModeDistanceRead

      def &(c: coordinates.type): coordinatesAndDistance.type = { val _ = c; coordinatesAndDistance }
      def &(h: hash.type): distanceAndHash.type               = { val _ = h; distanceAndHash }
    }
    object hash extends RadiusMode {
      override final type Res = KeyAndHash
      override final val params: List[String] = List("WITHHASH")
      override final val r: Read[Arr, Res]    = radiusModeHashRead

      def &(c: coordinates.type): coordinatesAndHash.type = { val _ = c; coordinatesAndHash }
      def &(c: distance.type): distanceAndHash.type       = { val _ = c; distanceAndHash }
    }
    object coordinatesAndDistance extends RadiusMode {
      override final type Res = KeyCoordinatesAndDistance
      override final val params: List[String] = List("WITHCOORD", "WITHDIST")
      override final val r: Read[Arr, Res]    = radiusModeCoordinatesAndDistanceRead

      def &(h: hash.type): all.type = { val _ = h; all }
    }
    object coordinatesAndHash extends RadiusMode {
      override final type Res = KeyCoordinatesAndHash
      override final val params: List[String] = List("WITHCOORD", "WITHHASH")
      override final val r: Read[Arr, Res]    = radiusModeCoordinatesAndHashRead

      def &(d: distance.type): all.type = { val _ = d; all }
    }
    object distanceAndHash extends RadiusMode {
      override final type Res = KeyDistanceAndHash
      override final val params: List[String] = List("WITHDIST", "WITHHASH")
      override final val r: Read[Arr, Res]    = radiusModeDistanceAndHashRead

      def &(c: coordinates.type): all.type = { val _ = c; all }
    }
    object all extends RadiusMode {
      override final type Res = KeyCoordinatesDistanceAndHash
      override final val params: List[String] = List("WITHCOORD", "WITHDIST", "WITHHASH")
      override final val r: Read[Arr, Res]    = radiusModeAllRead
    }
  }

  sealed trait StoreMode { def params: List[String] }
  object StoreMode       {
    final case class Hash(key: Key)     extends StoreMode { override final val params: List[String] = List("STORE", key.value)     }
    final case class Distance(key: Key) extends StoreMode { override final val params: List[String] = List("STOREDIST", key.value) }
    final case class Both(hashKey: Key, distanceKey: Key) extends StoreMode {
      override final val params: List[String] = List("STORE", hashKey.value, "STOREDIST", distanceKey.value)
    }
  }

  /** NX and XX, for GEOADD */
  sealed trait Flag
  object Flag {
    object nx extends Flag
    object xx extends Flag

    implicit val flagShow: Show[Flag] = Show.instance {
      case `nx` => "NX"
      case `xx` => "XX"
    }
  }

  /** FROMMEMBER and FROMLONLAT, for GEOSEARCH and GEOSEARCHSTORE */
  sealed trait Center { def params: List[String] }
  object Center       {
    private final class Of(override final val params: List[String]) extends Center

    def member(member: Key): Center                   = new Of(List("FROMMEMBER", member.value))
    def coordinates(coordinates: Coordinates): Center =
      new Of(List("FROMLONLAT", Show[Longitude].show(coordinates.longitude), Show[Latitude].show(coordinates.latitude)))
  }

  /** BYRADIUS and BYBOX around a center, for GEOSEARCH and GEOSEARCHSTORE */
  sealed trait Area { def params: List[String] }
  object Area       {
    private final class Of(override final val params: List[String]) extends Area

    def radius(center: Center, radius: NonNegDouble, unit: Unit): Area =
      new Of(center.params ++ List("BYRADIUS", Show[NonNegDouble].show(radius), Show[Unit].show(unit)))
    def box(center: Center, width: NonNegDouble, height: NonNegDouble, unit: Unit): Area =
      new Of(center.params ++ List("BYBOX", Show[NonNegDouble].show(width), Show[NonNegDouble].show(height), Show[Unit].show(unit)))
  }

  /** BYPOLYGON, for GEOSEARCH and GEOSEARCHSTORE: Valkey 9.0 */
  final class Polygon private (val params: List[String])
  object Polygon {
    def apply(first: Coordinates, second: Coordinates, third: Coordinates, more: Coordinates*): Polygon = {
      val vertices = first :: second :: third :: more.toList
      new Polygon(
        "BYPOLYGON" :: vertices.length.toString :: vertices.flatMap(c =>
          List(Show[Longitude].show(c.longitude), Show[Latitude].show(c.latitude))
        )
      )
    }
  }

  /** COUNT, with or without ANY, for GEOSEARCH and GEOSEARCHSTORE */
  sealed trait Count { def params: List[String] }
  object Count       {
    private final class Of(override final val params: List[String]) extends Count

    /** The closest `count` matches */
    def closest(count: PosInt): Count = new Of(List("COUNT", Show[PosInt].show(count)))

    /** The first `count` matches found, which may not be the closest */
    def any(count: PosInt): Count = new Of(List("COUNT", Show[PosInt].show(count), "ANY"))
  }

  /** STOREDIST, for GEOSEARCHSTORE: stores the distances instead of the geohashes */
  sealed trait StoreDistances
  object StoreDistances extends StoreDistances {
    implicit val storeDistancesShow: Show[StoreDistances] = Show.const("STOREDIST")
  }

  sealed trait Unit
  object Unit {
    case object meters     extends Unit
    case object kilometers extends Unit
    case object miles      extends Unit
    case object feet       extends Unit

    implicit val unitShow: Show[Unit] = Show.instance {
      case `meters`     => "m"
      case `kilometers` => "km"
      case `miles`      => "mi"
      case `feet`       => "ft"
    }
  }

  implicit final val radiusModeCoordinatesRead: Read[Arr, RadiusMode.coordinates.Res] = Read.instance {
    case Arr(Bulk(Key(k)) +: Arr(Bulk(ToDouble(Longitude(long))) +: Bulk(ToDouble(Latitude(lat))) +: Seq()) +: Seq()) =>
      Right(KeyAndCoordinates(k, Coordinates(lat, long)))
    case Arr(other) => Left(RESPDecErr(s"Unexpected radius mode encoding. Expected [key, [longitude, latitude]] but was $other"))
  }
  implicit final val radiusModeDistanceRead: Read[Arr, RadiusMode.distance.Res] = Read.instance {
    case Arr(Bulk(Key(k)) +: Bulk(ToDouble(NonNegDouble(d))) +: Seq()) => Right(KeyAndDistance(k, d))
    case Arr(other) => Left(RESPDecErr(s"Unexpected radius mode encoding. Expected [key, distance] but was $other"))
  }
  implicit final val radiusModeHashRead: Read[Arr, RadiusMode.hash.Res] = Read.instance {
    case Arr(Bulk(Key(k)) +: Num(NonNegLong(l)) +: Seq()) => Right(KeyAndHash(k, l))
    case Arr(other) => Left(RESPDecErr(s"Unexpected radius mode encoding. Expected [key, hash] but was $other"))
  }
  implicit final val radiusModeCoordinatesAndDistanceRead: Read[Arr, RadiusMode.coordinatesAndDistance.Res] = Read.instance {
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
  implicit final val radiusModeCoordinatesAndHashRead: Read[Arr, RadiusMode.coordinatesAndHash.Res] = Read.instance {
    case Arr(
          Bulk(Key(k)) +: Num(NonNegLong(l)) +:
          Arr(Bulk(ToDouble(Longitude(long))) +: Bulk(ToDouble(Latitude(lat))) +: Seq()) +: Seq()
        ) =>
      Right(KeyCoordinatesAndHash(k, Coordinates(lat, long), l))
    case Arr(other) =>
      Left(RESPDecErr(s"Unexpected encoding for key coordinates and hash. Expected [key, hash, [longitude, latitude]] but was $other"))
  }
  implicit final val radiusModeDistanceAndHashRead: Read[Arr, RadiusMode.distanceAndHash.Res] = Read.instance {
    case Arr(Bulk(Key(k)) +: Bulk(ToDouble(NonNegDouble(d))) +: Num(NonNegLong(l)) +: Seq()) => Right(KeyDistanceAndHash(k, d, l))
    case Arr(other) => Left(RESPDecErr(s"Unexpected encoding for key coordinates and hash. Expected [key, distance, hash] but was $other"))
  }
  implicit final val radiusModeAllRead: Read[Arr, RadiusMode.all.Res] = Read.instance {
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

trait GeoBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "GEOADD: CH NX XX",
      "GEODIST: FT KM M MI",
      "GEOHASH",
      "GEOPOS",
      "GEORADIUS: ASC COUNT DESC FT KM M MI STORE STOREDIST WITHCOORD WITHDIST WITHHASH",
      "GEORADIUSBYMEMBER: ASC COUNT DESC FT KM M MI STORE STOREDIST WITHCOORD WITHDIST WITHHASH",
      "GEORADIUSBYMEMBER_RO: ASC COUNT DESC FT KM M MI WITHCOORD WITHDIST WITHHASH",
      "GEORADIUS_RO: ASC COUNT DESC FT KM M MI WITHCOORD WITHDIST WITHHASH",
      "GEOSEARCH: ANY ASC BYBOX BYRADIUS COUNT DESC FROMLONLAT FROMMEMBER FT KM M MI WITHCOORD WITHDIST WITHHASH",
      "GEOSEARCHSTORE: ANY ASC BYBOX BYRADIUS COUNT DESC FROMLONLAT FROMMEMBER FT KM M MI STOREDIST"
    )

  object geotypes {
    final type GeoArea                = GeoP.Area
    final type GeoCenter              = GeoP.Center
    final type GeoCount               = GeoP.Count
    final type GeoFlag                = GeoP.Flag
    final type GeoPolygon             = GeoP.Polygon
    final type GeoStoreDistances      = GeoP.StoreDistances
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

    final val GeoArea                = GeoP.Area
    final val GeoCenter              = GeoP.Center
    final val GeoCount               = GeoP.Count
    final val GeoFlag                = GeoP.Flag
    final val GeoPolygon             = GeoP.Polygon
    final val GeoStoreDistances      = GeoP.StoreDistances
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

  import geotypes.*

  private final def lonLatMember(positions: OneOrMore[GeoPosition]) =
    positions.value.map { case GeoPosition(m, lat, long) => (long -> lat) -> m }

  final def geoadd(key: Key, positions: OneOrMore[GeoPosition]): Single.Aux[NonNegInt] =
    Protocol("GEOADD", key *: lonLatMember(positions) *: EmptyTuple).as[Num, NonNegInt]
  final def geoadd(key: Key, flag: GeoFlag, positions: OneOrMore[GeoPosition]): Single.Aux[NonNegInt] =
    Protocol("GEOADD", key *: flag *: lonLatMember(positions) *: EmptyTuple).as[Num, NonNegInt]

  /** GEOADD with CH, which replies with the number of positions added or changed */
  final def geoaddch(key: Key, positions: OneOrMore[GeoPosition]): Single.Aux[NonNegInt] =
    Protocol("GEOADD", key *: "CH" *: lonLatMember(positions) *: EmptyTuple).as[Num, NonNegInt]
  final def geoaddch(key: Key, flag: GeoFlag, positions: OneOrMore[GeoPosition]): Single.Aux[NonNegInt] =
    Protocol("GEOADD", key *: flag *: "CH" *: lonLatMember(positions) *: EmptyTuple).as[Num, NonNegInt]

  final def geodist(key: Key, member1: Key, member2: Key): Single.Aux[Option[NonNegDouble]] =
    Protocol("GEODIST", key *: member1 *: member2 *: EmptyTuple).as[Bulk, Option[NonNegDouble]]
  final def geodist(key: Key, member1: Key, member2: Key, unit: GeoUnit): Single.Aux[Option[NonNegDouble]] =
    Protocol("GEODIST", key *: member1 *: member2 *: unit *: EmptyTuple).as[Bulk, Option[NonNegDouble]]

  final def geohash(key: Key, members: OneOrMoreKeys): Single.Aux[Seq[Option[GeoHash]]] =
    Protocol("GEOHASH", key :: members.value).as[Arr.Of[Option[Bulk]], Seq[Option[GeoHash]]]

  final def geopos(key: Key, members: OneOrMoreKeys): Single.Aux[Seq[Option[GeoCoordinates]]] =
    Protocol("GEOPOS", key :: members.value).as[Arr.Of[Option[Arr]], Seq[Option[GeoCoordinates]]]

  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit): Single.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Single.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: EmptyTuple)
      .as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, sort: Direction): Single.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: sort *: EmptyTuple)
      .as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      sort: Direction
  ): Single.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: sort *: EmptyTuple)
      .as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: mode.params *: EmptyTuple)
      .as[Arr.Of[Arr], Seq[mode.Res]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: mode.params *: EmptyTuple
    )
      .as[Arr.Of[Arr], Seq[mode.Res]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: sort *: mode.params *: EmptyTuple)
      .as[Arr.Of[Arr], Seq[mode.Res]]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      sort: Direction,
      mode: GeoRadiusMode
  )(implicit ev: Read[Arr, mode.Res]): Single.Aux[Seq[mode.Res]] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: sort *: mode.params *: EmptyTuple
    ).as[Arr.Of[Arr], Seq[mode.Res]]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      store: GeoStoreMode
  ): Single.Aux[NonNegInt] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: store.params *: EmptyTuple)
      .as[Num, NonNegInt]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      store: GeoStoreMode
  ): Single.Aux[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: store.params *: EmptyTuple
    ).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      sort: Direction,
      store: GeoStoreMode
  ): Single.Aux[NonNegInt] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: sort *: store.params *: EmptyTuple)
      .as[Num, NonNegInt]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      sort: Direction,
      store: GeoStoreMode
  ): Single.Aux[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: sort *: store.params *: EmptyTuple
    ).as[Num, NonNegInt]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit): Single.Aux[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Single.Aux[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction): Single.Aux[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, sort: Direction): Single.Aux[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: mode.params *: EmptyTuple)
      .as[Arr.Of[Arr], Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: sort *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: sort *: mode.params *: EmptyTuple)
      .as[Arr.Of[Arr], Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, store: GeoStoreMode): Single.Aux[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: store.params *: EmptyTuple).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      member: Key,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      store: GeoStoreMode
  ): Single.Aux[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: store.params *: EmptyTuple).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      member: Key,
      radius: NonNegDouble,
      unit: GeoUnit,
      sort: Direction,
      store: GeoStoreMode
  ): Single.Aux[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: sort *: store.params *: EmptyTuple).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      member: Key,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      sort: Direction,
      store: GeoStoreMode
  ): Single.Aux[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: sort *: store.params *: EmptyTuple)
      .as[Num, NonNegInt]

  final def geosearch(key: Key, area: GeoArea): Single.Aux[Seq[Key]] =
    Protocol("GEOSEARCH", key *: area.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, area: GeoArea, sort: Direction): Single.Aux[Seq[Key]] =
    Protocol("GEOSEARCH", key *: area.params *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, area: GeoArea, count: GeoCount): Single.Aux[Seq[Key]] =
    Protocol("GEOSEARCH", key *: area.params *: count.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, area: GeoArea, sort: Direction, count: GeoCount): Single.Aux[Seq[Key]] =
    Protocol("GEOSEARCH", key *: area.params *: sort *: count.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, area: GeoArea, mode: GeoRadiusMode)(implicit ev: Read[Arr, mode.Res]): Single.Aux[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: area.params *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]
  final def geosearch(key: Key, area: GeoArea, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: area.params *: sort *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]
  final def geosearch(key: Key, area: GeoArea, count: GeoCount, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: area.params *: count.params *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]
  final def geosearch(key: Key, area: GeoArea, sort: Direction, count: GeoCount, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: area.params *: sort *: count.params *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]

  final def geosearchstore(source: Key, destination: Key, area: GeoArea): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, area: GeoArea, sort: Direction): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: sort *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, area: GeoArea, count: GeoCount): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: count.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, area: GeoArea, sort: Direction, count: GeoCount): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: sort *: count.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, area: GeoArea, distances: GeoStoreDistances): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      area: GeoArea,
      sort: Direction,
      distances: GeoStoreDistances
  ): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: sort *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      area: GeoArea,
      count: GeoCount,
      distances: GeoStoreDistances
  ): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: count.params *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      area: GeoArea,
      sort: Direction,
      count: GeoCount,
      distances: GeoStoreDistances
  ): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: sort *: count.params *: distances *: EmptyTuple)
      .as[Num, NonNegInt]

  object ro {
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit): Single.Aux[Seq[Key]] =
      Protocol("GEORADIUS_RO", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: EmptyTuple)
        .as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Single.Aux[Seq[Key]] =
      Protocol("GEORADIUS_RO", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: EmptyTuple)
        .as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(
        key: Key,
        coordinates: GeoCoordinates,
        radius: NonNegDouble,
        unit: GeoUnit,
        sort: Direction
    ): Single.Aux[Seq[Key]] =
      Protocol("GEORADIUS_RO", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: sort *: EmptyTuple)
        .as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(
        key: Key,
        coordinates: GeoCoordinates,
        radius: NonNegDouble,
        unit: GeoUnit,
        limit: PosInt,
        sort: Direction
    ): Single.Aux[Seq[Key]] =
      Protocol(
        "GEORADIUS_RO",
        key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: sort *: EmptyTuple
      )
        .as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
        implicit ev: Read[Arr, mode.Res]
    ): Single.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUS_RO", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: mode.params *: EmptyTuple)
        .as[Arr.Of[Arr], Seq[mode.Res]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
        implicit ev: Read[Arr, mode.Res]
    ): Single.Aux[Seq[mode.Res]] =
      Protocol(
        "GEORADIUS_RO",
        key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: mode.params *: EmptyTuple
      ).as[Arr.Of[Arr], Seq[mode.Res]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
        implicit ev: Read[Arr, mode.Res]
    ): Single.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUS_RO", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: sort *: mode.params *: EmptyTuple)
        .as[Arr.Of[Arr], Seq[mode.Res]]
    final def georadius(
        key: Key,
        coordinates: GeoCoordinates,
        radius: NonNegDouble,
        unit: GeoUnit,
        limit: PosInt,
        sort: Direction,
        mode: GeoRadiusMode
    )(implicit ev: Read[Arr, mode.Res]): Single.Aux[Seq[mode.Res]] =
      Protocol(
        "GEORADIUS_RO",
        key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: sort *: mode.params *: EmptyTuple
      ).as[Arr.Of[Arr], Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit): Single.Aux[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Single.Aux[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: "COUNT" *: limit *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction): Single.Aux[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(
        key: Key,
        member: Key,
        radius: NonNegDouble,
        unit: GeoUnit,
        limit: PosInt,
        sort: Direction
    ): Single.Aux[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: "COUNT" *: limit *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
        implicit ev: Read[Arr, mode.Res]
    ): Single.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
        implicit ev: Read[Arr, mode.Res]
    ): Single.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: "COUNT" *: limit *: mode.params *: EmptyTuple)
        .as[Arr.Of[Arr], Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
        implicit ev: Read[Arr, mode.Res]
    ): Single.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: sort *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, sort: Direction, mode: GeoRadiusMode)(
        implicit ev: Read[Arr, mode.Res]
    ): Single.Aux[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: "COUNT" *: limit *: sort *: mode.params *: EmptyTuple)
        .as[Arr.Of[Arr], Seq[mode.Res]]
  }
}

/** GEOSEARCH and GEOSEARCHSTORE with BYPOLYGON: Valkey 9.0 */
trait GeoSearchPolygonP extends Features { this: GeoBaseP =>
  override private[laserdisc] def supported: List[String] = super.supported ++ List("GEOSEARCH: BYPOLYGON", "GEOSEARCHSTORE: BYPOLYGON")

  import geotypes.*

  final def geosearch(key: Key, polygon: GeoPolygon): Single.Aux[Seq[Key]] =
    Protocol("GEOSEARCH", key *: polygon.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, polygon: GeoPolygon, sort: Direction): Single.Aux[Seq[Key]] =
    Protocol("GEOSEARCH", key *: polygon.params *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, polygon: GeoPolygon, count: GeoCount): Single.Aux[Seq[Key]] =
    Protocol("GEOSEARCH", key *: polygon.params *: count.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, polygon: GeoPolygon, sort: Direction, count: GeoCount): Single.Aux[Seq[Key]] =
    Protocol("GEOSEARCH", key *: polygon.params *: sort *: count.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, polygon: GeoPolygon, mode: GeoRadiusMode)(implicit ev: Read[Arr, mode.Res]): Single.Aux[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: polygon.params *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]
  final def geosearch(key: Key, polygon: GeoPolygon, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: polygon.params *: sort *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]
  final def geosearch(key: Key, polygon: GeoPolygon, count: GeoCount, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: polygon.params *: count.params *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]
  final def geosearch(key: Key, polygon: GeoPolygon, sort: Direction, count: GeoCount, mode: GeoRadiusMode)(
      implicit ev: Read[Arr, mode.Res]
  ): Single.Aux[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: polygon.params *: sort *: count.params *: mode.params *: EmptyTuple).as[Arr.Of[Arr], Seq[mode.Res]]

  final def geosearchstore(source: Key, destination: Key, polygon: GeoPolygon): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, polygon: GeoPolygon, sort: Direction): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: sort *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, polygon: GeoPolygon, count: GeoCount): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: count.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, polygon: GeoPolygon, sort: Direction, count: GeoCount): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: sort *: count.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, polygon: GeoPolygon, distances: GeoStoreDistances): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      polygon: GeoPolygon,
      sort: Direction,
      distances: GeoStoreDistances
  ): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: sort *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      polygon: GeoPolygon,
      count: GeoCount,
      distances: GeoStoreDistances
  ): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: count.params *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      polygon: GeoPolygon,
      sort: Direction,
      count: GeoCount,
      distances: GeoStoreDistances
  ): Single.Aux[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: sort *: count.params *: distances *: EmptyTuple)
      .as[Num, NonNegInt]
}

trait GeoP extends GeoBaseP with GeoExtP with GeoSearchPolygonP
