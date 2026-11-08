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
    private implicit final val longitude: Read[Bulk, Longitude] = Read.instancePF("Bulk(Longitude)") { case Bulk(ToDouble(Longitude(l))) =>
      l
    }
    private implicit final val latitude: Read[Bulk, Latitude] = Read.instancePF("Bulk(Latitude)") { case Bulk(ToDouble(Latitude(l))) => l }

    /** Longitude first, as servers reply */
    implicit final val coordinatesRead: Read[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple], Coordinates] =
      Read[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple], Longitude *: Latitude *: EmptyTuple].map { case long *: lat *: _ =>
        Coordinates(lat, long)
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

  private[protocol] final type CoordinatesReply = Arr.AsTuple[Bulk *: Bulk *: EmptyTuple]

  /** What GEORADIUS and GEOSEARCH reply with for each member: `Reply` as it's sent, and `Res` as it's read */
  sealed trait RadiusMode {
    type Reply
    type Res
    def params: List[String]

    /** How the reply is read, for a mode only known when running. With one known when compiling, it's found on its own */
    def r: RESPRead.Aux[Arr.Of[Reply], Seq[Res]]
  }
  object RadiusMode {
    object coordinates extends RadiusMode {
      override final type Reply = Arr.AsTuple[Bulk *: CoordinatesReply *: EmptyTuple]
      override final type Res   = KeyAndCoordinates
      override final val params: List[String]                     = List("WITHCOORD")
      override final val r: RESPRead.Aux[Arr.Of[Reply], Seq[Res]] = RESPRead[Arr.Of[Reply], Seq[Res]]

      def &(d: distance.type): coordinatesAndDistance.type = { val _ = d; coordinatesAndDistance }
      def &(h: hash.type): coordinatesAndHash.type         = { val _ = h; coordinatesAndHash }
    }
    object distance extends RadiusMode {
      override final type Reply = Arr.AsTuple[Bulk *: Bulk *: EmptyTuple]
      override final type Res   = KeyAndDistance
      override final val params: List[String]                     = List("WITHDIST")
      override final val r: RESPRead.Aux[Arr.Of[Reply], Seq[Res]] = RESPRead[Arr.Of[Reply], Seq[Res]]

      def &(c: coordinates.type): coordinatesAndDistance.type = { val _ = c; coordinatesAndDistance }
      def &(h: hash.type): distanceAndHash.type               = { val _ = h; distanceAndHash }
    }
    object hash extends RadiusMode {
      override final type Reply = Arr.AsTuple[Bulk *: Num *: EmptyTuple]
      override final type Res   = KeyAndHash
      override final val params: List[String]                     = List("WITHHASH")
      override final val r: RESPRead.Aux[Arr.Of[Reply], Seq[Res]] = RESPRead[Arr.Of[Reply], Seq[Res]]

      def &(c: coordinates.type): coordinatesAndHash.type = { val _ = c; coordinatesAndHash }
      def &(c: distance.type): distanceAndHash.type       = { val _ = c; distanceAndHash }
    }
    object coordinatesAndDistance extends RadiusMode {
      override final type Reply = Arr.AsTuple[Bulk *: Bulk *: CoordinatesReply *: EmptyTuple]
      override final type Res   = KeyCoordinatesAndDistance
      override final val params: List[String]                     = List("WITHCOORD", "WITHDIST")
      override final val r: RESPRead.Aux[Arr.Of[Reply], Seq[Res]] = RESPRead[Arr.Of[Reply], Seq[Res]]

      def &(h: hash.type): all.type = { val _ = h; all }
    }
    object coordinatesAndHash extends RadiusMode {
      override final type Reply = Arr.AsTuple[Bulk *: Num *: CoordinatesReply *: EmptyTuple]
      override final type Res   = KeyCoordinatesAndHash
      override final val params: List[String]                     = List("WITHCOORD", "WITHHASH")
      override final val r: RESPRead.Aux[Arr.Of[Reply], Seq[Res]] = RESPRead[Arr.Of[Reply], Seq[Res]]

      def &(d: distance.type): all.type = { val _ = d; all }
    }
    object distanceAndHash extends RadiusMode {
      override final type Reply = Arr.AsTuple[Bulk *: Bulk *: Num *: EmptyTuple]
      override final type Res   = KeyDistanceAndHash
      override final val params: List[String]                     = List("WITHDIST", "WITHHASH")
      override final val r: RESPRead.Aux[Arr.Of[Reply], Seq[Res]] = RESPRead[Arr.Of[Reply], Seq[Res]]

      def &(c: coordinates.type): all.type = { val _ = c; all }
    }
    object all extends RadiusMode {
      override final type Reply = Arr.AsTuple[Bulk *: Bulk *: Num *: CoordinatesReply *: EmptyTuple]
      override final type Res   = KeyCoordinatesDistanceAndHash
      override final val params: List[String]                     = List("WITHCOORD", "WITHDIST", "WITHHASH")
      override final val r: RESPRead.Aux[Arr.Of[Reply], Seq[Res]] = RESPRead[Arr.Of[Reply], Seq[Res]]
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
  trait PolygonBuilders {
    def apply(first: Coordinates, second: Coordinates, third: Coordinates, more: Coordinates*): Polygon
  }
  private[laserdisc] object Polygon extends PolygonBuilders {
    def apply(first: Coordinates, second: Coordinates, third: Coordinates, more: Coordinates*): Polygon = {
      val vertices = first :: second :: third :: more.toList
      new Polygon(
        "BYPOLYGON" :: Show[Int].show(vertices.length) :: vertices.flatMap(c =>
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

  implicit final val radiusModeCoordinatesRead: Read[RadiusMode.coordinates.Reply, RadiusMode.coordinates.Res] =
    Read[RadiusMode.coordinates.Reply, Key *: Coordinates *: EmptyTuple].map { case k *: c *: _ => KeyAndCoordinates(k, c) }
  implicit final val radiusModeDistanceRead: Read[RadiusMode.distance.Reply, RadiusMode.distance.Res] =
    Read[RadiusMode.distance.Reply, Key *: NonNegDouble *: EmptyTuple].map { case k *: d *: _ => KeyAndDistance(k, d) }
  implicit final val radiusModeHashRead: Read[RadiusMode.hash.Reply, RadiusMode.hash.Res] =
    Read[RadiusMode.hash.Reply, Key *: NonNegLong *: EmptyTuple].map { case k *: h *: _ => KeyAndHash(k, h) }
  implicit final val radiusModeCoordinatesAndDistanceRead: Read[RadiusMode.coordinatesAndDistance.Reply, RadiusMode.coordinatesAndDistance.Res] =
    Read[RadiusMode.coordinatesAndDistance.Reply, Key *: NonNegDouble *: Coordinates *: EmptyTuple].map { case k *: d *: c *: _ =>
      KeyCoordinatesAndDistance(k, c, d)
    }
  implicit final val radiusModeCoordinatesAndHashRead: Read[RadiusMode.coordinatesAndHash.Reply, RadiusMode.coordinatesAndHash.Res] =
    Read[RadiusMode.coordinatesAndHash.Reply, Key *: NonNegLong *: Coordinates *: EmptyTuple].map { case k *: h *: c *: _ =>
      KeyCoordinatesAndHash(k, c, h)
    }
  implicit final val radiusModeDistanceAndHashRead: Read[RadiusMode.distanceAndHash.Reply, RadiusMode.distanceAndHash.Res] =
    Read[RadiusMode.distanceAndHash.Reply, Key *: NonNegDouble *: NonNegLong *: EmptyTuple].map { case k *: d *: h *: _ =>
      KeyDistanceAndHash(k, d, h)
    }
  implicit final val radiusModeAllRead: Read[RadiusMode.all.Reply, RadiusMode.all.Res] =
    Read[RadiusMode.all.Reply, Key *: NonNegDouble *: NonNegLong *: Coordinates *: EmptyTuple].map { case k *: d *: h *: c *: _ =>
      KeyCoordinatesDistanceAndHash(k, c, d, h)
    }

  // the builders each feature's commands take, and the types of their replies
  trait GeoTypes {
    final type GeoArea                = GeoP.Area
    final type GeoCenter              = GeoP.Center
    final type GeoCount               = GeoP.Count
    final type GeoFlag                = GeoP.Flag
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
  trait GeoPolygonTypes {
    final type GeoPolygon = GeoP.Polygon
    final val GeoPolygon: GeoP.PolygonBuilders = GeoP.Polygon
  }

  // the same names for laserdisc's own code, which can't read the members a version overrides while it's being built
  private[protocol] object internal extends GeoTypes with GeoPolygonTypes
}

trait GeoBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "GEOADD: CH NX XX",
      "GEODIST: FT KM M MI",
      "GEOHASH",
      "GEOPOS",
      "GEORADIUS: ANY ASC COUNT DESC FT KM M MI STORE STOREDIST WITHCOORD WITHDIST WITHHASH",
      "GEORADIUSBYMEMBER: ANY ASC COUNT DESC FT KM M MI STORE STOREDIST WITHCOORD WITHDIST WITHHASH",
      "GEORADIUSBYMEMBER_RO: ANY ASC COUNT DESC FT KM M MI WITHCOORD WITHDIST WITHHASH",
      "GEORADIUS_RO: ANY ASC COUNT DESC FT KM M MI WITHCOORD WITHDIST WITHHASH",
      "GEOSEARCH: ANY ASC BYBOX BYRADIUS COUNT DESC FROMLONLAT FROMMEMBER FT KM M MI WITHCOORD WITHDIST WITHHASH",
      "GEOSEARCHSTORE: ANY ASC BYBOX BYRADIUS COUNT DESC FROMLONLAT FROMMEMBER FT KM M MI STOREDIST"
    )

  /** The builders of this family's commands, and the types of their replies */
  val geotypes: GeoP.GeoTypes = new GeoP.GeoTypes {}

  import GeoP.internal.*

  private final def lonLatMember(positions: OneOrMore[GeoPosition]) =
    positions.value.map { case GeoPosition(m, lat, long) => (long -> lat) -> m }

  final def geoadd(key: Key, positions: OneOrMore[GeoPosition]): Single[NonNegInt] =
    Protocol("GEOADD", key *: lonLatMember(positions) *: EmptyTuple).as[Num, NonNegInt]
  final def geoadd(key: Key, flag: GeoFlag, positions: OneOrMore[GeoPosition]): Single[NonNegInt] =
    Protocol("GEOADD", key *: flag *: lonLatMember(positions) *: EmptyTuple).as[Num, NonNegInt]
  final def geoadd(key: Key, flag: GeoFlag, position: GeoPosition, positions: GeoPosition*): Single[NonNegInt] =
    geoadd(key, flag, OneOrMore.unsafeFrom(position :: positions.toList))

  /** GEOADD with CH, which replies with the number of positions added or changed */
  final def geoaddch(key: Key, positions: OneOrMore[GeoPosition]): Single[NonNegInt] =
    Protocol("GEOADD", key *: "CH" *: lonLatMember(positions) *: EmptyTuple).as[Num, NonNegInt]
  final def geoaddch(key: Key, position: GeoPosition, positions: GeoPosition*): Single[NonNegInt] =
    geoaddch(key, OneOrMore.unsafeFrom(position :: positions.toList))
  final def geoaddch(key: Key, flag: GeoFlag, positions: OneOrMore[GeoPosition]): Single[NonNegInt] =
    Protocol("GEOADD", key *: flag *: "CH" *: lonLatMember(positions) *: EmptyTuple).as[Num, NonNegInt]
  final def geoaddch(key: Key, flag: GeoFlag, position: GeoPosition, positions: GeoPosition*): Single[NonNegInt] =
    geoaddch(key, flag, OneOrMore.unsafeFrom(position :: positions.toList))

  final def geodist(key: Key, member1: Key, member2: Key): Single[Option[NonNegDouble]] =
    Protocol("GEODIST", key *: member1 *: member2 *: EmptyTuple).as[OrNullBulk[Bulk], Option[NonNegDouble]]
  final def geodist(key: Key, member1: Key, member2: Key, unit: GeoUnit): Single[Option[NonNegDouble]] =
    Protocol("GEODIST", key *: member1 *: member2 *: unit *: EmptyTuple).as[OrNullBulk[Bulk], Option[NonNegDouble]]

  final def geohash(key: Key, members: OneOrMoreKeys): Single[Seq[Option[GeoHash]]] =
    Protocol("GEOHASH", key :: members.value).as[Arr.Of[OrNullBulk[Bulk]], Seq[Option[GeoHash]]]

  final def geopos(key: Key, members: OneOrMoreKeys): Single[Seq[Option[GeoCoordinates]]] =
    Protocol("GEOPOS", key :: members.value).as[Arr.Of[OrNilArr[GeoP.CoordinatesReply]], Seq[Option[GeoCoordinates]]]

  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit): Single[Seq[Key]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Single[Seq[Key]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: EmptyTuple)
      .as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, count: GeoCount): Single[Seq[Key]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: count.params *: EmptyTuple)
      .as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, sort: Direction): Single[Seq[Key]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: sort *: EmptyTuple)
      .as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      sort: Direction
  ): Single[Seq[Key]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: sort *: EmptyTuple)
      .as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      count: GeoCount,
      sort: Direction
  ): Single[Seq[Key]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: count.params *: sort *: EmptyTuple)
      .as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: mode.params *: EmptyTuple)
      .as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: mode.params *: EmptyTuple
    )
      .as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, count: GeoCount, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: count.params *: mode.params *: EmptyTuple
    )
      .as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: sort *: mode.params *: EmptyTuple)
      .as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      sort: Direction,
      mode: GeoRadiusMode
  )(implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]): Single[Seq[mode.Res]] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: sort *: mode.params *: EmptyTuple
    ).as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      count: GeoCount,
      sort: Direction,
      mode: GeoRadiusMode
  )(implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]): Single[Seq[mode.Res]] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: count.params *: sort *: mode.params *: EmptyTuple
    ).as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      store: GeoStoreMode
  ): Single[NonNegInt] =
    Protocol("GEORADIUS", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: store.params *: EmptyTuple)
      .as[Num, NonNegInt]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      store: GeoStoreMode
  ): Single[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: store.params *: EmptyTuple
    ).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      count: GeoCount,
      store: GeoStoreMode
  ): Single[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: count.params *: store.params *: EmptyTuple
    ).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      sort: Direction,
      store: GeoStoreMode
  ): Single[NonNegInt] =
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
  ): Single[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: sort *: store.params *: EmptyTuple
    ).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      coordinates: GeoCoordinates,
      radius: NonNegDouble,
      unit: GeoUnit,
      count: GeoCount,
      sort: Direction,
      store: GeoStoreMode
  ): Single[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: count.params *: sort *: store.params *: EmptyTuple
    ).as[Num, NonNegInt]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit): Single[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Single[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, count: GeoCount): Single[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: count.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction): Single[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, sort: Direction): Single[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, count: GeoCount, sort: Direction): Single[Seq[Key]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: count.params *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: mode.params *: EmptyTuple).as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: mode.params *: EmptyTuple)
      .as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, count: GeoCount, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: count.params *: mode.params *: EmptyTuple)
      .as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: sort *: mode.params *: EmptyTuple)
      .as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: sort *: mode.params *: EmptyTuple)
      .as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, count: GeoCount, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: count.params *: sort *: mode.params *: EmptyTuple)
      .as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, store: GeoStoreMode): Single[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: store.params *: EmptyTuple).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      member: Key,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      store: GeoStoreMode
  ): Single[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: store.params *: EmptyTuple).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      member: Key,
      radius: NonNegDouble,
      unit: GeoUnit,
      count: GeoCount,
      store: GeoStoreMode
  ): Single[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: count.params *: store.params *: EmptyTuple).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      member: Key,
      radius: NonNegDouble,
      unit: GeoUnit,
      sort: Direction,
      store: GeoStoreMode
  ): Single[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: sort *: store.params *: EmptyTuple).as[Num, NonNegInt]
  final def georadius(
      key: Key,
      member: Key,
      radius: NonNegDouble,
      unit: GeoUnit,
      limit: PosInt,
      sort: Direction,
      store: GeoStoreMode
  ): Single[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: "COUNT" *: limit *: sort *: store.params *: EmptyTuple)
      .as[Num, NonNegInt]
  final def georadius(
      key: Key,
      member: Key,
      radius: NonNegDouble,
      unit: GeoUnit,
      count: GeoCount,
      sort: Direction,
      store: GeoStoreMode
  ): Single[NonNegInt] =
    Protocol("GEORADIUSBYMEMBER", key *: member *: radius *: unit *: count.params *: sort *: store.params *: EmptyTuple)
      .as[Num, NonNegInt]

  final def geosearch(key: Key, area: GeoArea): Single[Seq[Key]] =
    Protocol("GEOSEARCH", key *: area.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, area: GeoArea, sort: Direction): Single[Seq[Key]] =
    Protocol("GEOSEARCH", key *: area.params *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, area: GeoArea, count: GeoCount): Single[Seq[Key]] =
    Protocol("GEOSEARCH", key *: area.params *: count.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, area: GeoArea, sort: Direction, count: GeoCount): Single[Seq[Key]] =
    Protocol("GEOSEARCH", key *: area.params *: sort *: count.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, area: GeoArea, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: area.params *: mode.params *: EmptyTuple).as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def geosearch(key: Key, area: GeoArea, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: area.params *: sort *: mode.params *: EmptyTuple).as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def geosearch(key: Key, area: GeoArea, count: GeoCount, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: area.params *: count.params *: mode.params *: EmptyTuple).as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def geosearch(key: Key, area: GeoArea, sort: Direction, count: GeoCount, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: area.params *: sort *: count.params *: mode.params *: EmptyTuple).as[Arr.Of[mode.Reply], Seq[mode.Res]]

  final def geosearchstore(source: Key, destination: Key, area: GeoArea): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, area: GeoArea, sort: Direction): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: sort *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, area: GeoArea, count: GeoCount): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: count.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, area: GeoArea, sort: Direction, count: GeoCount): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: sort *: count.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, area: GeoArea, distances: GeoStoreDistances): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      area: GeoArea,
      sort: Direction,
      distances: GeoStoreDistances
  ): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: sort *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      area: GeoArea,
      count: GeoCount,
      distances: GeoStoreDistances
  ): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: count.params *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      area: GeoArea,
      sort: Direction,
      count: GeoCount,
      distances: GeoStoreDistances
  ): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: area.params *: sort *: count.params *: distances *: EmptyTuple)
      .as[Num, NonNegInt]

  object ro {
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit): Single[Seq[Key]] =
      Protocol("GEORADIUS_RO", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: EmptyTuple)
        .as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Single[Seq[Key]] =
      Protocol("GEORADIUS_RO", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: EmptyTuple)
        .as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, count: GeoCount): Single[Seq[Key]] =
      Protocol("GEORADIUS_RO", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: count.params *: EmptyTuple)
        .as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(
        key: Key,
        coordinates: GeoCoordinates,
        radius: NonNegDouble,
        unit: GeoUnit,
        sort: Direction
    ): Single[Seq[Key]] =
      Protocol("GEORADIUS_RO", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: sort *: EmptyTuple)
        .as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(
        key: Key,
        coordinates: GeoCoordinates,
        radius: NonNegDouble,
        unit: GeoUnit,
        limit: PosInt,
        sort: Direction
    ): Single[Seq[Key]] =
      Protocol(
        "GEORADIUS_RO",
        key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: sort *: EmptyTuple
      )
        .as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(
        key: Key,
        coordinates: GeoCoordinates,
        radius: NonNegDouble,
        unit: GeoUnit,
        count: GeoCount,
        sort: Direction
    ): Single[Seq[Key]] =
      Protocol(
        "GEORADIUS_RO",
        key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: count.params *: sort *: EmptyTuple
      )
        .as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
        implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
    ): Single[Seq[mode.Res]] =
      Protocol("GEORADIUS_RO", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: mode.params *: EmptyTuple)
        .as[Arr.Of[mode.Reply], Seq[mode.Res]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
        implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
    ): Single[Seq[mode.Res]] =
      Protocol(
        "GEORADIUS_RO",
        key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: mode.params *: EmptyTuple
      ).as[Arr.Of[mode.Reply], Seq[mode.Res]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, count: GeoCount, mode: GeoRadiusMode)(
        implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
    ): Single[Seq[mode.Res]] =
      Protocol(
        "GEORADIUS_RO",
        key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: count.params *: mode.params *: EmptyTuple
      ).as[Arr.Of[mode.Reply], Seq[mode.Res]]
    final def georadius(key: Key, coordinates: GeoCoordinates, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
        implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
    ): Single[Seq[mode.Res]] =
      Protocol("GEORADIUS_RO", key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: sort *: mode.params *: EmptyTuple)
        .as[Arr.Of[mode.Reply], Seq[mode.Res]]
    final def georadius(
        key: Key,
        coordinates: GeoCoordinates,
        radius: NonNegDouble,
        unit: GeoUnit,
        limit: PosInt,
        sort: Direction,
        mode: GeoRadiusMode
    )(implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]): Single[Seq[mode.Res]] =
      Protocol(
        "GEORADIUS_RO",
        key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: "COUNT" *: limit *: sort *: mode.params *: EmptyTuple
      ).as[Arr.Of[mode.Reply], Seq[mode.Res]]
    final def georadius(
        key: Key,
        coordinates: GeoCoordinates,
        radius: NonNegDouble,
        unit: GeoUnit,
        count: GeoCount,
        sort: Direction,
        mode: GeoRadiusMode
    )(implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]): Single[Seq[mode.Res]] =
      Protocol(
        "GEORADIUS_RO",
        key *: coordinates.longitude *: coordinates.latitude *: radius *: unit *: count.params *: sort *: mode.params *: EmptyTuple
      ).as[Arr.Of[mode.Reply], Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit): Single[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt): Single[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: "COUNT" *: limit *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, count: GeoCount): Single[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: count.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction): Single[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(
        key: Key,
        member: Key,
        radius: NonNegDouble,
        unit: GeoUnit,
        limit: PosInt,
        sort: Direction
    ): Single[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: "COUNT" *: limit *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(
        key: Key,
        member: Key,
        radius: NonNegDouble,
        unit: GeoUnit,
        count: GeoCount,
        sort: Direction
    ): Single[Seq[Key]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: count.params *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, mode: GeoRadiusMode)(
        implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
    ): Single[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: mode.params *: EmptyTuple).as[Arr.Of[mode.Reply], Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, mode: GeoRadiusMode)(
        implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
    ): Single[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: "COUNT" *: limit *: mode.params *: EmptyTuple)
        .as[Arr.Of[mode.Reply], Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, count: GeoCount, mode: GeoRadiusMode)(
        implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
    ): Single[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: count.params *: mode.params *: EmptyTuple)
        .as[Arr.Of[mode.Reply], Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, sort: Direction, mode: GeoRadiusMode)(
        implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
    ): Single[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: sort *: mode.params *: EmptyTuple)
        .as[Arr.Of[mode.Reply], Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, limit: PosInt, sort: Direction, mode: GeoRadiusMode)(
        implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
    ): Single[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: "COUNT" *: limit *: sort *: mode.params *: EmptyTuple)
        .as[Arr.Of[mode.Reply], Seq[mode.Res]]
    final def georadius(key: Key, member: Key, radius: NonNegDouble, unit: GeoUnit, count: GeoCount, sort: Direction, mode: GeoRadiusMode)(
        implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
    ): Single[Seq[mode.Res]] =
      Protocol("GEORADIUSBYMEMBER_RO", key *: member *: radius *: unit *: count.params *: sort *: mode.params *: EmptyTuple)
        .as[Arr.Of[mode.Reply], Seq[mode.Res]]
  }
}

/** GEOSEARCH and GEOSEARCHSTORE with BYPOLYGON: Valkey 9.0 */
trait GeoSearchPolygonP extends Features { this: GeoBaseP =>
  override private[laserdisc] def supported: List[String] = super.supported ++ List("GEOSEARCH: BYPOLYGON", "GEOSEARCHSTORE: BYPOLYGON")

  import GeoP.internal.*

  final def geosearch(key: Key, polygon: GeoPolygon): Single[Seq[Key]] =
    Protocol("GEOSEARCH", key *: polygon.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, polygon: GeoPolygon, sort: Direction): Single[Seq[Key]] =
    Protocol("GEOSEARCH", key *: polygon.params *: sort *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, polygon: GeoPolygon, count: GeoCount): Single[Seq[Key]] =
    Protocol("GEOSEARCH", key *: polygon.params *: count.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, polygon: GeoPolygon, sort: Direction, count: GeoCount): Single[Seq[Key]] =
    Protocol("GEOSEARCH", key *: polygon.params *: sort *: count.params *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]
  final def geosearch(key: Key, polygon: GeoPolygon, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: polygon.params *: mode.params *: EmptyTuple).as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def geosearch(key: Key, polygon: GeoPolygon, sort: Direction, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: polygon.params *: sort *: mode.params *: EmptyTuple).as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def geosearch(key: Key, polygon: GeoPolygon, count: GeoCount, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: polygon.params *: count.params *: mode.params *: EmptyTuple).as[Arr.Of[mode.Reply], Seq[mode.Res]]
  final def geosearch(key: Key, polygon: GeoPolygon, sort: Direction, count: GeoCount, mode: GeoRadiusMode)(
      implicit ev: RESPRead.Aux[Arr.Of[mode.Reply], Seq[mode.Res]]
  ): Single[Seq[mode.Res]] =
    Protocol("GEOSEARCH", key *: polygon.params *: sort *: count.params *: mode.params *: EmptyTuple).as[Arr.Of[mode.Reply], Seq[mode.Res]]

  final def geosearchstore(source: Key, destination: Key, polygon: GeoPolygon): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, polygon: GeoPolygon, sort: Direction): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: sort *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, polygon: GeoPolygon, count: GeoCount): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: count.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, polygon: GeoPolygon, sort: Direction, count: GeoCount): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: sort *: count.params *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(source: Key, destination: Key, polygon: GeoPolygon, distances: GeoStoreDistances): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      polygon: GeoPolygon,
      sort: Direction,
      distances: GeoStoreDistances
  ): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: sort *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      polygon: GeoPolygon,
      count: GeoCount,
      distances: GeoStoreDistances
  ): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: count.params *: distances *: EmptyTuple).as[Num, NonNegInt]
  final def geosearchstore(
      source: Key,
      destination: Key,
      polygon: GeoPolygon,
      sort: Direction,
      count: GeoCount,
      distances: GeoStoreDistances
  ): Single[NonNegInt] =
    Protocol("GEOSEARCHSTORE", destination *: source *: polygon.params *: sort *: count.params *: distances *: EmptyTuple)
      .as[Num, NonNegInt]
}

trait GeoP extends GeoBaseP with GeoExtP with GeoSearchPolygonP {
  override val geotypes: GeoP.GeoTypes with GeoP.GeoPolygonTypes = new GeoP.GeoTypes with GeoP.GeoPolygonTypes {}
}
