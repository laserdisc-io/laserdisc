package laserdisc
package protocol

object GeoP {
  final case class Coordinates(latitude: Latitude, longitude: Longitude)
  final object Coordinates {
    implicit final val arr2Coordinates: Read[Arr, Coordinates] = Read.instancePF {
      case Arr(Bulk(ToDouble(Longitude(long))) +: Bulk(ToDouble(Latitude(lat))) +: Seq()) => Coordinates(lat, long)
    }
  }
  final case class Position(member: Key, latitude: Latitude, longitude: Longitude)

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
}

trait GeoBaseP {
  import auto._
  import shapeless._

  final object geos {
    type Coordinates = GeoP.Coordinates
    type Position    = GeoP.Position
    type Unit        = GeoP.Unit

    final val Position = GeoP.Position
    final val unit     = GeoP.Unit
  }

  final def geoadd(key: Key, positions: OneOrMore[geos.Position]): Protocol.Aux[NonNegInt] =
    Protocol("GEOADD", key :: positions.map(p => (p.longitude -> p.latitude) -> p.member) :: HNil).as[Num, NonNegInt]

  final def geodist(key: Key, member1: Key, member2: Key): Protocol.Aux[Option[ValidDouble]] =
    Protocol("GEODIST", key :: member1 :: member2 :: HNil).opt[GenBulk].as[ValidDouble]
  final def geodist(key: Key, member1: Key, member2: Key, unit: geos.Unit): Protocol.Aux[Option[ValidDouble]] =
    Protocol("GEODIST", key :: member1 :: member2 :: unit :: HNil).opt[GenBulk].as[ValidDouble]

  final def geohash(key: Key, members: OneOrMoreKeys): Protocol.Aux[Seq[Option[GeoHash]]] =
    Protocol("GEOHASH", key :: members).as[Arr, Seq[Option[GeoHash]]]

  final def geopos(key: Key, members: OneOrMoreKeys): Protocol.Aux[Seq[Option[geos.Coordinates]]] =
    Protocol("GEOPOS", key :: members).as[Arr, Seq[Option[geos.Coordinates]]]

  final def georadius(key: Key, coordinates: geos.Coordinates, radius: ValidDouble, unit: geos.Unit): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: HNil).as[Arr, Seq[Key]]
  final def georadius(
      key: Key,
      coordinates: geos.Coordinates,
      radius: ValidDouble,
      unit: geos.Unit,
      limit: PosInt
  ): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: HNil)
      .as[Arr, Seq[Key]]
  final def georadius(
      key: Key,
      coordinates: geos.Coordinates,
      radius: ValidDouble,
      unit: geos.Unit,
      sort: Direction
  ): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: HNil).as[Arr, Seq[Key]]
  final def georadius(
      key: Key,
      coordinates: geos.Coordinates,
      radius: ValidDouble,
      unit: geos.Unit,
      limit: PosInt,
      sort: Direction
  ): Protocol.Aux[Seq[Key]] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: HNil)
      .as[Arr, Seq[Key]]

//  final def georadiuswithcoord(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates)]] =
//    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "WITHCOORD" :: HNil)
//      .as[Arr, Seq[(Key, geos.Coordinates)]]
//
//  final def georadiuswithcoord(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: "WITHCOORD" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates)]]
//
//  final def georadiuswithcoord(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: "WITHCOORD" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates)]]
//
//  final def georadiuswithcoord(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: "WITHCOORD" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates)]]
//
//  final def georadiuswithdist(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit
//  ): Protocol.Aux[Seq[(Key, NonNegDouble)]] =
//    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "WITHDIST" :: HNil)
//      .as[Arr, Seq[(Key, NonNegDouble)]]
//
//  final def georadiuswithdist(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt
//  ): Protocol.Aux[Seq[(Key, NonNegDouble)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: "WITHDIST" :: HNil
//    ).as[Arr, Seq[(Key, NonNegDouble)]]
//
//  final def georadiuswithdist(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, NonNegDouble)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: "WITHDIST" :: HNil
//    ).as[Arr, Seq[(Key, NonNegDouble)]]
//
//  final def georadiuswithdist(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, NonNegDouble)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: "WITHDIST" :: HNil
//    ).as[Arr, Seq[(Key, NonNegDouble)]]
//
//  final def georadiuswithhash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit
//  ): Protocol.Aux[Seq[(Key, GeoHash)]] =
//    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "WITHHASH" :: HNil)
//      .as[Arr, Seq[(Key, GeoHash)]]
//
//  final def georadiuswithhash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt
//  ): Protocol.Aux[Seq[(Key, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, GeoHash)]]
//
//  final def georadiuswithhash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, GeoHash)]]
//
//  final def georadiuswithhash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, GeoHash)]]
//
//  final def georadiuswithcoorddist(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, NonNegDouble)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "WITHCOORD" :: "WITHDIST" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, NonNegDouble)]]
//
//  final def georadiuswithcoorddist(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, NonNegDouble)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: "WITHCOORD" :: "WITHDIST" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, NonNegDouble)]]
//
//  final def georadiuswithcoorddist(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, NonNegDouble)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: "WITHCOORD" :: "WITHDIST" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, NonNegDouble)]]
//
//  final def georadiuswithcoorddist(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, NonNegDouble)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: "WITHCOORD" :: "WITHDIST" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, NonNegDouble)]]
//
//  final def georadiuswithcoordhash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "WITHCOORD" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, GeoHash)]]
//
//  final def georadiuswithcoordhash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: "WITHCOORD" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, GeoHash)]]
//
//  final def georadiuswithcoordhash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: "WITHCOORD" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, GeoHash)]]
//
//  final def georadiuswithcoordhash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: "WITHCOORD" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, GeoHash)]]
//
//  final def georadiuswithdisthash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit
//  ): Protocol.Aux[Seq[(Key, NonNegDouble, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "WITHDIST" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, NonNegDouble, GeoHash)]]
//
//  final def georadiuswithdisthash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt
//  ): Protocol.Aux[Seq[(Key, NonNegDouble, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: "WITHDIST" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, NonNegDouble, GeoHash)]]
//
//  final def georadiuswithdisthash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, NonNegDouble, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: "WITHDIST" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, NonNegDouble, GeoHash)]]
//
//  final def georadiuswithdisthash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, NonNegDouble, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: "WITHDIST" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, NonNegDouble, GeoHash)]]
//
//  final def georadiuswithcoorddisthash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, NonNegDouble, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "WITHCOORD" :: "WITHDIST" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, NonNegDouble, GeoHash)]]
//
//  final def georadiuswithcoorddisthash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, NonNegDouble, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: "WITHCOORD" :: "WITHDIST" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, NonNegDouble, GeoHash)]]
//
//  final def georadiuswithcoorddisthash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, NonNegDouble, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: "WITHCOORD" :: "WITHDIST" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, NonNegDouble, GeoHash)]]
//
//  final def georadiuswithcoorddisthash(
//      key: Key,
//      coordinates: geos.Coordinates,
//      radius: ValidDouble,
//      unit: geos.Unit,
//      limit: PosInt,
//      sort: Direction
//  ): Protocol.Aux[Seq[(Key, geos.Coordinates, NonNegDouble, GeoHash)]] =
//    Protocol(
//      "GEORADIUS",
//      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: "WITHCOORD" :: "WITHDIST" :: "WITHHASH" :: HNil
//    ).as[Arr, Seq[(Key, geos.Coordinates, NonNegDouble, GeoHash)]]

  final def georadiusstore(
      key: Key,
      coordinates: geos.Coordinates,
      radius: ValidDouble,
      unit: geos.Unit,
      newKey: Key
  ): Protocol.Aux[NonNegInt] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "STORE" :: newKey :: HNil)
      .as[Num, NonNegInt]
  final def georadiusstore(
      key: Key,
      coordinates: geos.Coordinates,
      radius: ValidDouble,
      unit: geos.Unit,
      limit: PosInt,
      newKey: Key
  ): Protocol.Aux[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: "STORE" :: newKey :: HNil
    ).as[Num, NonNegInt]
  final def georadiusstore(
      key: Key,
      coordinates: geos.Coordinates,
      radius: ValidDouble,
      unit: geos.Unit,
      sort: Direction,
      newKey: Key
  ): Protocol.Aux[NonNegInt] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: "STORE" :: newKey :: HNil)
      .as[Num, NonNegInt]
  final def georadiusstore(
      key: Key,
      coordinates: geos.Coordinates,
      radius: ValidDouble,
      unit: geos.Unit,
      limit: PosInt,
      sort: Direction,
      newKey: Key
  ): Protocol.Aux[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: "STORE" :: newKey :: HNil
    ).as[Num, NonNegInt]

  final def georadiusstoredist(
      key: Key,
      coordinates: geos.Coordinates,
      radius: ValidDouble,
      unit: geos.Unit,
      newKey: Key
  ): Protocol.Aux[NonNegInt] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "STOREDIST" :: newKey :: HNil)
      .as[Num, NonNegInt]
  final def georadiusstoredist(
      key: Key,
      coordinates: geos.Coordinates,
      radius: ValidDouble,
      unit: geos.Unit,
      limit: PosInt,
      newKey: Key
  ): Protocol.Aux[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: "STOREDIST" :: newKey :: HNil
    ).as[Num, NonNegInt]
  final def georadiusstoredist(
      key: Key,
      coordinates: geos.Coordinates,
      radius: ValidDouble,
      unit: geos.Unit,
      sort: Direction,
      newKey: Key
  ): Protocol.Aux[NonNegInt] =
    Protocol("GEORADIUS", key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: sort :: "STOREDIST" :: newKey :: HNil)
      .as[Num, NonNegInt]
  final def georadiusstoredist(
      key: Key,
      coordinates: geos.Coordinates,
      radius: ValidDouble,
      unit: geos.Unit,
      limit: PosInt,
      sort: Direction,
      newKey: Key
  ): Protocol.Aux[NonNegInt] =
    Protocol(
      "GEORADIUS",
      key :: coordinates.longitude :: coordinates.latitude :: radius :: unit :: "COUNT" :: limit :: sort :: "STOREDIST" :: newKey :: HNil
    ).as[Num, NonNegInt]
}

trait GeoP extends GeoBaseP with GeoExtraP
