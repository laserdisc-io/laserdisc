package laserdisc

final case class NaN()

final case class KV[A](key: Key, value: A)
final case class ScanKV(cursor: NonNegLong, maybeValues: Option[Seq[KV[String]]])
final case class Scan[A](cursor: NonNegLong, values: Option[Seq[A]])
final case class Time(timestamp: NonNegLong, elapsedMicroseconds: NonNegLong)

sealed trait Direction
object Direction {
  final object asc  extends Direction
  final object desc extends Direction

  implicit val directionShow: Show[Direction] = Show.instance {
    case `asc`  => "ASC"
    case `desc` => "DESC"
  }
}
