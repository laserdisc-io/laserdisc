package laserdisc

final case class NaN()

final case class KV[A](key: Key, value: A)
final case class ScanKV(cursor: NonNegLong, maybeValues: Option[Seq[KV[String]]])
final case class Scan[A](cursor: NonNegLong, values: Option[Seq[A]])
final case class Time(timestamp: NonNegLong, elapsedMicroseconds: NonNegLong)
