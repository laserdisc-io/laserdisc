package laserdisc

package object fs2 {
  final type Queue[F[_]] = _root_.fs2.async.mutable.Queue[F, Request[F]]
}
