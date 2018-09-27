package laserdisc

package object fs2 {
  final type Pipe[F[_], -I, +O] = _root_.fs2.Pipe[F, I, O]
  final type Queue[F[_]]        = _root_.fs2.concurrent.Queue[F, Request[F]]
  final type Signal[F[_], A]    = _root_.fs2.concurrent.SignallingRef[F, A]
  final type Stream[+F[_], +O]  = _root_.fs2.Stream[F, O]

  final val Queue  = _root_.fs2.concurrent.Queue
  final val Signal = _root_.fs2.concurrent.SignallingRef
  final val Stream = _root_.fs2.Stream
}
