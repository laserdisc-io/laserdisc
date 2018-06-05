package laserdisc
package fs2

sealed trait Request[F[_]] {
  type A
  def protocol: Protocol.Aux[A]
  def callback: Maybe[Maybe[A]] => F[Unit]
}

object Request {
  sealed abstract case class Req[F[_], A0](protocol: Protocol.Aux[A0], callback: Maybe[Maybe[A0]] => F[Unit])
      extends Request[F] { override type A = A0 }

  def apply[F[_], A](protocol: Protocol.Aux[A], callback: Maybe[Maybe[A]] => F[Unit]): Request[F] =
    new Req[F, A](protocol, callback) {}

  def unapply[F[_]](req: Request[F]): Option[(Protocol.Aux[req.A], Maybe[Maybe[req.A]] => F[Unit])] =
    Some(req.protocol -> req.callback)
}
