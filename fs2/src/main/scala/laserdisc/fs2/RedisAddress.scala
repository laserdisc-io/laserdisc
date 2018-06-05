package laserdisc
package fs2

import java.net.InetSocketAddress

import cats.ApplicativeError

final case class RedisAddress(host: Host, port: Port) {
  def toInetSocketAddress[F[_]](implicit F: ApplicativeError[F, Throwable]): F[InetSocketAddress] = F.catchNonFatal {
    new InetSocketAddress(host.value, port.value)
  }
  override def toString: String = host.value + ":" + port.value
}
