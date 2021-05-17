package laserdisc
package fs2

import cats.syntax.eq._
import cats.{ApplicativeError, Eq}
import com.comcast.ip4s
import com.comcast.ip4s.SocketAddress

import java.net.InetSocketAddress

final case class RedisAddress(host: Host, port: Port) {
  def toInetSocketAddress[F[_]](implicit F: ApplicativeError[F, Throwable]): F[InetSocketAddress] =
    F.catchNonFatal {
      new InetSocketAddress(host.value, port.value)
    }
  def toSocketAddress[F[_]](implicit F: ApplicativeError[F, Throwable]): F[SocketAddress[ip4s.Host]] =
    (for {
      h <- ip4s.Host.fromString(host.value)
      p <- ip4s.Port.fromInt(port.value)
    } yield new SocketAddress(h, p)) match {
      case None    => F.raiseError(InvalidSocketAddress(host, port))
      case Some(a) => F.pure(a)
    }

  override def toString: String = host.value + ":" + port.value
}

object RedisAddress {
  implicit final val redisAddressEq: Eq[RedisAddress] = Eq.instance { (a1, a2) =>
    a1.host.value === a2.host.value && a1.port.value === a2.port.value
  }
}
