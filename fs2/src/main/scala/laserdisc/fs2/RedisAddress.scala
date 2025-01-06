/*
 * Copyright (c) 2018-2025 LaserDisc
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
