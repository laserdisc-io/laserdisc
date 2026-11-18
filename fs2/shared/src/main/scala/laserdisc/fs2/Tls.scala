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
package fs2

import _root_.fs2.io.net.{Network, Socket}
import _root_.fs2.io.net.tls.TLSContext
import cats.effect.{Resource, Sync}

/** Connecting with TLS, trusting the certificates `context` trusts. The server's certificate also has to be for the host connected to,
  * unless `checkServerName` is false, as when connecting to an address the certificate doesn't name
  */
final case class Tls[F[_]](context: TLSContext[F], checkServerName: Boolean = true) {

  // the host is sent as the server name, which servers that have a certificate for each name choose by. The handshake is done here, so
  // that it's within the time connecting has
  private[fs2] def secure(socket: Socket[F], host: String)(implicit S: Sync[F]): Resource[F, Socket[F]] =
    context
      .clientBuilder(socket)
      .withParameters(TlsParameters(host, checkServerName))
      .build
      .evalTap(TlsHandshake(_, host, checkServerName))
      .map(tls => tls: Socket[F])
}

object Tls {

  /** Trusting the certificates the system trusts, as for a server whose certificate a public authority signed */
  def system[F[_]: Network]: Resource[F, Tls[F]] = Network[F].tlsContext.systemResource.map(Tls(_))
}
