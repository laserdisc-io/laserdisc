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

package laserdisc.fs2

import _root_.fs2.Chunk
import _root_.fs2.io.net.tls.TLSParameters
import com.comcast.ip4s.IpAddress

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

// Node checks the certificate against the server name. An address can't be one, so its certificate is checked against the address here:
// without a server name, Node wouldn't check it against anything
private[fs2] object TlsParameters {
  final def apply(host: String, checkServerName: Boolean): TLSParameters =
    (IpAddress.fromString(host), checkServerName) match {
      case (None, true)     => TLSParameters(servername = Some(host))
      case (None, false)    => TLSParameters(servername = Some(host), checkServerIdentity = Some(anyName))
      case (Some(_), true)  => TLSParameters(checkServerIdentity = Some(forAddress(host)))
      case (Some(_), false) => TLSParameters(checkServerIdentity = Some(anyName))
    }

  private val anyName: TLSParameters.CheckServerIdentity = new TLSParameters.CheckServerIdentity {
    override def apply(servername: String, cert: Chunk[Byte]): Either[Throwable, Unit] = Right(())
  }

  private def forAddress(address: String): TLSParameters.CheckServerIdentity = new TLSParameters.CheckServerIdentity {
    override def apply(servername: String, cert: Chunk[Byte]): Either[Throwable, Unit] =
      if (new X509Certificate(cert.toUint8Array).checkIP(address).isDefined) Right(())
      else Left(new _root_.fs2.io.net.tls.SSLException(s"The server's certificate isn't for $address"))
  }

  @js.native
  @JSImport("crypto", "X509Certificate")
  private class X509Certificate(@unused raw: Uint8Array) extends js.Object {
    def checkIP(ip: String): js.UndefOr[String] = js.native
  }
}
