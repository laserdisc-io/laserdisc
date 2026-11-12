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

import _root_.fs2.io.net.tls.TLSParameters
import com.comcast.ip4s.IpAddress

import java.security.cert.{Certificate, X509Certificate}
import javax.net.ssl.{SNIHostName, SSLException}

// the JVM checks the certificate against the server name only when asked to, as for HTTPS. An address isn't sent as a server name, so the
// JVM has nothing to check its certificate against: it's checked after the handshake instead. A name ending with a dot is sent without it
private[fs2] object TlsParameters {
  final def apply(host: String, checkServerName: Boolean): TLSParameters =
    IpAddress.fromString(host) match {
      case Some(_) => TLSParameters(serverNames = Some(Nil), endpointIdentificationAlgorithm = None)
      case None    =>
        TLSParameters(
          serverNames = Some(List(new SNIHostName(host.stripSuffix(".")))),
          endpointIdentificationAlgorithm = if (checkServerName) Some("HTTPS") else None
        )
    }

  // whether the server's certificate, the first of those it sent, has the address among its IP addresses
  final def checkAddress(address: IpAddress, certificates: Array[Certificate]): Either[SSLException, Unit] = {
    val addresses = certificates.headOption.toList.flatMap {
      case certificate: X509Certificate =>
        // each name is its type, 7 for an IP address, and its value
        Option(certificate.getSubjectAlternativeNames).toList
          .flatMap(_.toArray.toList)
          .collect {
            case name: java.util.List[?] if name.get(0) == Integer.valueOf(7) => IpAddress.fromString(name.get(1).toString)
          }
          .flatten
      case _ => Nil
    }
    if (addresses.contains(address)) Right(()) else Left(new SSLException(s"The server's certificate isn't for $address"))
  }
}
