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

import _root_.fs2.io.net.Network
import cats.effect.{IO, Resource}

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.security.KeyStore
import java.security.cert.CertificateFactory
import javax.net.ssl.{SSLContext, TrustManagerFactory}

// a TLS context trusting the certificate given, as PEM
object TrustedTls {
  final val supported = true

  def trusting(pem: String): Resource[IO, Tls[IO]] =
    Resource.eval(IO {
      val certificate = CertificateFactory.getInstance("X.509").generateCertificate(new ByteArrayInputStream(pem.getBytes(UTF_8)))
      val store       = KeyStore.getInstance(KeyStore.getDefaultType)
      store.load(null, null)
      store.setCertificateEntry("ca", certificate)
      val trust = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      trust.init(store)
      val context = SSLContext.getInstance("TLS")
      context.init(null, trust.getTrustManagers, null)
      Tls(Network[IO].tlsContext.fromSSLContext(context))
    })
}
