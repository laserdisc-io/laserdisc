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
package cli

import _root_.fs2.io.net.Network
import _root_.fs2.io.net.tls.TLSContext
import cats.effect.{IO, Resource}

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.security.KeyStore
import java.security.cert.{Certificate, CertificateFactory}

// a TLS context trusting the certificate authorities given, as PEM, and no others
private[cli] object TrustedAuthorities {
  def context(pem: String): Resource[IO, TLSContext[IO]] =
    Resource.eval(IO {
      val certificates = CertificateFactory.getInstance("X.509").generateCertificates(new ByteArrayInputStream(pem.getBytes(UTF_8)))
      val store        = KeyStore.getInstance(KeyStore.getDefaultType)
      store.load(null, null)
      certificates.toArray.toList.collect { case certificate: Certificate => certificate }.zipWithIndex.foreach { case (certificate, i) =>
        store.setCertificateEntry(s"ca$i", certificate)
      }
      if (store.size == 0) throw new IllegalArgumentException("there's no certificate in it")
      store
    }.flatMap(store => Network[IO].tlsContext.fromKeyStore(store, Array.emptyCharArray)))
}
