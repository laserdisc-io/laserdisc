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

import com.comcast.ip4s.IpAddress
import munit.FunSuite

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.security.cert.{Certificate, CertificateFactory}
import javax.net.ssl.SNIHostName

final class TlsParametersSpec extends FunSuite {

  // a certificate for 127.0.0.1, ::1 and localhost
  private val forAddresses: Certificate = CertificateFactory
    .getInstance("X.509")
    .generateCertificate(new ByteArrayInputStream("""-----BEGIN CERTIFICATE-----
MIIDNjCCAh6gAwIBAgIUOF7Ij3GqelE7Doy4P5z/BYGKGfAwDQYJKoZIhvcNAQEL
BQAwIzEhMB8GA1UEAwwYbGFzZXJkaXNjIHRlc3QgYWRkcmVzc2VzMCAXDTI2MTEx
MjIxMDc0MVoYDzIxMjYxMDE5MjEwNzQxWjAjMSEwHwYDVQQDDBhsYXNlcmRpc2Mg
dGVzdCBhZGRyZXNzZXMwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQC/
PMKhhqtUn+YRsCzcP915SpBD+2iiaXqK15z6kU79KPaETjU/DEgKXpjLUhmn2l1m
RlHHzk411OBg8wKoATpkhMcZ9sKXKI1u6F8dMTU5MMLrVL6ND8oQC6l5qLdbjTs4
Q/p1tqySKfwcfJgkXo9MhFnMa7Qy2Q16XnYNSURuRHTbbIRYzs3rTaA7BrSvKcXr
n6iM6DlYZitZMJ2O9vE4qj4/TeStEBCzvn5B0FVV1orVb0dEoUXY8PrbXGN7JGEf
+ePPclocikebUkZrzIouPPm9fGJ3prIngMMXXvmhOg01WHRR+uiFaM4oYlauoUvu
yisVjrQuQZvCwjmNzPcfAgMBAAGjYDBeMB0GA1UdDgQWBBQ+NELPaWLZMUjtPfSg
8ksH3mvqsTAPBgNVHRMBAf8EBTADAQH/MCwGA1UdEQQlMCOHBH8AAAGHEAAAAAAA
AAAAAAAAAAAAAAGCCWxvY2FsaG9zdDANBgkqhkiG9w0BAQsFAAOCAQEAiuIgwYXl
12ZJ93ZmaRs915fNbs/Nm3gfuMjb6UDT1a9AFjYV2zB3496obGEPeaafT13Ac4X7
qu2f3rLEtWD2avNpBVyX1Li9dS+a+6kMBF5cwY1uuCcdcvtuyPk8bVAbyWQZc4sM
4eYJDTgQa/UXLZJTsOpjSwr/6PCZQ2QLrmu4x1LgxS0IJZQgglNr9Qqlq+T8h4Em
f/+ohcFpXPxkClwrQ/z4gKd1HpokqZDuswIj1EKO6nIGQ/A9syO3IHIAeTySpGTc
NwnKrRwhnUfevoy3QPAcxQ1UtdogQBkUQpYmcf7WFqcYIwPNelfJUCQ+K0V8vMuu
kftDZNHnZNtvNw==
-----END CERTIFICATE-----""".getBytes(UTF_8)))

  private def address(s: String): IpAddress = IpAddress.fromString(s).get

  test("the host is sent as the server name, and the certificate checked against it unless told otherwise") {
    val checked = TlsParameters("redis.example.com", checkServerName = true)
    assertEquals(checked.serverNames, Some(List(new SNIHostName("redis.example.com"))))
    assertEquals(checked.endpointIdentificationAlgorithm, Some("HTTPS"))
    assertEquals(TlsParameters("redis.example.com", checkServerName = false).endpointIdentificationAlgorithm, None)
  }

  test("a host ending with a dot is sent as the server name without it") {
    assertEquals(TlsParameters("redis.example.com.", checkServerName = true).serverNames, Some(List(new SNIHostName("redis.example.com"))))
  }

  test("an address, IPv4 or IPv6, isn't sent as the server name, and the JVM isn't asked to check the certificate against it") {
    List("10.0.0.1", "::1", "fe80::1").foreach { host =>
      val parameters = TlsParameters(host, checkServerName = true)
      assertEquals(parameters.serverNames, Some(Nil), host)
      assertEquals(parameters.endpointIdentificationAlgorithm, None, host)
    }
  }

  test("a certificate is for an address when it has it among its IP addresses, however the address is written") {
    assertEquals(TlsParameters.checkAddress(address("127.0.0.1"), Array(forAddresses)), Right(()))
    assertEquals(TlsParameters.checkAddress(address("0:0:0:0:0:0:0:1"), Array(forAddresses)), Right(()))
    assertEquals(TlsParameters.checkAddress(address("::1"), Array(forAddresses)), Right(()))
    assert(TlsParameters.checkAddress(address("10.0.0.1"), Array(forAddresses)).isLeft)
    assert(TlsParameters.checkAddress(address("127.0.0.1"), Array.empty).isLeft)
  }
}
