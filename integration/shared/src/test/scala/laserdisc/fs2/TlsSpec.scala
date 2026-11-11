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

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import laserdisc.auto.*
import laserdisc.protocol.Feed.Message
import laserdisc.redis_8_10.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

// against a Redis with TLS on port 6390, whose certificate test-setup/tls/ca.crt signed, for localhost only
final class TlsSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 60.seconds

  private val port = Port(6390)

  // the certificate of test-setup/tls/ca.crt, which made the server's
  private val testCa =
    """-----BEGIN CERTIFICATE-----
MIIDKzCCAhOgAwIBAgIUb/z+hw+vyNGA74AXQmSS77A1qmowDQYJKoZIhvcNAQEL
BQAwHDEaMBgGA1UEAwwRbGFzZXJkaXNjIHRlc3QgQ0EwIBcNMjYxMTExMDUwMTM4
WhgPMjEyNjEwMTgwNTAxMzhaMBwxGjAYBgNVBAMMEWxhc2VyZGlzYyB0ZXN0IENB
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApNeCDeEcfVaej0BK6Rry
rLIk3BPkNNI2C30H8j6jE/c6ANLbtMJl+AqMhpG+WBS1Hks+bqo9dIjEqz+Fk3pF
bonK42JKv1eCIAZ+9cX3HcCko1BEu/ycCHJ5oWf/N9VWzVUZp/h5R0HYNCG47pzH
5lhuFbgCtT0dklMRl+jyRoscQzk1WDnpdNaB8rYYmOwf0eZHUU3Y0T8cn5TZ+ZOZ
VxEYzJqqWE3bIeNPQOkkfcT8OfDOyoZBgNCfvNeuYI9GcnIBcZX2Y44hmm+Sznqh
T1duvwtmwtRwYx08nbM9GuW/ZXBk3Bzwta5E9PLJrr+ceE7PAfSCnoDi9/MREikc
gwIDAQABo2MwYTAdBgNVHQ4EFgQU2uVyCKsQ7O3NwYJ8Ep+G8FQMs1MwHwYDVR0j
BBgwFoAU2uVyCKsQ7O3NwYJ8Ep+G8FQMs1MwDwYDVR0TAQH/BAUwAwEB/zAOBgNV
HQ8BAf8EBAMCAQYwDQYJKoZIhvcNAQELBQADggEBAIdpir985dr9CYRXcdrbypHR
jjFsAcHyVEyv+lgDLGQ9c+Y7Z2jDWC2+UUsgJb50Bi2Eg67QYrypq7kAHHiOpd5s
IC4zYa411kVlZ6Gjy85tlWkDj2OagHRLnx2+af3yywvHV+D8pgQdfckyC1XSAW5L
99/PX9YgfC8MAqA5HkwGMVQY/Yop7CUfH/w/W7XBL0a9CeCMgS6QSZEaHVOJHDpe
cvo9Z1AP/BSe+c3qbPeaw/PDwAzlgudebc8eno4hlv/R8eWf1o1erwH7AayEaTkL
37vc5lMji0ZsMYCV7aA75X1Ujvvp4enY3biNIA2ISLrgo7LWvweC0HL093BLK0s=
-----END CERTIFICATE-----"""

  private val trusted: Resource[IO, Tls[IO]] =
    Resource.eval(IO(assume(TrustedTls.supported, "TLS isn't tested on this platform"))) >> TrustedTls.trusting(testCa)

  // one command at a time: on Node.js 26, fs2 can stall reading replies that arrive together (https://github.com/typelevel/fs2/issues/3793)
  private def roundTrip(client: RedisClient[IO]): IO[Option[String]] =
    client.send(set("tls:k", "v"), 5.seconds).rethrow >> client.send(get[String]("tls:k"), 5.seconds).rethrow

  test("a client sends and reads over TLS, the server's certificate checked") {
    trusted.flatMap(tls => RedisClient[IO].to("localhost", port, tls = Some(tls))).use(roundTrip).assertEquals(Some("v"))
  }

  test("a client doesn't send to a server whose certificate isn't for the host it connected to") {
    trusted.flatMap(tls => RedisClient[IO].to("127.0.0.1", port, tls = Some(tls))).use(roundTrip).attempt.map { outcome =>
      assert(outcome.isLeft, outcome.toString)
    }
  }

  test("a client told not to check the server's name connects to an address its certificate doesn't name") {
    trusted
      .flatMap(tls => RedisClient[IO].to("127.0.0.1", port, tls = Some(tls.copy(checkServerName = false))))
      .use(roundTrip)
      .assertEquals(Some("v"))
  }

  test("a client doesn't send to a server whose certificate it doesn't trust") {
    (trusted >> Tls.system[IO]).flatMap(tls => RedisClient[IO].to("localhost", port, tls = Some(tls))).use(roundTrip).attempt.map {
      outcome => assert(outcome.isLeft, outcome.toString)
    }
  }

  test("a subscriber reads the messages over TLS") {
    trusted
      .use { tls =>
        RedisSubscriber[IO].withTls(tls).to("localhost", port, subscribe[String](OneOrMoreKeys.unsafeFrom(List(Key("tls:c"))))).use {
          messages =>
            RedisClient[IO].to("localhost", port, tls = Some(tls)).use(_.send(publish(Key("tls:c"), "m"))) >>
              messages.take(1).compile.toList
        }
      }
      .assertEquals(List(Right(Message(Key("tls:c"), "m"))))
  }
}
