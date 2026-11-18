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

import munit.FunSuite

final class TlsParametersSpec extends FunSuite {

  test("the host is sent as the server name, without a dot it ends with, and the certificate checked against it unless told otherwise") {
    val checked = TlsParameters("redis.example.com.", checkServerName = true)
    assertEquals(checked.serverName, Some("redis.example.com"))
    assertEquals(checked.verifyHostCallback, None)
    val unchecked = TlsParameters("redis.example.com", checkServerName = false)
    assertEquals(unchecked.serverName, Some("redis.example.com"))
    assert(unchecked.verifyHostCallback.exists(_("other.example.com").unsafeRunSync()))
  }

  test("an address isn't sent as the server name, and the certificate's addresses are checked against it instead") {
    List("127.0.0.1" -> "127.0.0.1", "::1" -> "::1", "0:0:0:0:0:0:0:1" -> "::1").foreach { case (host, inCertificate) =>
      val checked = TlsParameters(host, checkServerName = true)
      assertEquals(checked.serverName, None, host)
      val check = checked.verifyHostCallback.getOrElse(fail(s"no check for $host"))
      assert(check(inCertificate).unsafeRunSync(), host)
      assert(!check("10.0.0.1").unsafeRunSync(), host)
      assert(!check("localhost").unsafeRunSync(), host)
    }
    val unchecked = TlsParameters("127.0.0.1", checkServerName = false)
    assertEquals(unchecked.serverName, None)
    assert(unchecked.verifyHostCallback.exists(_("10.0.0.1").unsafeRunSync()))
  }
}
