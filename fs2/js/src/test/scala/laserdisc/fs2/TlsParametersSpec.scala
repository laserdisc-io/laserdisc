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

  test("the host is sent as the server name, without a dot it ends with") {
    assertEquals(TlsParameters("redis.example.com.", checkServerName = true).servername, Some("redis.example.com"))
    assertEquals(TlsParameters("redis.example.com.", checkServerName = false).servername, Some("redis.example.com"))
  }

  test("an address isn't sent as the server name, as TLS doesn't allow one") {
    List("127.0.0.1", "::1").foreach { host =>
      assertEquals(TlsParameters(host, checkServerName = true).servername, None, host)
      assertEquals(TlsParameters(host, checkServerName = false).servername, None, host)
    }
  }
}
