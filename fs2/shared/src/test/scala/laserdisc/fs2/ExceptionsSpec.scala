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

import munit.FunSuite

final class ExceptionsSpec extends FunSuite {
  test("an error without a message of its own says why with its cause's, as TLS errors on Node.js have none, or else what it is") {
    val withCause = new RuntimeException(null: String, new RuntimeException("self-signed certificate in certificate chain"))
    assertEquals(ServerUnavailable(withCause).getMessage, "No server available: self-signed certificate in certificate chain")
    assertEquals(ServerUnavailable(new IllegalStateException()).getMessage, "No server available: java.lang.IllegalStateException")
    assertEquals(ServerUnavailable(new RuntimeException("refused")).getMessage, "No server available: refused")
    assertEquals(
      ConnectionSetupRefused(RedisAddress(Host.unsafeFrom("localhost"), Port.unsafeFrom(6379)), withCause).getMessage,
      "Server localhost:6379 refused to set up the connection: self-signed certificate in certificate chain"
    )
  }
}
