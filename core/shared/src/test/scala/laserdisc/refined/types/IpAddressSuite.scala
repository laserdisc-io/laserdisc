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
package refined.types

import org.scalacheck.Prop.forAll

final class IpAddressSuite extends BaseSpec {
  test("IpAddress compiles given an IPv4 or IPv6 address") {
    assertEquals(IpAddress("10.0.0.1").value, "10.0.0.1")
    assertEquals(IpAddress("::1").value, "::1")
    assertEquals(IpAddress("2001:db8::1").value, "2001:db8::1")
  }

  test("IpAddress fails to compile given a hostname, which a server never matches a client's address against") {
    assert(compileErrors("""IpAddress("redis.example")""").nonEmpty)
    assert(compileErrors("""IpAddress("localhost")""").nonEmpty)
    assert(compileErrors("""IpAddress("")""").nonEmpty)
  }

  property("IpAddress refines the addresses a Host takes, and none of its hostnames") {
    forAll(hostGen) { h =>
      assertEquals(IpAddress.from(h.value).isRight, h.value.forall(c => c.isDigit || c == '.') || h.value.contains(':'))
    }
  }

  test("the commands that match a client's address take an IpAddress, not a Host") {
    assert(compileErrors("""laserdisc.valkey_9_1.servertypes.clientFilter.address(Host("redis.example"), Port(1))""").nonEmpty)
    assert(compileErrors("""laserdisc.valkey_9_1.servertypes.valkeyClientFilter.ip(Host("redis.example"))""").nonEmpty)
    assert(compileErrors("""laserdisc.valkey_9_1.meet(Host("redis.example"), Port(1))""").nonEmpty)
    assert(compileErrors("""laserdisc.dragonfly_2_0.client.killaddress(Host("redis.example"), Port(1))""").nonEmpty)
    assert(compileErrors("""laserdisc.valkey_9_1.meet(IpAddress("10.0.0.1"), Port(1))""").isEmpty)
  }
}
