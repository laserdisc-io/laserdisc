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

  test("IpAddress takes an IPv6 address only as a server writes it, as a client's address written otherwise matches no client") {
    List("::", "::1", "1::", "fe80::1:2", "2001:db8:0:1:1:1:1:1", "2001:db8::1:0:0:1", "::ffff:10.0.0.1", "::ffff:0.0.0.0").foreach { a =>
      assert(IpAddress.from(a).isRight, a)
    }
    List(
      "0:0:0:0:0:0:0:1",     // the zeros not shortened
      "::0:1",               // not all of them shortened
      "2001:DB8::1",         // upper case
      "2001:0db8::1",        // a leading zero
      "2001:db8::1:1:1:1:1", // a single zero group shortened
      "2001:db8:0:0:1::1",   // the second run of zeros shortened, not the first as long
      "::ffff:a00:1",        // an IPv4 address in hexadecimal
      "fe80::1%eth0"         // a zone id, which a server doesn't write
    ).foreach(a => assert(IpAddress.from(a).isLeft, a))
    assert(compileErrors("""IpAddress("0:0:0:0:0:0:0:1")""").nonEmpty)
  }

  test("IpAddress fails to compile given a hostname, which a server never matches a client's address against") {
    assert(compileErrors("""IpAddress("redis.example")""").nonEmpty)
    assert(compileErrors("""IpAddress("localhost")""").nonEmpty)
    assert(compileErrors("""IpAddress("")""").nonEmpty)
  }

  property("IpAddress refines the IPv4 addresses a Host takes, its IPv6 ones written as a server writes them, and none of its hostnames") {
    forAll(hostGen) { h =>
      val ipv6 = h.value.contains(':')
      assertEquals(
        IpAddress.from(h.value).isRight,
        h.value.forall(c => c.isDigit || c == '.') || (ipv6 && HostCheck.canonicalIPv6(h.value).contains(h.value))
      )
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
