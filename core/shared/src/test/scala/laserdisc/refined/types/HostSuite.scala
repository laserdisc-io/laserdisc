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

final class HostSuite extends BaseSpec {
  test("Host fails to compile given an empty String") {
    assert(!compileErrors("""Host("")""").isEmpty)
  }

  test("Host fails to compile given a dash-ending hostname (RFC-1123)") {
    assert(!compileErrors("""Host("A0c-")""").isEmpty)
  }

  test("Host fails to compile given a dash-beginning hostname (RFC-1123)") {
    assert(!compileErrors("""Host("-A0c")""").isEmpty)
  }

  test("Host fails to compile given a hostname label whose length is > 63 (RFC-1123)") {
    assert(!compileErrors("""Host("o123456701234567012345670123456701234567012345670123456701234567")""").isEmpty)
  }

  test("Host fails to compile given a hostname whose length is > 253") {
    assert(
      !compileErrors(
        """Host(
        "o12345670123456701234567012345670123456701234567012345670123456" +
        ".o12345670123456701234567012345670123456701234567012345670123456" +
        ".o12345670123456701234567012345670123456701234567012345670123456" +
        ".o1234567012345670123456701234567012345670123456701234567012345"
      )"""
      ).isEmpty
    )
  }

  test("Host fails to compile given a hostname with an empty label, an underscore or a space") {
    assert(!compileErrors("""Host(".")""").isEmpty)
    assert(!compileErrors("""Host("a..b")""").isEmpty)
    assert(!compileErrors("""Host("a.b..")""").isEmpty)
    assert(!compileErrors("""Host("a_b.example")""").isEmpty)
    assert(!compileErrors("""Host("a b")""").isEmpty)
  }

  test("Host fails to compile given a name whose last label is only digits, as it would be read as an address") {
    assert(!compileErrors("""Host("01234")""").isEmpty)
    assert(!compileErrors("""Host("example.123")""").isEmpty)
    assert(!compileErrors("""Host("256.1.1.1")""").isEmpty)
    assert(!compileErrors("""Host("1.2.3")""").isEmpty)
    assert(!compileErrors("""Host("1.2.3.4.5")""").isEmpty)
  }

  test("Host fails to compile given an IPv4 address with a leading zero") {
    assert(!compileErrors("""Host("010.1.2.3")""").isEmpty)
    assert(!compileErrors("""Host("10.1.2.03")""").isEmpty)
  }

  test("Host fails to compile given an invalid IPv6 address") {
    assert(!compileErrors("""Host(":")""").isEmpty)
    assert(!compileErrors("""Host(":::")""").isEmpty)
    assert(!compileErrors("""Host("1:2:3:4:5:6:7")""").isEmpty)
    assert(!compileErrors("""Host("1:2:3:4:5:6:7:8:9")""").isEmpty)
    assert(!compileErrors("""Host("1:2:3:4:5:6:7:8::")""").isEmpty)
    assert(!compileErrors("""Host("1::2::3")""").isEmpty)
    assert(!compileErrors("""Host("12345::")""").isEmpty)
    assert(!compileErrors("""Host("g::1")""").isEmpty)
    assert(!compileErrors("""Host("1.2.3.4::")""").isEmpty)
    assert(!compileErrors("""Host("::1.2.3.4:5")""").isEmpty)
    assert(!compileErrors("""Host("[::1]")""").isEmpty)
  }

  test("Host fails to compile given an IPv6 address with an empty or invalid zone id") {
    assert(!compileErrors("""Host("fe80::1%")""").isEmpty)
    assert(!compileErrors("""Host("fe80::1%eth 0")""").isEmpty)
    assert(!compileErrors("""Host("fe80::1%eth0%1")""").isEmpty)
  }

  test("Host compiles given localhost (RFC-1123)") {
    Host("localhost")
  }
  test("Host compiles given domain.local (RFC-1123)") {
    Host("domain.local")
  }
  test("Host compiles given a hostname ending with a dot") {
    Host("localhost.")
    Host("redis.example.com.")
  }
  test("Host compiles given labels starting with digits, the last one with something else than digits") {
    Host("01234-abc")
    Host("1-2.example")
    Host("123.example.com")
  }
  test("Host compiles given a hostname whose length is 253") {
    Host(
      "o12345670123456701234567012345670123456701234567012345670123456" +
        ".o12345670123456701234567012345670123456701234567012345670123456" +
        ".o12345670123456701234567012345670123456701234567012345670123456" +
        ".o123456701234567012345670123456701234567012345670123456701234"
    )
  }
  test("Host compiles given any IPv4 address") {
    Host("0.0.0.0")
    Host("127.0.0.1")
    Host("1.1.1.1")
    Host("10.1.2.3")
    Host("172.16.1.2")
    Host("192.168.1.2")
    Host("8.8.8.8")
    Host("255.255.255.255")
  }
  test("Host compiles given any IPv6 address") {
    Host("::")
    Host("::1")
    Host("2001:db8::1")
    Host("2001:DB8::1")
    Host("1:2:3:4:5:6:7:8")
    Host("1:2:3:4:5:6:7::")
    Host("1::8")
    Host("::ffff:192.0.2.1")
    Host("64:ff9b::192.0.2.1")
    Host("1:2:3:4:5:6:192.0.2.1")
  }
  test("Host compiles given an IPv6 address with a zone id") {
    Host("fe80::1%eth0")
    Host("fe80::1%1")
    Host("fe80::1%en0.100")
  }

  test("Host.from takes and refuses the same values at run time") {
    val (taken, refused) = (
      List("localhost", "localhost.", "1.1.1.1", "::1", "fe80::1%eth0", "::ffff:192.0.2.1"),
      List("", "01234", "256.1.1.1", "010.1.2.3", "1::2::3", "fe80::1%", "a_b", "[::1]")
    )
    assertEquals(taken.filterNot(Host.from(_).isRight), Nil)
    assertEquals(refused.filter(Host.from(_).isRight), Nil)
  }
}
