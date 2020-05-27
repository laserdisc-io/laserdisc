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

  test("Host fails to compile given a hostname whose length is > 255 (RFC-1123)") {
    assert(
      !compileErrors(
        """Host(
        "o12345670123456701234567012345670123456701234567012345670123456" +
        ".o12345670123456701234567012345670123456701234567012345670123456" +
        ".o12345670123456701234567012345670123456701234567012345670123456" +
        ".o12345670123456701234567012345670123456701234567012345670123456" +
        ".a"
      )"""
      ).isEmpty
    )
  }

  test("Host fails to compile given an invalid (non-private) IP address") {
    assert(!compileErrors("""Host("1.1.1.1")""").isEmpty)
  }

  test("Host compiles given all NICs (0.0.0.0)") {
    Host("0.0.0.0")
  }
  test("Host compiles given loopback address (127.0.0.1)") {
    Host("127.0.0.1")
  }
  test("Host compiles given localhost (RFC-1123)") {
    Host("localhost")
  }
  test("Host compiles given domain.local (RFC-1123)") {
    Host("domain.local")
  }
  test("Host compiles given any digit hostname (RFC-1123)") {
    Host("01234")
  }
  test("Host compiles given any dash inside hostname (RFC-1123)") {
    Host("01234-abc")
  }
  test("Host compiles given 10. IPv4 (RFC-1918)") {
    Host("10.1.2.3")
  }
  test("Host compiles given 172.(15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31). IPv4 (RFC-1918)") {
    Host("10.15.1.2")
    Host("10.16.1.2")
    Host("10.17.1.2")
    Host("10.18.1.2")
    Host("10.19.1.2")
    Host("10.20.1.2")
    Host("10.21.1.2")
    Host("10.22.1.2")
    Host("10.23.1.2")
    Host("10.24.1.2")
    Host("10.25.1.2")
    Host("10.26.1.2")
    Host("10.27.1.2")
    Host("10.28.1.2")
    Host("10.29.1.2")
    Host("10.30.1.2")
    Host("10.31.1.2")
  }
  test("Host compiles given 192.168. IPv4 (RFC-1918)") {
    Host("192.168.1.2")
  }
  test("Host compiles given 192.0.2. IPv4 (RFC-5737)") {
    Host("192.0.2.1")
  }
  test("Host compiles given 198.51.100. IPv4 (RFC-5737)") {
    Host("198.51.100.1")
  }
  test("Host compiles given 169.254. IPv4 (RFC-3927)") {
    Host("169.254.1.2")
  }
  test("Host compiles given 198.(18|19). IPv4 (RFC-2544)") {
    Host("198.18.1.2")
    Host("198.19.1.2")
  }
}
