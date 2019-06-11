package laserdisc

import org.scalacheck.Gen
import org.scalacheck.Gen._

final class RefinedTypesSpec extends BaseSpec {

  final val geoHashGen: Gen[String]   = listOfN(11, frequency(1 -> numChar, 9 -> alphaLowerChar)).map(_.mkString)
  final val latitudeGen: Gen[Double]  = choose(-85.05112878D, 85.05112878D)
  final val longitudeGen: Gen[Double] = choose(-180.0D, 180.0D)
  final val stringsWithHighProbabilityOfSpacesGen: Gen[String] = {
    val validRangesInclusive = List[(Char, Char)](
      (0x0000, 0x001F),
      (0x0021, 0xD7FF),
      (0xE000, 0xFFFD)
    )
    val allWeightedEqualChars = validRangesInclusive.map { case (first, last) => (1, choose[Char](first, last)) }
    listOf(frequency(((100 -> const(' ')) :: allWeightedEqualChars): _*)).map(_.mkString)
  }

  "ConnectionName" should {

    "fail to compile" when {
      "given empty String" in {
        """ConnectionName("")""" shouldNot compile
      }
      "given a String that contains a space" in {
        """ConnectionName(" ")""" shouldNot compile
      }
    }

    "fail at runtime" when {
      "providing non literal cases" in forAll(stringsWithHighProbabilityOfSpacesGen) { s =>
        whenever(s.contains(" ")) {
          an[IllegalArgumentException] should be thrownBy ConnectionName.unsafeFrom(s)
        }
      }
    }
  }

  "DbIndex" should {

    "fail to compile" when {
      "given out of range Int (< 0)" in {
        "DbIndex(-1)" shouldNot compile
      }
      "given out of range Int (> 15)" in {
        "DbIndex(16)" shouldNot compile
      }
    }

    "fail at runtime" when {
      "providing non literal cases" in forAll { (i: Int) =>
        whenever(i < 0 || i > 15) {
          an[IllegalArgumentException] should be thrownBy DbIndex.unsafeFrom(i)
        }
      }
    }
  }

  "GeoHash" should {

    "fail to compile" when {
      "given empty String" in {
        """GeoHash("")""" shouldNot compile
      }
      "given a non conformant String" in {
        """GeoHash("abcdefghijkl")""" shouldNot compile
      }
    }

    "fail at runtime" when {
      "providing non literal cases" in forAll { (s: String) =>
        whenever(!s.matches("[a-z0-9]{11}")) {
          an[IllegalArgumentException] should be thrownBy GeoHash.unsafeFrom(s)
        }
      }
    }

    "refine correctly" when {
      "provided valid String" in forAll(geoHashGen) { s =>
        whenever(!s.matches("[a-z0-9]{11}")) {
          GeoHash.from(s).right.value shouldBe s
        }
      }
    }
  }

  "GlobPattern" should {

    "fail to compile" when {
      "given empty String" in {
        """GlobPattern("")""" shouldNot compile
      }
      "given a non conformant String" in {
        """GlobPattern("!")""" shouldNot compile
      }
    }

    "fail at runtime" when {
      "providing non literal cases" in forAll { (s: String) =>
        whenever(!s.matches("(\\\\[?[\\\\w\\\\*\\\\?]+\\\\]?)+")) {
          an[IllegalArgumentException] should be thrownBy GlobPattern.unsafeFrom(s)
        }
      }
    }
  }

  "Host" should {

    "fail to compile" when {
      "given empty String" in {
        """Host("")""" shouldNot compile
      }
      "given dash-ending hostname (RFC-1123)" in {
        """Host("A0c-")""" shouldNot compile
      }
      "given dash-beginning hostname (RFC-1123)" in {
        """Host("-A0c")""" shouldNot compile
      }
      "given too long (>63) hostname label (RFC-1123)" in {
        """Host("o123456701234567012345670123456701234567012345670123456701234567")""" shouldNot compile
      }
      "given too long (>255) hostname (RFC-1123)" in {
        """Host(
             "o12345670123456701234567012345670123456701234567012345670123456" +
             ".o12345670123456701234567012345670123456701234567012345670123456" +
             ".o12345670123456701234567012345670123456701234567012345670123456" +
             ".o12345670123456701234567012345670123456701234567012345670123456" +
             ".a"
           )""" shouldNot compile
      }
      "given invalid (non-private) IP address" in {
        """Host("1.1.1.1")""" shouldNot compile
      }
    }

    "compile" when {
      "given all NICs (0.0.0.0)" in {
        """Host("0.0.0.0")""" should compile
      }
      "given localhst (127.0.0.1)" in {
        """Host("localhost")""" should compile
      }
      "given localhost (RFC-1123)" in {
        """Host("localhost")""" should compile
      }
      "given domain.local (RFC-1123)" in {
        """Host("domain.local")""" should compile
      }
      "given any digit hostname (RFC-1123)" in {
        """Host("01234")""" should compile
      }
      "given any dash inside hostname (RFC-1123)" in {
        """Host("01234-abc")""" should compile
      }
      "given 10. IPv4 (RFC-1918)" in {
        """Host("10.1.2.3")""" should compile
      }
      "given 172.(15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31). IPv4 (RFC-1918)" in {
        """Host("10.15.1.2")""" should compile
        """Host("10.16.1.2")""" should compile
        """Host("10.17.1.2")""" should compile
        """Host("10.18.1.2")""" should compile
        """Host("10.19.1.2")""" should compile
        """Host("10.20.1.2")""" should compile
        """Host("10.21.1.2")""" should compile
        """Host("10.22.1.2")""" should compile
        """Host("10.23.1.2")""" should compile
        """Host("10.24.1.2")""" should compile
        """Host("10.25.1.2")""" should compile
        """Host("10.26.1.2")""" should compile
        """Host("10.27.1.2")""" should compile
        """Host("10.28.1.2")""" should compile
        """Host("10.29.1.2")""" should compile
        """Host("10.30.1.2")""" should compile
        """Host("10.31.1.2")""" should compile
      }
      "given 192.168. IPv4 (RFC-1918)" in {
        """Host("192.168.1.2")""" should compile
      }
      "given 192.0.2. IPv4 (RFC-5737)" in {
        """Host("192.0.2.1")""" should compile
      }
      "given 198.51.100. IPv4 (RFC-5737)" in {
        """Host("198.51.100.1")""" should compile
      }
      "given 203.0.113. IPv4 (RFC-5737)" in {
        """Host("203.0.113.1")""" should compile
      }
      "given 169.254. IPv4 (RFC-3927)" in {
        """Host("169.254.1.2")""" should compile
      }
      "given 198.(18|19). IPv4 (RFC-2544)" in {
        """Host("198.18.1.2")""" should compile
        """Host("198.19.1.2")""" should compile
      }
    }
  }

  "Key" should {

    "fail to compile" when {
      "given empty String" in {
        """Key("")""" shouldNot compile
      }
    }

    "compile" when {
      "given a non empty String" in {
        """Key("a")""" should compile
      }
    }

    "refine correctly" when {
      "given correct non literal cases" in forAll { (s: String) =>
        whenever(s.nonEmpty) {
          Key.from(s).right.value.value shouldBe s
        }
      }
    }
  }

  "Latitude" should {

    "fail to compile" when {
      "given out of range Double (< -85.05112878D)" in {
        "Latitude(-85.05112879D)" shouldNot compile
      }
      "given out of range Double (> 85.05112878D)" in {
        "Latitude(85.05112879D)" shouldNot compile
      }
    }

    "fail at runtime" when {
      "given out of range non literal cases (d < -85.05112878D | d > 85.05112878D)" in forAll { (d: Double) =>
        whenever(d < -85.05112878D || d > 85.05112878D) {
          an[IllegalArgumentException] should be thrownBy Latitude.unsafeFrom(d)
        }
      }
    }

    "compile" when {
      "given correct literal edge cases (-85.05112878D)" in {
        "Latitude(-85.05112878D)" should compile
      }
      "given correct literal edge cases (85.05112878D)" in {
        "Latitude(85.05112878D)" should compile
      }
    }

    "refine correctly" when {
      "given correct non literal cases (-85.05112878D <= d <= 85.05112878D)" in forAll(latitudeGen) { d =>
        Latitude.from(d).right.value.value shouldBe d
      }
    }
  }

  "Longitude" should {

    "fail to compile" when {
      "given out of range Double (< -180.0D)" in {
        "Longitude(-180.00000001D)" shouldNot compile
      }
      "given out of range Double (> 180.0D)" in {
        "Longitude(180.00000001D)" shouldNot compile
      }
    }

    "fail at runtime" when {
      "given out of range non literal cases (d < -180.0D | d > 180.0D)" in forAll { (d: Double) =>
        whenever(d < -180.0D || d > 180.0D) {
          an[IllegalArgumentException] should be thrownBy Longitude.unsafeFrom(d)
        }
      }
    }

    "compile" when {
      "given correct literal edge cases (-180.0D)" in {
        "Longitude(-180.0D)" should compile
      }
      "given correct literal edge cases (180.0D)" in {
        "Longitude(180.0D)" should compile
      }
    }

    "refine correctly" when {
      "given correct non literal cases (-180.0D <= d <= 180.0D)" in forAll(longitudeGen) { d =>
        Longitude.from(d).right.value.value shouldBe d
      }
    }
  }
}
