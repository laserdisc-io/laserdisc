package laserdisc

final class RefinedTypesSpec extends BaseSpec {
  "ConnectionName" should {
    "fail to compile" when {
      "given an empty String" in {
        """ConnectionName("")""" shouldNot compile
      }
      "given a space" in {
        """ConnectionName(" ")""" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided non literal cases of Strings that contain spaces" in forAll(stringsWithSpacesGen) { s =>
        whenever(!connectionNameIsValid(s)) {
          an[IllegalArgumentException] should be thrownBy ConnectionName.unsafeFrom(s)
        }
      }
    }

    "compile" when {
      "given non empty String with no spaces" in {
        """ConnectionName("a")""" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of non empty Strings with no spaces" in forAll(connectionNameGen) { s =>
        whenever(connectionNameIsValid(s)) {
          ConnectionName.from(s) onRight (_.value shouldBe s)
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
      "provided non literal cases of out of range Ints" in forAll { (i: Int) =>
        whenever(!dbIndexIsValid(i)) {
          an[IllegalArgumentException] should be thrownBy DbIndex.unsafeFrom(i)
        }
      }
    }

    "compile" when {
      "given in range Int" in {
        "DbIndex(0)" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of in range Ints" in forAll(dbIndexGen) { i =>
        whenever(dbIndexIsValid(i)) {
          DbIndex.from(i) onRight (_.value shouldBe i)
        }
      }
    }
  }

  "GeoHash" should {
    "fail to compile" when {
      "given a non conformant String (length < 11)" in {
        """GeoHash("abcdefghij")""" shouldNot compile
      }
      "given a non conformant String (length > 11)" in {
        """GeoHash("abcdefghijkl")""" shouldNot compile
      }
      "given a non conformant String (uppercase)" in {
        """GeoHash("abCdefghijk")""" shouldNot compile
      }
      "given a non conformant String (invalid chars)" in {
        """GeoHash("abcd&fghijk")""" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided non literal cases of non conformant Strings" in forAll { (s: String) =>
        whenever(!geoHashIsValid(s)) {
          an[IllegalArgumentException] should be thrownBy GeoHash.unsafeFrom(s)
        }
      }
    }

    "compile" when {
      "given conformant String" in {
        """GeoHash("abcd3fgh1jk")""" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of conformant Strings" in forAll(geoHashGen) { s =>
        whenever(geoHashIsValid(s)) {
          GeoHash.from(s) onRight (_.value shouldBe s)
        }
      }
    }
  }

  "GlobPattern" should {
    "fail to compile" when {
      "given an empty String" in {
        """GlobPattern("")""" shouldNot compile
      }
      "given a non conformant String" in {
        """GlobPattern("!")""" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided non literal cases of non conformant Strings" in forAll { (s: String) =>
        whenever(!globPatternIsValid(s)) {
          an[IllegalArgumentException] should be thrownBy GlobPattern.unsafeFrom(s)
        }
      }
    }

    "compile" when {
      "given conformant String" in {
        """GlobPattern("abc*fg?1jk")""" should compile
        """GlobPattern("a[bc*]fg?1jk")""" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of conformant Strings" in forAll(globPatternGen) { s =>
        whenever(globPatternIsValid(s)) {
          GlobPattern.from(s) onRight (_.value shouldBe s)
        }
      }
    }
  }

  "Host" should {
    "fail to compile" when {
      "given an empty String" in {
        """Host("")""" shouldNot compile
      }
      "given a dash-ending hostname (RFC-1123)" in {
        """Host("A0c-")""" shouldNot compile
      }
      "given a dash-beginning hostname (RFC-1123)" in {
        """Host("-A0c")""" shouldNot compile
      }
      "given a hostname label whose length is > 63 (RFC-1123)" in {
        """Host("o123456701234567012345670123456701234567012345670123456701234567")""" shouldNot compile
      }
      "given a hostname whose length is > 255 (RFC-1123)" in {
        """Host(
             "o12345670123456701234567012345670123456701234567012345670123456" +
             ".o12345670123456701234567012345670123456701234567012345670123456" +
             ".o12345670123456701234567012345670123456701234567012345670123456" +
             ".o12345670123456701234567012345670123456701234567012345670123456" +
             ".a"
           )""" shouldNot compile
      }
      "given an invalid (non-private) IP address" in {
        """Host("1.1.1.1")""" shouldNot compile
      }
    }

    "compile" when {
      "given all NICs (0.0.0.0)" in {
        """Host("0.0.0.0")""" should compile
      }
      "given loopback address (127.0.0.1)" in {
        """Host("127.0.0.1")""" should compile
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
      "given an empty String" in {
        """Key("")""" shouldNot compile
      }
    }

    "compile" when {
      "given a non empty String" in {
        """Key("a")""" should compile
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
      "provided non literal cases of out of range Doubles (d < -85.05112878D | d > 85.05112878D)" in forAll { (d: Double) =>
        whenever(!latitudeIsValid(d)) {
          an[IllegalArgumentException] should be thrownBy Latitude.unsafeFrom(d)
        }
      }
    }

    "compile" when {
      "given edge cases (-85.05112878D)" in {
        "Latitude(-85.05112878D)" should compile
      }
      "given edge cases (85.05112878D)" in {
        "Latitude(85.05112878D)" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of in range Doubles (-85.05112878D <= d <= 85.05112878D)" in forAll(latitudeGen) { d =>
        whenever(latitudeIsValid(d)) {
          Latitude.from(d) onRight (_.value shouldBe d)
        }
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
      "provided non literal cases of out of range Doubles (d < -180.0D | d > 180.0D)" in forAll { (d: Double) =>
        whenever(!longitudeIsValid(d)) {
          an[IllegalArgumentException] should be thrownBy Longitude.unsafeFrom(d)
        }
      }
    }

    "compile" when {
      "given edge cases (-180.0D)" in {
        "Longitude(-180.0D)" should compile
      }
      "given edge cases (180.0D)" in {
        "Longitude(180.0D)" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of in range Doubles (-180.0D <= d <= 180.0D)" in forAll(longitudeGen) { d =>
        whenever(longitudeIsValid(d)) {
          Longitude.from(d) onRight (_.value shouldBe d)
        }
      }
    }
  }

  "NodeId" should {
    "fail to compile" when {
      "given a non conformant String (length < 40)" in {
        """NodeId("0123456789abcdef0123456789abcdef0123456")""" shouldNot compile
      }
      "given a non conformant String (length > 40)" in {
        """NodeId("0123456789abcdef0123456789abcdef012345678")""" shouldNot compile
      }
      "given a non conformant String (uppercase)" in {
        """NodeId("0123456789abcdEf0123456789abcdef01234567")""" shouldNot compile
      }
      "given a non conformant String (invalid chars)" in {
        """NodeId("0123456789abcd&f0123456789abcdef01234567&fghijk")""" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided non literal cases of non conformant Strings" in forAll { (s: String) =>
        whenever(!nodeIdIsValid(s)) {
          an[IllegalArgumentException] should be thrownBy NodeId.unsafeFrom(s)
        }
      }
    }

    "compile" when {
      "given conformant String" in {
        """NodeId("0123456789abcdef0123456789abcdef01234567")""" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of conformant Strings" in forAll(nodeIdGen) { s =>
        whenever(nodeIdIsValid(s)) {
          NodeId.from(s) onRight (_.value shouldBe s)
        }
      }
    }
  }

  "NonNegDouble" should {
    "fail to compile" when {
      "given out of range Double (< 0.0D)" in {
        "NonNegDouble(-0.00000001D)" shouldNot compile
      }
      "given NaN Double" in {
        "NonNegDouble(Double.NaN)" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided non literal cases of out of range Doubles (d < 0.0D)" in forAll { (d: Double) =>
        whenever(!nonNegDoubleIsValid(d)) {
          an[IllegalArgumentException] should be thrownBy NonNegDouble.unsafeFrom(d)
        }
      }
    }

    "compile" when {
      "given edge cases (0.0D)" in {
        "NonNegDouble(0.0D)" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of in range Doubles (d >= 0.0D)" in forAll(nonNegDoubleGen) { d =>
        whenever(nonNegDoubleIsValid(d)) {
          NonNegDouble.from(d) onRight (_.value shouldBe d)
        }
      }
    }
  }

  "NonNegInt" should {
    "fail to compile" when {
      "given out of range Ints (< 0)" in {
        "NonNegInt(-1)" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided non literal cases of out of range Ints (i < 0)" in forAll { (i: Int) =>
        whenever(!nonNegIntIsValid(i)) {
          an[IllegalArgumentException] should be thrownBy NonNegInt.unsafeFrom(i)
        }
      }
    }

    "compile" when {
      "given edge cases (0)" in {
        "NonNegInt(0)" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of in range Ints (i > 0)" in forAll(nonNegIntGen) { i =>
        whenever(nonNegIntIsValid(i)) {
          NonNegInt.from(i) onRight (_.value shouldBe i)
        }
      }
    }
  }

  "NonNegLong" should {
    "fail to compile" when {
      "given out of range Longs (< 0L)" in {
        "NonNegLong(-1L)" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided non literal cases of out of range Longs (l < 0L)" in forAll { (l: Long) =>
        whenever(!nonNegLongIsValid(l)) {
          an[IllegalArgumentException] should be thrownBy NonNegLong.unsafeFrom(l)
        }
      }
    }

    "compile" when {
      "given edge cases (0L)" in {
        "NonNegLong(0L)" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of in range Longs (l > 0L)" in forAll(nonNegLongGen) { l =>
        whenever(nonNegLongIsValid(l)) {
          NonNegLong.from(l) onRight (_.value shouldBe l)
        }
      }
    }
  }

  "NonZeroDouble" should {
    "fail to compile" when {
      "given 0.0D" in {
        "NonZeroDouble(0.0D)" shouldNot compile
      }
      "given NaN" in {
        "NonZeroDouble(Double.NaN)" shouldNot compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of valid Doubles (d != 0.0D)" in forAll(nonZeroDoubleGen) { d =>
        whenever(nonZeroDoubleIsValid(d)) {
          NonZeroDouble.from(d) onRight (_.value shouldBe d)
        }
      }
    }
  }

  "NonZeroInt" should {
    "fail to compile" when {
      "given 0" in {
        "NonZeroInt(0)" shouldNot compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of valid Ints (i != 0)" in forAll(nonZeroIntGen) { i =>
        whenever(nonZeroIntIsValid(i)) {
          NonZeroInt.from(i) onRight (_.value shouldBe i)
        }
      }
    }
  }

  "NonZeroLong" should {
    "fail to compile" when {
      "given 0L" in {
        "NonZeroLong(0L)" shouldNot compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of valid Longs (l != 0L)" in forAll(nonZeroLongGen) { l =>
        whenever(nonZeroLongIsValid(l)) {
          NonZeroLong.from(l) onRight (_.value shouldBe l)
        }
      }
    }
  }

  "OneOrMore" should {
    "fail at runtime" when {
      "provided empty List" in {
        an[IllegalArgumentException] should be thrownBy OneOrMore.unsafeFrom(List.empty[Int])
      }
    }

    "refine correctly" when {
      "provided non literal cases of non empty Lists (length > 0)" in forAll { (is: List[Int]) =>
        whenever(is.nonEmpty) {
          OneOrMore.from(is) onRight (_.value shouldBe is)
        }
      }
    }
  }

  "OneOrMoreKeys" should {
    "fail to compile" when {
      "given non literal empty List" in {
        "OneOrMoreKeys(List.empty)" shouldNot compile
      }
      "given non literal non empty List" in {
        """OneOrMoreKeys(List(Key("a")))""" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided empty List" in {
        an[IllegalArgumentException] should be thrownBy OneOrMoreKeys.unsafeFrom(List.empty)
      }
    }

    "refine correctly" when {
      "provided non literal cases of non empty Lists (length > 0)" in forAll { (ks: List[Key]) =>
        whenever(ks.nonEmpty) {
          OneOrMoreKeys.from(ks) onRight (_.value shouldBe ks)
        }
      }
    }
  }

  "RangeOffset" should {
    "fail to compile" when {
      "given out of range Int (< 0)" in {
        "RangeOffset(-1)" shouldNot compile
      }
      "given out of range Int (> 536870911)" in {
        "RangeOffset(536870912)" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided non literal cases of out of range Ints (i < 0 | i > 536870911)" in forAll { (i: Int) =>
        whenever(!rangeOffsetIsValid(i)) {
          an[IllegalArgumentException] should be thrownBy RangeOffset.unsafeFrom(i)
        }
      }
    }

    "compile" when {
      "given edge cases (0)" in {
        "RangeOffset(0)" should compile
      }
      "given edge cases (536870911)" in {
        "RangeOffset(536870911)" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of in range Ints (0 <= i <= 536870911)" in forAll(rangeOffsetGen) { i =>
        whenever(rangeOffsetIsValid(i)) {
          RangeOffset.from(i) onRight (_.value shouldBe i)
        }
      }
    }
  }

  "Slot" should {
    "fail to compile" when {
      "given out of range Int (< 0)" in {
        "Slot(-1)" shouldNot compile
      }
      "given out of range Int (> 16383)" in {
        "Slot(16384)" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided non literal cases of out of range Ints (i < 0 | i > 16383)" in forAll { (i: Int) =>
        whenever(!slotIsValid(i)) {
          an[IllegalArgumentException] should be thrownBy Slot.unsafeFrom(i)
        }
      }
    }

    "compile" when {
      "given edge cases (0)" in {
        "Slot(0)" should compile
      }
      "given edge cases (16383)" in {
        "Slot(16383)" should compile
      }
    }
  }

  "StringLength" should {
    "fail to compile" when {
      "given out of range Long (< 0L)" in {
        "StringLength(-1L)" shouldNot compile
      }
      "given out of range Long (> 4294967295L)" in {
        "StringLength(4294967296L)" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided non literal cases of out of range Longs (l < 0L | l > 4294967295L)" in forAll { (l: Long) =>
        whenever(!stringLengthIsValid(l)) {
          an[IllegalArgumentException] should be thrownBy StringLength.unsafeFrom(l)
        }
      }
    }

    "compile" when {
      "given edge cases (0L)" in {
        "StringLength(0L)" should compile
      }
      "given edge cases (4294967295L)" in {
        "StringLength(4294967295L)" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of in range Longs (0L <= l <= 4294967295L)" in forAll(stringLengthGen) { l =>
        whenever(stringLengthIsValid(l)) {
          StringLength.from(l) onRight (_.value shouldBe l)
        }
      }
    }
  }

  "TwoOrMoreKeys" should {
    "fail to compile" when {
      "given non literal empty List" in {
        "TwoOrMoreKeys(List.empty)" shouldNot compile
      }
      "given non literal single element List" in {
        """TwoOrMoreKeys(List(Key("a")))""" shouldNot compile
      }
      "given non literal List of two elements" in {
        """TwoOrMoreKeys(List(Key("a"), Key("b")))""" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided empty List" in {
        an[IllegalArgumentException] should be thrownBy TwoOrMoreKeys.unsafeFrom(List.empty)
      }
      "provided single element List" in {
        an[IllegalArgumentException] should be thrownBy TwoOrMoreKeys.unsafeFrom(List(Key("a")))
      }
    }

    "refine correctly" when {
      "provided non literal cases of Lists of length > 1" in forAll { (ks: List[Key]) =>
        whenever(ks.size > 1) {
          TwoOrMoreKeys.from(ks) onRight (_.value shouldBe ks)
        }
      }
    }
  }

  "TwoOrMoreWeightedKeys" should {
    "fail to compile" when {
      "given non literal empty List" in {
        "TwoOrMoreWeightedKeys(List.empty)" shouldNot compile
      }
      "given non literal single element List" in {
        """TwoOrMoreWeightedKeys(List(Key("a") -> ValidDouble(42.0D)))""" shouldNot compile
      }
      "given non literal List of two elements" in {
        """TwoOrMoreWeightedKeys(List(Key("a") -> ValidDouble(42.0D), Key("b") -> ValidDouble(23.0D)))""" shouldNot compile
      }
    }

    "fail at runtime" when {
      "provided empty List" in {
        an[IllegalArgumentException] should be thrownBy TwoOrMoreWeightedKeys.unsafeFrom(List.empty)
      }
      "provided single element List" in {
        an[IllegalArgumentException] should be thrownBy TwoOrMoreWeightedKeys.unsafeFrom(List(Key("a") -> ValidDouble(42.0d)))
      }
    }

    "refine correctly" when {
      "provided non literal cases of Lists of length > 1" in forAll { (kvds: List[(Key, ValidDouble)]) =>
        whenever(kvds.size > 1) {
          TwoOrMoreWeightedKeys.from(kvds) onRight (_.value shouldBe kvds)
        }
      }
    }
  }

  "ValidDouble" should {
    "fail to compile" when {
      "given Double.NaN" in {
        "ValidDouble(Double.NaN)" shouldNot compile
      }
    }

    "compile" when {
      "given edge cases (-1.7976931348623157E308) -> can't use Double.MinValue as not a literal" in {
        "ValidDouble(-1.7976931348623157E308)" should compile
      }
      "given edge cases (Double.MaxValue)" in {
        "ValidDouble(Double.MaxValue)" should compile
      }
    }

    "refine correctly" when {
      "provided non literal cases of valid Doubles (d != Double.NaN)" in forAll { (d: Double) =>
        whenever(validDoubleIsValid(d)) {
          ValidDouble.from(d) onRight (_.value shouldBe d)
        }
      }
    }
  }
}
