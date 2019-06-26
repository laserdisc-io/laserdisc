package laserdisc
package protocol

final class HyperLogLogPSpec extends HyperLogLogExtPSpec {

  "The HyperLogLog protocol" when {

    "using pfadd" should {

      "roundtrip successfully" when {
        "given key and elements" in forAll("key", "elements", "return value") { (k: Key, es: OneOrMoreKeys, b: Boolean) =>
          val protocol = pfadd(k, es)

          protocol.encode shouldBe Arr(Bulk("PFADD") :: Bulk(k) :: es.value.map(Bulk(_)))
          protocol.decode(bool2Num(b)).right.value shouldBe b
        }
      }
    }

    "using pfcount" should {

      "roundtrip successfully" when {
        "given keys" in forAll("keys", "return value") { (ks: OneOrMoreKeys, nni: NonNegInt) =>
          val protocol = pfcount(ks)

          protocol.encode shouldBe Arr(Bulk("PFCOUNT") :: ks.value.map(Bulk(_)))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
      }
    }

    "using pfmerge" should {

      "roundtrip successfully" when {
        "given two or more source keys and one destination key" in forAll("sourcekeys", "destinationkey") { (sks: TwoOrMoreKeys, dk: Key) =>
          val protocol = pfmerge(sks, dk)

          protocol.encode shouldBe Arr(Bulk("PFMERGE") :: Bulk(dk) :: sks.value.map(Bulk(_)))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
      }
    }
  }
}
