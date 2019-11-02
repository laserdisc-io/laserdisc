package laserdisc
package protocol

final class HyperLogLogPSpec extends HyperLogLogExtPSpec {
  "The HyperLogLog protocol" when {
    "using pfadd" should {
      "roundtrip successfully" when {
        "given key and elements" in forAll("key", "elements", "added") { (k: Key, es: OneOrMoreKeys, b: Boolean) =>
          val protocol = pfadd(k, es)

          protocol.encode shouldBe Arr(Bulk("PFADD") :: Bulk(k) :: es.value.map(Bulk(_)))
          protocol.decode(boolToNum(b)) onRight (_ shouldBe b)
        }
      }
    }

    "using pfcount" should {
      "roundtrip successfully" when {
        "given keys" in forAll("keys", "count") { (ks: OneOrMoreKeys, nni: NonNegInt) =>
          val protocol = pfcount(ks)

          protocol.encode shouldBe Arr(Bulk("PFCOUNT") :: ks.value.map(Bulk(_)))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using pfmerge" should {
      "roundtrip successfully" when {
        "given two or more source keys and a destination key" in forAll("source keys", "destination key") { (sks: TwoOrMoreKeys, dk: Key) =>
          val protocol = pfmerge(sks, dk)

          protocol.encode shouldBe Arr(Bulk("PFMERGE") :: Bulk(dk) :: sks.value.map(Bulk(_)))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }
  }
}
