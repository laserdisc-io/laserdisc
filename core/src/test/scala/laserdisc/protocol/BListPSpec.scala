package laserdisc
package protocol

final class BListPSpec extends BListExtPSpec {
  "The Blocking List protocol" when {
    "using blpop" should {
      "fail to compile" when {
        "given one key and timeout but missing read instance" in {
          """blpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))""" shouldNot compile
        }
      }

      "roundtrip successfully" when {
        "given one or more keys and timeout" in forAll("keys", "timeout", "returned value") { (ks: OneOrMoreKeys, nni: NonNegInt, i: Int) =>
          val protocol = blpop[Int](ks, nni)

          protocol.encode shouldBe Arr((Bulk("BLPOP") :: ks.value.map(Bulk(_))) :+ Bulk(nni))
          protocol.decode(Arr(Bulk(ks.value.headOption.value), Bulk(i))) onRight (_.value shouldBe KV(ks.value.headOption.value, i))
        }
        "given one or more keys, timeout and specific read instance" in {
          forAll("keys", "timeout", "returned value") { (ks: OneOrMoreKeys, nni: NonNegInt, i: Int) =>
            val protocol = blpop[Foo](ks, nni)

            protocol.encode shouldBe Arr((Bulk("BLPOP") :: ks.value.map(Bulk(_))) :+ Bulk(nni))
            protocol.decode(Arr(Bulk(ks.value.headOption.value), Bulk(i))) onRight (_.value shouldBe KV(ks.value.headOption.value, Foo(i)))
          }
        }
      }
    }

    "using brpop" should {
      "fail to compile" when {
        "given one key and timeout but missing read instance" in {
          """brpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))""" shouldNot compile
        }
      }

      "roundtrip successfully" when {
        "given one or more keys and timeout" in forAll("keys", "timeout", "returned value") { (ks: OneOrMoreKeys, nni: NonNegInt, i: Int) =>
          val protocol = brpop[Int](ks, nni)

          protocol.encode shouldBe Arr((Bulk("BRPOP") :: ks.value.map(Bulk(_))) :+ Bulk(nni))
          protocol.decode(Arr(Bulk(ks.value.headOption.value), Bulk(i))) onRight (_.value shouldBe KV(ks.value.headOption.value, i))
        }
        "given one or more keys, timeout and specific read instance" in {
          forAll("keys", "timeout", "returned value") { (ks: OneOrMoreKeys, nni: NonNegInt, i: Int) =>
            val protocol = brpop[Foo](ks, nni)

            protocol.encode shouldBe Arr((Bulk("BRPOP") :: ks.value.map(Bulk(_))) :+ Bulk(nni))
            protocol.decode(Arr(Bulk(ks.value.headOption.value), Bulk(i))) onRight (_.value shouldBe KV(ks.value.headOption.value, Foo(i)))
          }
        }
      }
    }

    "using brpoplpush" should {
      "roundtrip successfully" when {
        "given source key and destination key" in forAll("source key", "destination key", "returned value") { (s: Key, d: Key, i: Int) =>
          val protocol = brpoplpush[Int](s, d)

          protocol.encode shouldBe Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(0))
          protocol.decode(Bulk(i)) onRight (_.value shouldBe i)
        }
        "given source key, destination key and specific read instance" in {
          forAll("source key", "destination key", "returned value") { (s: Key, d: Key, i: Int) =>
            val protocol = brpoplpush[Foo](s, d)

            protocol.encode shouldBe Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(0))
            protocol.decode(Bulk(i)) onRight (_.value shouldBe Foo(i))
          }
        }
        "given source key, destination key and timeout" in {
          forAll("source key", "destination key", "timeout", "returned value") { (s: Key, d: Key, pi: PosInt, i: Int) =>
            val protocol = brpoplpush[Int](s, d, pi)

            protocol.encode shouldBe Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(pi))
            protocol.decode(Bulk(i)) onRight (_.value shouldBe i)
          }
        }
        "given source key, destination key, timeout and specific read instance" in {
          forAll("source key", "destination key", "timeout", "returned value") { (s: Key, d: Key, pi: PosInt, i: Int) =>
            val protocol = brpoplpush[Foo](s, d, pi)

            protocol.encode shouldBe Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(pi))
            protocol.decode(Bulk(i)) onRight (_.value shouldBe Foo(i))
          }
        }
      }
    }
  }
}
