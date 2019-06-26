package laserdisc
package protocol

final class BListPSpec extends BListExtPSpec {

  "The Blocking List protocol" when {

    "using blpop" should {

      "fail to compile" when {
        "given key and timeout but missing read instance" in {
          """blpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))""" shouldNot compile
        }
      }

      "roundtrip successfully" when {
        "given non empty key list and non negative timeout" in {
          forAll("keys", "timeout", "return value") { (ks: OneOrMoreKeys, nni: NonNegInt, i: Int) =>
            val protocol = blpop[Int](ks, nni)

            protocol.encode shouldBe Arr((Bulk("BLPOP") :: ks.value.map(k => Bulk(k))) :+ Bulk(nni))
            protocol.decode(Arr(Bulk(ks.value.headOption.value), Bulk(i))).right.value.value shouldBe KV(ks.value.headOption.value, i)
          }
        }
        "given non empty key list, non negative timeout and specific read instance" in {
          forAll("keys", "timeout", "return value") { (ks: OneOrMoreKeys, nni: NonNegInt, i: Int) =>
            val protocol = blpop[Foo](ks, nni)

            protocol.encode shouldBe Arr((Bulk("BLPOP") :: ks.value.map(k => Bulk(k))) :+ Bulk(nni))
            protocol.decode(Arr(Bulk(ks.value.headOption.value), Bulk(i))).right.value.value shouldBe KV(ks.value.headOption.value, Foo(i))
          }
        }
      }
    }

    "using brpop" should {

      "fail to compile" when {
        "given key and timeout but missing read instance" in {
          """brpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))""" shouldNot compile
        }
      }

      "roundtrip successfully" when {
        "given non empty key list and non negative timeout" in {
          forAll("keys", "timeout", "return value") { (ks: OneOrMoreKeys, nni: NonNegInt, i: Int) =>
            val protocol = brpop[Int](ks, nni)

            protocol.encode shouldBe Arr((Bulk("BRPOP") :: ks.value.map(k => Bulk(k))) :+ Bulk(nni))
            protocol.decode(Arr(Bulk(ks.value.headOption.value), Bulk(i))).right.value.value shouldBe KV(ks.value.headOption.value, i)
          }
        }
        "given non empty key list, non negative timeout and specific read instance" in {
          forAll("keys", "timeout", "return value") { (ks: OneOrMoreKeys, nni: NonNegInt, i: Int) =>
            val protocol = brpop[Foo](ks, nni)

            protocol.encode shouldBe Arr((Bulk("BRPOP") :: ks.value.map(k => Bulk(k))) :+ Bulk(nni))
            protocol.decode(Arr(Bulk(ks.value.headOption.value), Bulk(i))).right.value.value shouldBe KV(ks.value.headOption.value, Foo(i))
          }
        }
      }
    }

    "using brpoplpush" should {

      "roundtrip successfully" when {
        "given non empty source key and non empty destination key" in {
          forAll("source", "destination", "return value") { (s: Key, d: Key, i: Int) =>
            val protocol = brpoplpush[Int](s, d)

            protocol.encode shouldBe Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(0))
            protocol.decode(Bulk(i)).right.value.value shouldBe i
          }
        }
        "given non empty source key, non empty destination key and specific read instance" in {
          forAll("source", "destination", "return value") { (s: Key, d: Key, i: Int) =>
            val protocol = brpoplpush[Foo](s, d)

            protocol.encode shouldBe Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(0))
            protocol.decode(Bulk(i)).right.value.value shouldBe Foo(i)
          }
        }
        "given non empty source key, non empty destination key and positive timeout" in {
          forAll("source", "destination", "timeout", "return value") { (s: Key, d: Key, pi: PosInt, i: Int) =>
            val protocol = brpoplpush[Int](s, d, pi)

            protocol.encode shouldBe Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(pi))
            protocol.decode(Bulk(i)).right.value.value shouldBe i
          }
        }
        "given non empty source key, non empty destination key, positive timeout and specific read instance" in {
          forAll("source", "destination", "timeout", "return value") { (s: Key, d: Key, pi: PosInt, i: Int) =>
            val protocol = brpoplpush[Foo](s, d, pi)

            protocol.encode shouldBe Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(pi))
            protocol.decode(Bulk(i)).right.value.value shouldBe Foo(i)
          }
        }
      }
    }
  }
}
