package laserdisc
package protocol

final class HashPSpec extends BaseSpec with HashP {
  import shapeless._

  "The Hash protocol" when {

    "using hdel" should {

      "roundtrip successfully" when {
        "given key and one field" in forAll { (k: Key, f: Key, nni: NonNegInt) =>
          val protocol = hdel(k, f)

          protocol.encode shouldBe Arr(Bulk("HDEL"), Bulk(k), Bulk(f))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
        "given key and two fields" in forAll { (k: Key, f1: Key, f2: Key, nni: NonNegInt) =>
          val protocol = hdel(k, f1, f2)

          protocol.encode shouldBe Arr(Bulk("HDEL"), Bulk(k), Bulk(f1), Bulk(f2))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
      }

    }

    "using hexists" should {

      "roundtrip successfully" when {
        "given key and field" in forAll { (k: Key, f: Key, b: Boolean) =>
          val protocol = hexists(k, f)

          protocol.encode shouldBe Arr(Bulk("HEXISTS"), Bulk(k), Bulk(f))
          protocol.decode(Num(if (b) 1 else 0)).right.value shouldBe b
        }
      }

    }

    "using hget" should {

      "fail to compile" when {
        "given key and one field but missing read instance" in {
          """hget[Bar](Key("a"), Key("f"))""" shouldNot compile
        }
      }

      "roundtrip successfully" when {
        "given non empty key and non empty field" in forAll { (k: Key, f: Key, s: String) =>
          val protocol = hget(k, f)

          protocol.encode shouldBe Arr(Bulk("HGET"), Bulk(k), Bulk(f))
          protocol.decode(Bulk(s)).right.value.value shouldBe Bulk(s)
        }
        "given specific read instance" in forAll { (k: Key, f: Key, i: Int) =>
          val protocol = hget[Foo](k, f)

          protocol.encode shouldBe Arr(Bulk("HGET"), Bulk(k), Bulk(f))
          protocol.decode(Bulk(i)).right.value.value shouldBe Foo(i)
        }
      }

    }

    "using hgetall" should {

      "fail to compile" when {
        "missing read instance" in {
          """hgetall[Map[String, Int]](Key("a"))""" shouldNot compile
        }
      }

      "roundtrip successfully" when {
        "given key" in forAll { (k: Key, f: Key, v: String) =>
          val protocol = hgetall(k)

          protocol.encode shouldBe Arr(Bulk("HGETALL"), Bulk(k))
          protocol.decode(Arr(Bulk(f), Bulk(v))).right.value shouldBe Arr(Bulk(f), Bulk(v))
        }
        "using OOB read instance" in forAll { (k: Key, f: Key, v: String) =>
          val protocol = hgetall[Map[Key, String]](k)

          protocol.encode shouldBe Arr(Bulk("HGETALL"), Bulk(k))
          protocol.decode(Arr(Bulk(f), Bulk(v))).right.value shouldBe Map(f -> v)
        }
        "deriving HList read instance" in forAll { (k: Key, f: Key, v: String) =>
          val protocol = hgetall[Key :: String :: HNil](k)

          protocol.encode shouldBe Arr(Bulk("HGETALL"), Bulk(k))
          protocol.decode(Arr(Bulk(f), Bulk(v))).right.value shouldBe f :: v :: HNil
        }
        "deriving Product read instance" in {
          //"""hgetall[Foo]("a")""" should compile
        }
      }

    }

    "using hincrby" should {

      "roundtrip successfully" when {
        "given key, field and increment" in forAll { (k: Key, f: Key, nzl: NonZeroLong, l: Long) =>
          val protocol = hincrby(k, f, nzl)

          protocol.encode shouldBe Arr(Bulk("HINCRBY"), Bulk(k), Bulk(f), Bulk(nzl))
          protocol.decode(Num(l)).right.value shouldBe l
        }
      }

    }

    "using hincrbyfloat" should {

      "roundtrip successfully" when {
        "given key, field and increment" in forAll { (k: Key, f: Key, nzd: NonZeroDouble, d: Double) =>
          val protocol = hincrbyfloat(k, f, nzd)

          protocol.encode shouldBe Arr(Bulk("HINCRBYFLOAT"), Bulk(k), Bulk(f), Bulk(nzd))
          protocol.decode(Bulk(d)).right.value shouldBe d
        }
      }

    }

    "using hkeys" should {

      "roundtrip successfully" when {
        "given key" in forAll { (k: Key, ks: List[Key]) =>
          whenever(keyIsValid(k.value) && ks.forall(k => keyIsValid(k.value))) {
            val protocol = hkeys(k)

            protocol.encode shouldBe Arr(Bulk("HKEYS"), Bulk(k))
            protocol.decode(Arr(ks.map(Bulk(_)))).right.value shouldBe ks
          }
        }
      }

    }

    "using hlen" should {

      "roundtrip successfully" when {
        "given key" in forAll { (k: Key, nni: NonNegInt) =>
          val protocol = hlen(k)

          protocol.encode shouldBe Arr(Bulk("HLEN"), Bulk(k))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
      }

    }

    "using hmget" should {

      "roundtrip successfully" when {
        "given key and one field" in forAll { (k: Key, f: Key, i: Int) =>
          val protocol = hmget[Int](k, f)

          protocol.encode shouldBe Arr(Bulk("HMGET"), Bulk(k), Bulk(f))
          protocol.decode(Arr(Bulk(i))).right.value shouldBe i
        }
        "given key and two fields" in forAll { (k: Key, f1: Key, f2: Key, i: Int, s: String) =>
          val protocol = hmget[Int, String](k, f1, f2)

          protocol.encode shouldBe Arr(Bulk("HMGET"), Bulk(k), Bulk(f1), Bulk(f2))
          protocol.decode(Arr(Bulk(i), Bulk(s))).right.value shouldBe (i -> s)
        }
      }

    }
  }
}
