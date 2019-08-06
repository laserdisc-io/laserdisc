package laserdisc
package protocol

final class HashPSpec extends HashExtPSpec {
  import shapeless._

  private[this] final val scanKVToArr: ScanKV => Arr = scanKV =>
    Arr(
      Bulk(scanKV.cursor.value),
      scanKV.maybeValues.fold(NilArr: GenArr)(kvs => Arr(kvs.flatMap { case KV(k, v) => List(Bulk(k.value), Bulk(v)) }.toList))
  )

  "The Hash protocol" when {

    "using hdel" should {

      "roundtrip successfully" when {
        "given key and fields" in forAll { (k: Key, fs: OneOrMoreKeys, nni: NonNegInt) =>
          val protocol = hdel(k, fs)

          protocol.encode shouldBe Arr(Bulk("HDEL") :: Bulk(k) :: fs.value.map(Bulk(_)))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
      }
    }

    "using hexists" should {

      "roundtrip successfully" when {
        "given key and field" in forAll { (k: Key, f: Key, b: Boolean) =>
          val protocol = hexists(k, f)

          protocol.encode shouldBe Arr(Bulk("HEXISTS"), Bulk(k), Bulk(f))
          protocol.decode(boolToNum(b)).right.value shouldBe b
        }
      }

    }

    "using hget" should {

      "fail to compile" when {
        "given key and field but missing read instance" in {
          """hget[Bar](Key("a"), Key("f"))""" shouldNot compile
        }
      }

      "roundtrip successfully" when {
        "given key and field" in forAll("key", "field", "returned value") { (k: Key, f: Key, s: String) =>
          val protocol = hget(k, f)

          protocol.encode shouldBe Arr(Bulk("HGET"), Bulk(k), Bulk(f))
          protocol.decode(Bulk(s)).right.value shouldBe Some(Bulk(s))
        }
        "given key, field and specific read instance" in forAll("key", "field", "returned value") { (k: Key, f: Key, i: Int) =>
          val protocol = hget[Foo](k, f)

          protocol.encode shouldBe Arr(Bulk("HGET"), Bulk(k), Bulk(f))
          protocol.decode(Bulk(i)).right.value shouldBe Some(Foo(i))
        }
      }
    }

    "using hgetall" should {

      "fail to compile" when {
        "given key but missing read instance" in {
          """hgetall[Map[String, Int]](Key("a"))""" shouldNot compile
        }
      }

      "roundtrip successfully" when {
        "given key" in forAll("key", "returned field", "returned value") { (k: Key, f: Key, v: String) =>
          val protocol = hgetall(k)

          protocol.encode shouldBe Arr(Bulk("HGETALL"), Bulk(k))
          protocol.decode(Arr(Bulk(f), Bulk(v))).right.value shouldBe Arr(Bulk(f), Bulk(v))
        }
        "given key and specific read instance (Map[Key, String])" in {
          forAll("key", "returned field", "returned value") { (k: Key, f: Key, v: String) =>
            val protocol = hgetall[Map[Key, String]](k)

            protocol.encode shouldBe Arr(Bulk("HGETALL"), Bulk(k))
            protocol.decode(Arr(Bulk(f), Bulk(v))).right.value shouldBe Map(f -> v)
          }
        }
        "given key and specific read instance (Key :: String :: HNil)" in {
          forAll("key", "returned field", "returned value") { (k: Key, f: Key, v: String) =>
            val protocol = hgetall[Key :: String :: HNil](k)

            protocol.encode shouldBe Arr(Bulk("HGETALL"), Bulk(k))
            protocol.decode(Arr(Bulk(f), Bulk(v))).right.value shouldBe f :: v :: HNil
          }
        }
      }
    }

    "using hincrby" should {

      "roundtrip successfully" when {
        "given key, field and long increment" in {
          forAll("key", "field", "increment", "incremented value") { (k: Key, f: Key, nzl: NonZeroLong, l: Long) =>
            val protocol = hincrby(k, f, nzl)

            protocol.encode shouldBe Arr(Bulk("HINCRBY"), Bulk(k), Bulk(f), Bulk(nzl))
            protocol.decode(Num(l)).right.value shouldBe l
          }
        }
        "given key, field and double increment" in {
          forAll("key", "field", "increment", "incremented value") { (k: Key, f: Key, nzd: NonZeroDouble, d: Double) =>
            val protocol = hincrby(k, f, nzd)

            protocol.encode shouldBe Arr(Bulk("HINCRBYFLOAT"), Bulk(k), Bulk(f), Bulk(nzd))
            protocol.decode(Bulk(d)).right.value shouldBe d
          }
        }
      }
    }

    "using hkeys" should {

      "roundtrip successfully" when {
        "given key" in forAll("key", "returned keys") { (k: Key, ks: List[Key]) =>
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
        "given key" in forAll("key", "length") { (k: Key, nni: NonNegInt) =>
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

      "using hmset" should {

        "fail to compile" when {
          "given key and HNil" in {
            """hmset(Key("a"), HNil)""" shouldNot compile
          }
        }

        "roundtrip successfully" when {
          "given key and HList of (Key, A) pairs" in forAll { (k: Key, f1: Key, i: Int, f2: Key, s: String) =>
            val protocol = hmset(k, (f1 -> i) :: (f2 -> s) :: HNil)

            protocol.encode shouldBe Arr(Bulk("HMSET"), Bulk(k), Bulk(f1), Bulk(i), Bulk(f2), Bulk(s))
            protocol.decode(Str(OK.value)).right.value shouldBe OK
          }
          "given key and a Product (Baz)" in forAll { (k: Key, baz: Baz) =>
            val protocol = hmset(k, baz)

            protocol.encode shouldBe Arr(Bulk("HMSET"), Bulk(k), Bulk("f1"), Bulk(baz.f1), Bulk("f2"), Bulk(baz.f2))
            protocol.decode(Str(OK.value)).right.value shouldBe OK
          }
        }
      }
    }

    "using hscan" should {

      "roundtrip successfully" when {
        "given key and cursor" in forAll("key", "cursor", "scan result") { (k: Key, nnl: NonNegLong, skv: ScanKV) =>
          val protocol = hscan(k, nnl)

          protocol.encode shouldBe Arr(Bulk("HSCAN"), Bulk(k), Bulk(nnl))
          protocol.decode(scanKVToArr(skv)).right.value shouldBe skv
        }
      }
    }

    //TODO add all other missing tests
  }
}
