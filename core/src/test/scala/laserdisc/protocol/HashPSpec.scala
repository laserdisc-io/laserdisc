package laserdisc
package protocol

final class HashPSpec extends BaseSpec {
  import auto._
  import hashmaps._
  import shapeless._
  import show._

  "A HashP with HashPExtra" when {

    "using hdel" should {

      "fail to compile" when {
        "given empty key" in {
          """hdel("", Key("f"))""" shouldNot compile
        }
        "given empty field" in {
          """hdel(Key("a"), "")""" shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and one non empty field" in forAll { (k: Key, f: Key, nni: NonNegInt) =>
          val protocol = hdel(k, f)

          protocol.encode shouldBe Arr(Bulk("HDEL"), Bulk(k.show), Bulk(f.show))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
        "given non empty key and two non empty fields" in forAll { (k: Key, f1: Key, f2: Key, nni: NonNegInt) =>
          val protocol = hdel(k, f1, f2)

          protocol.encode shouldBe Arr(Bulk("HDEL"), Bulk(k.show), Bulk(f1.show), Bulk(f2.show))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
      }

    }

    "using hexists" should {

      "fail to compile" when {
        "given empty key" in {
          """hexists("", Key("f"))""" shouldNot compile
        }
        "given empty field" in {
          """hexists(Key("a"), "")""" shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and non empty field" in forAll { (k: Key, f: Key, b: Boolean) =>
          val protocol = hexists(k, f)

          protocol.encode shouldBe Arr(Bulk("HEXISTS"), Bulk(k.show), Bulk(f.show))
          protocol.decode(Num(if (b) 1 else 0)).right.value shouldBe b
        }
      }

    }

    "using hget" should {

      "fail to compile" when {
        "given empty key" in {
          """hget("", Key("f"))""" shouldNot compile
        }
        "given empty field" in {
          """hget(Key("a"), "")""" shouldNot compile
        }
        "missing read instance" in {
          """hget[Bar](Key("a"), Key("f"))""" shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and non empty field" in forAll { (k: Key, f: Key, s: String) =>
          val protocol = hget(k, f)

          protocol.encode shouldBe Arr(Bulk("HGET"), Bulk(k.show), Bulk(f.show))
          protocol.decode(Bulk(s)).right.value.value shouldBe Bulk(s)
        }
        "given specific read instance" in {
          forAll { (k: Key, f: Key, i: Int) =>
            val protocol = hget[Foo](k, f)

            protocol.encode shouldBe Arr(Bulk("HGET"), Bulk(k.show), Bulk(f.show))
            protocol.decode(Bulk(i.show)).right.value.value shouldBe Foo(i)
          }
        }
      }

    }

    "using hgetall" should {

      "fail to compile" when {
        "given empty key" in {
          """hgetall("")""" shouldNot compile
        }
        "missing read instance" in {
          """hgetall[Map[String, Int]]("a")""" shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key" in forAll { (k: Key, f: Key, v: String) =>
          val protocol = hgetall(k)

          protocol.encode shouldBe Arr(Bulk("HGETALL"), Bulk(k.show))
          protocol.decode(Arr(Bulk(f.show), Bulk(v))).right.value shouldBe Arr(Bulk(f.show), Bulk(v))
        }
        "using OOB read instance" in forAll { (k: Key, f: Key, v: String) =>
          val protocol = hgetall[Map[Key, String]](k)

          protocol.encode shouldBe Arr(Bulk("HGETALL"), Bulk(k.show))
          protocol.decode(Arr(Bulk(f.show), Bulk(v))).right.value shouldBe Map(f -> v)
        }
        "deriving HList read instance" in forAll { (k: Key, f: Key, v: String) =>
          val protocol = hgetall[Key :: String :: HNil](k)

          protocol.encode shouldBe Arr(Bulk("HGETALL"), Bulk(k.show))
          protocol.decode(Arr(Bulk(f.show), Bulk(v))).right.value shouldBe f :: v :: HNil
        }
        "deriving Product read instance" in {
          //"""hgetall[Foo]("a")""" should compile
        }
      }

    }

    "using hincrby" should {

      "fail to compile" when {
        "given empty key" in {
          """hincrby("", Key("f"), NonZeroLong(1L))""" shouldNot compile
        }
        "given empty field" in {
          """hincrby(Key("a"), "", NonZeroLong(1L))""" shouldNot compile
        }
        "given zero increment" in {
          """hincrby(Key("a"), Key("f"), 0L)""" shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key, non empty field and non zero increment" in forAll { (k: Key, f: Key, nzl: NonZeroLong, l: Long) =>
          val protocol = hincrby(k, f, nzl)

          protocol.encode shouldBe Arr(Bulk("HINCRBY"), Bulk(k.show), Bulk(f.show), Bulk(nzl.show))
          protocol.decode(Num(l)).right.value shouldBe l
        }
      }

    }

    "using hincrbyfloat" should {

      "fail to compile" when {
        "given empty key" in {
          """hincrbyfloat("", Key("f"), NonZeroLong(1d))""" shouldNot compile
        }
        "given empty field" in {
          """hincrbyfloat(Key("a"), "", NonZeroLong(1d))""" shouldNot compile
        }
        "given zero increment" in {
          """hincrbyfloat(Key("a"), Key("f"), 0d)""" shouldNot compile
        }
        "given NaN increment" in {
          """hincrbyfloat(Key("a"), Key("f"), java.lang.Double.NaN)""" shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key, non empty field and non zero increment" in forAll { (k: Key, f: Key, nzd: NonZeroDouble, d: Double) =>
          val protocol = hincrbyfloat(k, f, nzd)

          protocol.encode shouldBe Arr(Bulk("HINCRBYFLOAT"), Bulk(k.show), Bulk(f.show), Bulk(nzd.show))
          protocol.decode(Bulk(d.show)).right.value shouldBe d
        }
      }

    }

    "using hkeys" should {

      "fail to compile" when {
        "given empty key" in {
          """hkeys("")""" shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key" in forAll { (k: Key, ks: List[Key]) =>
          val protocol = hkeys(k)

          protocol.encode shouldBe Arr(Bulk("HKEYS"), Bulk(k.show))
          protocol.decode(Arr(ks.map(k => Bulk(k.show)))).right.value shouldBe ks
        }
      }

    }

    "using hlen" should {

      "fail to compile" when {
        "given empty key" in {
          """hlen("")""" shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key" in forAll { (k: Key, nni: NonNegInt) =>
          val protocol = hlen(k)

          protocol.encode shouldBe Arr(Bulk("HLEN"), Bulk(k.show))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
      }

    }

    "using hmget" should {

      "fail to compile" when {
        "given empty key" in {
          """hmget("", "f")""" shouldNot compile
        }
        "given empty field" in {
          """hmget("a", "")""" shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and one non empty field" in forAll { (k: Key, f: Key, i: Int) =>
          val protocol = hmget[Int](k, f)

          protocol.encode shouldBe Arr(Bulk("HMGET"), Bulk(k.show), Bulk(f.show))
          protocol.decode(Arr(Bulk(i.show))).right.value shouldBe i
        }
        "given non empty key and two non empty fields" in forAll { (k: Key, f1: Key, f2: Key, i: Int, s: String) =>
          val protocol = hmget[Int, String](k, f1, f2)

          protocol.encode shouldBe Arr(Bulk("HMGET"), Bulk(k.show), Bulk(f1.show), Bulk(f2.show))
          protocol.decode(Arr(Bulk(i.show), Bulk(s.show))).right.value shouldBe (i -> s)
        }
      }

    }
  }
}
