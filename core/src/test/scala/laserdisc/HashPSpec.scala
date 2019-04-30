package laserdisc

import laserdisc.all._
import laserdisc.protocol.Bulk
import laserdisc.protocol.RESP._
import shapeless._

final class HashPSpec extends BaseSpec {
  "A HashP with HashPExtra" when {

    "using hdel" should {

      "fail to compile" when {
        "given empty key" in {
          """hdel("", "f")""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """hdel("a", "")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and one non empty field" in forAll { (k: Key, f: Key, nni: NonNegInt) =>
          val protocol = hdel(k, f)

          protocol.encode shouldBe arr(bulk("HDEL"), bulk(k.show), bulk(f.show))
          protocol.decode(num(nni.value.toLong)).right.value shouldBe nni
        }
        "given non empty key and two non empty fields" in forAll { (k: Key, f1: Key, f2: Key, nni: NonNegInt) =>
          val protocol = hdel(k, f1, f2)

          protocol.encode shouldBe arr(bulk("HDEL"), bulk(k.show), bulk(f1.show), bulk(f2.show))
          protocol.decode(num(nni.value.toLong)).right.value shouldBe nni
        }
      }

    }

    "using hexists" should {

      "fail to compile" when {
        "given empty key" in {
          """hexists("", "f")""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """hexists("a", "")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and non empty field" in forAll { (k: Key, f: Key, b: Boolean) =>
          val protocol = hexists(k, f)

          protocol.encode shouldBe arr(bulk("HEXISTS"), bulk(k.show), bulk(f.show))
          protocol.decode(num(if (b) 1 else 0)).right.value shouldBe b
        }
      }

    }

    "using hget" should {

      "fail to compile" when {
        "given empty key" in {
          """hget("", "f")""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """hget("a", "")""".stripMargin shouldNot compile
        }
        "missing read instance" in {
          """hget[Foo]("a", "f")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and non empty field" in forAll { (k: Key, f: Key, s: String) =>
          val protocol = hget(k, f)

          protocol.encode shouldBe arr(bulk("HGET"), bulk(k.show), bulk(f.show))
          protocol.decode(bulk(s)).right.value.value shouldBe bulk(s)
        }
        "given specific read instance" in {
          implicit val fooRead: Bulk ==> Foo = Read.instancePF {
            case Bulk(ToInt(x)) => Foo(x)
          }
          forAll { (k: Key, f: Key, i: Int) =>
            val protocol = hget[Foo](k, f)

            protocol.encode shouldBe arr(bulk("HGET"), bulk(k.show), bulk(f.show))
            protocol.decode(bulk(i.show)).right.value.value shouldBe Foo(i)
          }
        }
      }

    }

    "using hgetall" should {

      "fail to compile" when {
        "given empty key" in {
          """hgetall("", "f")""".stripMargin shouldNot compile
        }
        "missing read instance" in {
          """hgetall[Map[String, Int]]("a")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key" in forAll { (k: Key, f: Key, v: String) =>
          val protocol = hgetall(k)

          protocol.encode shouldBe arr(bulk("HGETALL"), bulk(k.show))
          protocol.decode(arr(bulk(f.show), bulk(v))).right.value shouldBe arr(bulk(f.show), bulk(v))
        }
        "using OOB read instance" in forAll { (k: Key, f: Key, v: String) =>
          val protocol = hgetall[Map[Key, String]](k)

          protocol.encode shouldBe arr(bulk("HGETALL"), bulk(k.show))
          protocol.decode(arr(bulk(f.show), bulk(v))).right.value shouldBe Map(f -> v)
        }
        "deriving HList read instance" in forAll { (k: Key, f: Key, v: String) =>
          val protocol = hgetall[Key :: String :: HNil](k)

          protocol.encode shouldBe arr(bulk("HGETALL"), bulk(k.show))
          protocol.decode(arr(bulk(f.show), bulk(v))).right.value shouldBe f :: v :: HNil
        }
        "deriving Product read instance" in {
          //"""hgetall[Foo]("a")""".stripMargin should compile
        }
      }

    }

    "using hincrby" should {

      "fail to compile" when {
        "given empty key" in {
          """hincrby("", "f", 1L)""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """hincrby("a", "", 1L)""".stripMargin shouldNot compile
        }
        "given zero increment" in {
          """hincrby("a", "f", 0L)""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key, non empty field and non zero increment" in forAll { (k: Key, f: Key, nzl: NonZeroLong, l: Long) =>
          val protocol = hincrby(k, f, nzl)

          protocol.encode shouldBe arr(bulk("HINCRBY"), bulk(k.show), bulk(f.show), bulk(nzl.show))
          protocol.decode(num(l)).right.value shouldBe l
        }
      }

    }

    "using hincrbyfloat" should {

      "fail to compile" when {
        "given empty key" in {
          """hincrbyfloat("", "f", 1d)""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """hincrbyfloat("a", "", 1d)""".stripMargin shouldNot compile
        }
        "given zero increment" in {
          """hincrbyfloat("a", "f", 0d)""".stripMargin shouldNot compile
        }
        "given NaN increment" in {
          """hincrbyfloat("a", "f", java.lang.Double.NaN)""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key, non empty field and non zero increment" in forAll { (k: Key, f: Key, nzd: NonZeroDouble, d: Double) =>
          val protocol = hincrbyfloat(k, f, nzd)

          protocol.encode shouldBe arr(bulk("HINCRBYFLOAT"), bulk(k.show), bulk(f.show), bulk(nzd.show))
          protocol.decode(bulk(d.show)).right.value shouldBe d
        }
      }

    }

    "using hkeys" should {

      "fail to compile" when {
        "given empty key" in {
          """hkeys("")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key" in forAll { (k: Key, ks: Seq[Key]) =>
          val protocol = hkeys(k)

          protocol.encode shouldBe arr(bulk("HKEYS"), bulk(k.show))
          protocol.decode(arr(ks.map(k => bulk(k.show)))).right.value shouldBe ks
        }
      }

    }

    "using hlen" should {

      "fail to compile" when {
        "given empty key" in {
          """hlen("")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key" in forAll { (k: Key, nni: NonNegInt) =>
          val protocol = hlen(k)

          protocol.encode shouldBe arr(bulk("HLEN"), bulk(k.show))
          protocol.decode(num(nni.value.toLong)).right.value shouldBe nni
        }
      }

    }

    "using hmget" should {

      "fail to compile" when {
        "given empty key" in {
          """hmget("", "f")""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """hmget("a", "")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and one non empty field" in forAll { (k: Key, f: Key, i: Int) =>
          val protocol = hmget[Int](k, f)

          protocol.encode shouldBe arr(bulk("HMGET"), bulk(k.show), bulk(f.show))
          protocol.decode(arr(bulk(i.show))).right.value shouldBe i
        }
        "given non empty key and two non empty fields" in forAll { (k: Key, f1: Key, f2: Key, i: Int, s: String) =>
          val protocol = hmget[Int, String](k, f1, f2)

          protocol.encode shouldBe arr(bulk("HMGET"), bulk(k.show), bulk(f1.show), bulk(f2.show))
          protocol.decode(arr(bulk(i.show), bulk(s.show))).right.value shouldBe (i -> s)
        }
      }

    }
  }
}
