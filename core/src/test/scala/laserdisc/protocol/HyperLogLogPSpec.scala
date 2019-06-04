package laserdisc
package protocol

final class HyperLogLogPSpec extends BaseSpec {
  import auto._
  import hyperloglog._
  import RESP._
  import show._

  "A HyperLogLogP with HyperLogLogPExtra" when {

    "using pfadd" should {

      "fail to compile" when {
        "given empty key" in {
          """pfadd("", "e")""".stripMargin shouldNot compile
        }
        "given empty element" in {
          """pfadd("a", "")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and one non empty element" in forAll { (key: Key, el: Key, b: Boolean) =>
          val protocol = pfadd(key, el)

          protocol.encode shouldBe arr(bulk("PFADD"), bulk(key.show), bulk(el.show))
          protocol.decode(num(if (b) 1 else 0)).right.value shouldBe b
        }
        "given non empty key and two non empty elements" in forAll { (key: Key, el1: Key, el2: Key, b: Boolean) =>
          val protocol = pfadd(key, el1, el2)

          protocol.encode shouldBe arr(bulk("PFADD"), bulk(key.show), bulk(el1.show), bulk(el2.show))
          protocol.decode(num(if (b) 1 else 0)).right.value shouldBe b
        }
      }

    }

    "using pfcount" should {

      "fail to compile" when {
        "given one empty key" in {
          """pfcount("")""".stripMargin shouldNot compile
        }
        "given two empty keys" in {
          """pfcount("", "")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given one non empty key" in forAll { (key: Key, nni: NonNegInt) =>
          val protocol = pfcount(key)

          protocol.encode shouldBe arr(bulk("PFCOUNT"), bulk(key.show))
          protocol.decode(num(nni.value.toLong)).right.value shouldBe nni
        }
        "given two non empty keys" in forAll { (key1: Key, key2: Key, nni: NonNegInt) =>
          val protocol = pfcount(key1, key2)

          protocol.encode shouldBe arr(bulk("PFCOUNT"), bulk(key1.show), bulk(key2.show))
          protocol.decode(num(nni.value.toLong)).right.value shouldBe nni
        }
      }

    }
  }
}
