package laserdisc
package protocol

object BListPSpec {
  final case class Foo(x: Int)
}

final class BListPSpec extends BaseSpec {

  import auto._
  import BListPSpec._
  import lists.blocking._
  import RESP._
  import show._

  "AllBListP" when {

    "using blpop" should {

      "fail to compile" when {
        "given non literal to refine" in {
          """blpop(OneOrMoreKeys(List(Key("a"))), 0)""" shouldNot compile
        }
        "given negative timeout" in {
          """blpop(OneOrMoreKeys.unsafeFrom(List(Key("a"))), -1)""" shouldNot compile
        }
        "missing read instance" in {
          """blpop[Foo](OneOrMoreKeys.unsafeFrom(List(Key("a"))), 0)""".stripMargin shouldNot compile
        }
      }

      "fail at runtime" when {
        "given empty key list to safely refine" in {
          OneOrMoreKeys
            .from(List.empty[Key])
            .right
            .map(blpop(_, 0))
            .left
            .value shouldBe "Predicate isEmpty(List()) did not fail."
        }
        "given empty key list to unsafely refine" in {
          the[IllegalArgumentException].thrownBy {
            blpop(OneOrMoreKeys.unsafeFrom(List.empty[Key]), 0)
          }.getMessage shouldBe "Predicate isEmpty(List()) did not fail."
        }
      }

      "compile successfully" when {
        "given non empty key list and non negative timeout" in {
          forAll("keys", "timeout", "return value") { (ks: OneOrMoreKeys, nni: NonNegInt, i: Int) =>
            val protocol = blpop[Int](ks, nni)

            protocol.encode shouldBe arr((bulk("BLPOP") :: ks.map(k => bulk(k.show))) :+ bulk(nni.show))
            protocol
              .decode(
                arr(bulk(ks.headOption.value.show), bulk(i.show))
              )
              .right
              .value
              .value shouldBe KV(ks.headOption.value, i)
          }
        }
        "given specific read instance" in {
          implicit val fooRead: Read[Bulk, Foo] = Read.instancePF {
            case Bulk(ToInt(i)) => Foo(i)
          }
          forAll("key", "return value") { (k: Key, i: Int) =>
            val protocol = blpop[Foo](k)

            protocol.encode shouldBe arr(bulk("BLPOP"), bulk(k.show), bulk(0.show))
            protocol.decode(arr(bulk(k.show), bulk(i.show))).right.value.value shouldBe KV(k, Foo(i))
          }
        }
      }

    }

    "using brpop" should {

      "fail to compile" when {
        "given empty key" in {
          """brpop("")""".stripMargin shouldNot compile
        }
        "given negative timeout" in {
          """brpop("a", -1)""".stripMargin shouldNot compile
        }
        "given zero timeout" in {
          """brpop("a", 0)""".stripMargin shouldNot compile
        }
        "missing read instance" in {
          """brpop[Foo]("a", 1)""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given specific read instance" in {
          implicit val fooRead: Read[Bulk, Foo] = Read.instancePF {
            case Bulk(ToInt(i)) => Foo(i)
          }
          forAll("key", "return value") { (k: Key, i: Int) =>
            val protocol = brpop[Foo](k)

            protocol.encode shouldBe arr(bulk("BRPOP"), bulk(k.show), bulk(0.show))
            protocol.decode(arr(bulk(k.show), bulk(i.show))).right.value.value shouldBe KV(k, Foo(i))
          }
        }
      }

    }

    "using brpoplpush" should {

      "fail to compile" when {
        "given empty source key" in {
          """brpoplpush("", "b")""".stripMargin shouldNot compile
        }
        "given empty destination key" in {
          """brpoplpush("a", "")""".stripMargin shouldNot compile
        }
        "given negative timeout" in {
          """brpoplpush("a", "b", -1)""".stripMargin shouldNot compile
        }
        "given zero timeout" in {
          """brpoplpush("a", "b", 0)""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty source key and non empty destination key" in {
          forAll("source", "destination", "return value") { (s: Key, d: Key, i: Int) =>
            val protocol = brpoplpush[Int](s, d)

            protocol.encode shouldBe arr(bulk("BRPOPLPUSH"), bulk(s.show), bulk(d.show), bulk(0.show))
            protocol.decode(bulk(i.show)).right.value.value shouldBe i
          }
        }
        "given non empty source key, non empty destination key and positive timeout" in {
          forAll("source", "destination", "timeout", "return value") { (s: Key, d: Key, pi: PosInt, i: Int) =>
            val protocol = brpoplpush[Int](s, d, pi)

            protocol.encode shouldBe arr(bulk("BRPOPLPUSH"), bulk(s.show), bulk(d.show), bulk(pi.show))
            protocol.decode(bulk(i.show)).right.value.value shouldBe i
          }
        }
        "given specific read instance" in {
          implicit val fooRead: Read[Bulk, Foo] = Read.instancePF {
            case Bulk(ToInt(i)) => Foo(i)
          }
          forAll("source", "destination", "timeout", "return value") { (s: Key, d: Key, pi: PosInt, i: Int) =>
            val protocol = brpoplpush[Foo](s, d, pi)

            protocol.encode shouldBe arr(bulk("BRPOPLPUSH"), bulk(s.show), bulk(d.show), bulk(pi.show))
            protocol.decode(bulk(i.show)).right.value.value shouldBe Foo(i)
          }
        }
      }

    }
  }
}
