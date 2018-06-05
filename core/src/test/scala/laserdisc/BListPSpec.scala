package laserdisc

import org.scalatest.{Matchers, WordSpecLike}

final class BListPSpec extends WordSpecLike with Matchers {

  "A BListP" when {

    "using blpop" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |lists.blocking.blpop("")""".stripMargin shouldNot compile
        }
        "given negative timeout" in {
          """import auto._
            |lists.blocking.blpop("a", -1)""".stripMargin shouldNot compile
        }
        "given zero timeout" in {
          """import auto._
            |lists.blocking.blpop("a", 0)""".stripMargin shouldNot compile
        }
        "missing read instance" in {
          """import auto._
            |case class Foo(x: Int)
            |lists.blocking.blpop[Foo]("a", 1)""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given one non empty key" in {
          """import auto._
            |lists.blocking.blpop("a")""".stripMargin should compile
        }
        "given one non empty key and positive timeout" in {
          """import auto._
            |lists.blocking.blpop("a", 1)""".stripMargin should compile
        }
        "given two non empty keys" in {
          """import auto._
            |lists.blocking.blpop("a", "b")""".stripMargin should compile
        }
        "given two non empty keys and positive timeout" in {
          """import auto._
            |lists.blocking.blpop("a", "b", 1)""".stripMargin should compile
        }
        "given specific read instance" in {
          """import auto._
            |case class Foo(x: PosInt)
            |implicit val fooRead: Read[protocol.NonNullBulkString, Foo] = Read.instancePF {
            |  case protocol.NonNullBulkString(ToInt(PosInt(i))) => Foo(i)
            |}
            |lists.blocking.blpop[Foo]("a", 1)""".stripMargin should compile
        }
      }

    }

    "using brpop" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |lists.blocking.brpop("")""".stripMargin shouldNot compile
        }
        "given negative timeout" in {
          """import auto._
            |lists.blocking.brpop("a", -1)""".stripMargin shouldNot compile
        }
        "given zero timeout" in {
          """import auto._
            |lists.blocking.brpop("a", 0)""".stripMargin shouldNot compile
        }
        "missing read instance" in {
          """import auto._
            |case class Foo(x: Int)
            |lists.blocking.brpop[Foo]("a", 1)""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given one non empty key" in {
          """import auto._
            |lists.blocking.brpop("a")""".stripMargin should compile
        }
        "given one non empty key and positive timeout" in {
          """import auto._
            |lists.blocking.brpop("a", 1)""".stripMargin should compile
        }
        "given two non empty keys" in {
          """import auto._
            |lists.blocking.brpop("a", "b")""".stripMargin should compile
        }
        "given two non empty keys and positive timeout" in {
          """import auto._
            |lists.blocking.brpop("a", "b", 1)""".stripMargin should compile
        }
        "given specific read instance" in {
          """import auto._
            |case class Foo(x: PosInt)
            |implicit val fooRead: Read[protocol.NonNullBulkString, Foo] = Read.instancePF {
            |  case protocol.NonNullBulkString(ToInt(PosInt(i))) => Foo(i)
            |}
            |lists.blocking.brpop[Foo]("a", 1)""".stripMargin should compile
        }
      }

    }

    "using brpoplpush" should {

      "fail to compile" when {
        "given empty source key" in {
          """import auto._
            |lists.blocking.brpoplpush("", "b")""".stripMargin shouldNot compile
        }
        "given empty destination key" in {
          """import auto._
            |lists.blocking.brpoplpush("a", "")""".stripMargin shouldNot compile
        }
        "given negative timeout" in {
          """import auto._
            |lists.blocking.brpoplpush("a", "b", -1)""".stripMargin shouldNot compile
        }
        "given zero timeout" in {
          """import auto._
            |lists.blocking.brpoplpush("a", "b", 0)""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty source key and non empty destination key" in {
          """import auto._
            |lists.blocking.brpoplpush("a", "b")""".stripMargin should compile
        }
        "given non empty source key, non empty destination key and positive timeout" in {
          """import auto._
            |lists.blocking.brpoplpush("a", "b", 1)""".stripMargin should compile
        }
      }

    }
  }
}
