package laserdisc

import org.scalatest.{Matchers, WordSpecLike}

final class HashPSpec extends WordSpecLike with Matchers {

  "A HashP" when {

    "using hdel" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |hashmaps.hdel("", "f")""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """import auto._
            |hashmaps.hdel("a", "")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and one non empty field" in {
          """import auto._
            |hashmaps.hdel("a", "f1")""".stripMargin should compile
        }
        "given non empty key and two non empty fields" in {
          """import auto._
            |hashmaps.hdel("a", "f1", "f2")""".stripMargin should compile
        }
      }

    }

    "using hexists" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |hashmaps.hexists("", "f")""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """import auto._
            |hashmaps.hexists("a", "")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and non empty field" in {
          """import auto._
            |hashmaps.hexists("a", "f")""".stripMargin should compile
        }
      }

    }

    "using hget" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |hashmaps.hget("", "f")""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """import auto._
            |hashmaps.hget("a", "")""".stripMargin shouldNot compile
        }
        "missing read instance" in {
          """import auto._
            |case class Foo(x: Int)
            |hashmaps.hget[Foo]("a", "f")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and non empty field" in {
          """import auto._
            |hashmaps.hget("a", "f1")""".stripMargin should compile
        }
        "given specific read instance" in {
          """import auto._
            |case class Foo(x: String)
            |implicit val fooRead: Read[protocol.NonNullBulkString, Foo] = Read.instancePF {
            |  case protocol.NonNullBulkString(x) => Foo(x)
            |}
            |hashmaps.hget[Foo]("a", "f")""".stripMargin should compile
        }
      }

    }

    "using hgetall" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |hashmaps.hgetall("", "f")""".stripMargin shouldNot compile
        }
        "missing read instance" in {
          """import auto._
            |hashmaps.hgetall[Map[String, Int]]("a")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key" in {
          """import auto._
            |hashmaps.hgetall("a")""".stripMargin should compile
        }
        "using OOB read instance" in {
          """import auto._
            |hashmaps.hgetall[Map[Key, String]]("a")""".stripMargin should compile
        }
        "deriving HList read instance" in {
          """import auto._, shapeless._
            |hashmaps.hgetall[Key :: Int :: HNil]("a")""".stripMargin should compile
        }
        "deriving Product read instance" in {
          """import auto._
            |case class Foo(a: String)
            |hashmaps.hgetall[Foo]("a")""".stripMargin should compile
        }
      }

    }

    "using hincrby" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |hashmaps.hincrby("", "f", 1L)""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """import auto._
            |hashmaps.hincrby("a", "", 1L)""".stripMargin shouldNot compile
        }
        "given zero increment" in {
          """import auto._
            |hashmaps.hincrby("a", "f", 0L)""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key, non empty field and positive increment" in {
          """import auto._
            |hashmaps.hincrby("a", "f", 1L)""".stripMargin should compile
        }
        "given non empty key, non empty field and negative increment" in {
          """import auto._
            |hashmaps.hincrby("a", "f", -1L)""".stripMargin should compile
        }
      }

    }

    "using hincrbyfloat" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |hashmaps.hincrbyfloat("", "f", 1d)""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """import auto._
            |hashmaps.hincrbyfloat("a", "", 1d)""".stripMargin shouldNot compile
        }
        "given zero increment" in {
          """import auto._
            |hashmaps.hincrbyfloat("a", "f", 0d)""".stripMargin shouldNot compile
        }
        "given NaN increment" in {
          """import auto._
            |hashmaps.hincrbyfloat("a", "f", java.lang.Double.NaN)""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key, non empty field and positive increment" in {
          """import auto._
            |hashmaps.hincrbyfloat("a", "f", 1d)""".stripMargin should compile
        }
        "given non empty key, non empty field and negative increment" in {
          """import auto._
            |hashmaps.hincrbyfloat("a", "f", -1d)""".stripMargin should compile
        }
      }

    }

    "using hkeys" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |hashmaps.hkeys("")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key" in {
          """import auto._
            |hashmaps.hkeys("a")""".stripMargin should compile
        }
      }

    }

    "using hlen" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |hashmaps.hlen("")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key" in {
          """import auto._
            |hashmaps.hlen("a")""".stripMargin should compile
        }
      }

    }

    "using hmget" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |hashmaps.hmget("", "f")""".stripMargin shouldNot compile
        }
        "given empty field" in {
          """import auto._
            |hashmaps.hmget("a", "")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and one non empty field" in {
          """import auto._
            |hashmaps.hmget[Int]("a", "f1")""".stripMargin should compile
        }
        "given non empty key and two non empty fields" in {
          """import auto._
            |hashmaps.hmget[Int, Int]("a", "f1", "f2")""".stripMargin should compile
        }
      }

    }
  }
}
