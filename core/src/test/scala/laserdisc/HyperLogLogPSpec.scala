package laserdisc

import org.scalatest.{Matchers, WordSpecLike}

final class HyperLogLogPSpec extends WordSpecLike with Matchers {

  "A HyperLogLogP" when {

    "using pfadd" should {

      "fail to compile" when {
        "given empty key" in {
          """import auto._
            |hyperloglog.pfadd("", "e")""".stripMargin shouldNot compile
        }
        "given empty element" in {
          """import auto._
            |hyperloglog.pfadd("a", "")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty key and one non empty element" in {
          """import auto._
            |hyperloglog.pfadd("a", "e1")""".stripMargin should compile
        }
        "given non empty key and two non empty elements" in {
          """import auto._
            |hyperloglog.pfadd("a", "e1", "e2")""".stripMargin should compile
        }
      }

    }

    "using pfcount" should {

      "fail to compile" when {
        "given one empty key" in {
          """import auto._
            |hyperloglog.pfcount("")""".stripMargin shouldNot compile
        }
        "given two empty keys" in {
          """import auto._
            |hyperloglog.pfcount("", "")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given one non empty key" in {
          """import auto._
            |hyperloglog.pfcount("a")""".stripMargin should compile
        }
        "given two non empty keys" in {
          """import auto._
            |hyperloglog.pfcount("a", "b")""".stripMargin should compile
        }
      }

    }
  }
}
