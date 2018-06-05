package laserdisc

import org.scalatest.{Matchers, WordSpecLike}

final class ConnectionPSpec extends WordSpecLike with Matchers {

  "A ConnectionP" when {

    "using auth" should {

      "fail to compile" when {
        "given empty password" in {
          """import auto._
            |connection.auth("")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty password" in {
          """import auto._
            |connection.auth("a")""".stripMargin should compile
        }
      }

    }

    "using echo" should {

      "fail to compile" when {
        "given empty message" in {
          """import auto._
            |connection.echo("")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty message" in {
          """import auto._
            |connection.echo("a")""".stripMargin should compile
        }
      }

    }

    "using ping" should {

      "fail to compile" when {
        "given empty message" in {
          """import auto._
            |connection.ping("")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty message" in {
          """import auto._
            |connection.ping("a")""".stripMargin should compile
        }
        "using val for empty message" in {
          """connection.ping""".stripMargin should compile
        }
      }

    }

    "using quit" should {

      "always compile" in {
        """connection.quit""".stripMargin should compile
      }

    }

    "using select" should {

      "fail to compile" when {
        "given out of bounds index (less than min: -1)" in {
          """import auto._
            |connection.select(-1)""".stripMargin shouldNot compile
        }
        "given out of bounds index (greater than max: 16)" in {
          """import auto._
            |connection.select(16)""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given valid index (min: 0)" in {
          """import auto._
            |connection.select(0)""".stripMargin should compile
        }
        "given valid index (max: 15)" in {
          """import auto._
            |connection.select(15)""".stripMargin should compile
        }
      }

    }

    "using swapdb" should {

      "fail to compile" when {
        "given out of bounds indexes (index1 less than min: -1, index2 greater than max: 16)" in {
          """import auto._
            |connection.swapdb(-1, 16)""".stripMargin shouldNot compile
        }
        "given out of bounds indexes (index1 greater than max: 16, index2 less than min: -1)" in {
          """import auto._
            |connection.swapdb(16, -1)""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given valid indexes (index1 min: 0, index2 max: 15)" in {
          """import auto._
            |connection.swapdb(0, 15)""".stripMargin should compile
        }
        "given valid indexes (index1 max: 15, index2 min: 0)" in {
          """import auto._
            |connection.swapdb(15, 0)""".stripMargin should compile
        }
      }

    }
  }
}
