package laserdisc

import laserdisc.all._
import laserdisc.protocol.RESP._

final class ConnectionPSpec extends BaseSpec {

  "A ConnectionP" when {

    "using auth" should {

      "fail to compile" when {
        "given empty password" in {
          """auth("")""".stripMargin shouldNot compile
        }
      }

      "compile successfully" when {
        "given non empty password" in forAll { key: Key =>
          val protocol = auth(key)

          protocol.encode shouldBe arr(bulk("AUTH"), bulk(key.value))
          protocol.decode(str("OK")).right.value shouldBe OK
        }
      }

    }

    "using echo" should {

      "compile successfully" when {
        "given string message" in forAll { s: String =>
          val protocol = echo(s)

          protocol.encode shouldBe arr(bulk("ECHO"), bulk(s))
          protocol.decode(bulk(s)).right.value shouldBe s
        }
        "given int message" in forAll { i: Int =>
          val protocol = echo(i)

          protocol.encode shouldBe arr(bulk("ECHO"), bulk(i.show))
          protocol.decode(bulk(i.show)).right.value shouldBe i
        }
      }

    }

    "using ping" should {

      "compile successfully" when {
        "given string message" in forAll { s: String =>
          val protocol = ping(s)

          protocol.encode shouldBe arr(bulk("PING"), bulk(s))
          protocol.decode(bulk(s)).right.value shouldBe s
        }
        "given int message" in forAll { i: Int =>
          val protocol = ping(i)

          protocol.encode shouldBe arr(bulk("PING"), bulk(i.show))
          protocol.decode(bulk(i.show)).right.value shouldBe i
        }
        "using val for empty message" in {
          val protocol = ping

          protocol.encode shouldBe arr(bulk("PING"))
          protocol.decode(str("PONG")).right.value shouldBe PONG
        }
      }

    }

    "using quit" should {

      "always compile" in {
        val protocol = quit

        protocol.encode shouldBe arr(bulk("QUIT"))
        protocol.decode(str("OK")).right.value shouldBe OK
      }

    }

    "using select" should {

      "fail to compile" when {
        "given out of bounds index (less than min: -1)" in {
          "select(-1)" shouldNot compile
        }
        "given out of bounds index (greater than max: 16)" in {
          "select(16)" shouldNot compile
        }
      }

      "compile successfully" when {
        "given valid index" in forAll { dbi: DbIndex =>
          val protocol = select(dbi)

          protocol.encode shouldBe arr(bulk("SELECT"), bulk(dbi.show))
          protocol.decode(str("OK")).right.value shouldBe OK
        }
      }

    }

    "using swapdb" should {

      "fail to compile" when {
        "given out of bounds indexes (index1 less than min: -1, index2 greater than max: 16)" in {
          "swapdb(-1, 16)" shouldNot compile
        }
        "given out of bounds indexes (index1 greater than max: 16, index2 less than min: -1)" in {
          "swapdb(16, -1)" shouldNot compile
        }
      }

      "compile successfully" when {
        "given valid indexes" in forAll { (dbi1: DbIndex, dbi2: DbIndex) =>
          val protocol = swapdb(dbi1, dbi2)

          protocol.encode shouldBe arr(bulk("SWAPDB"), bulk(dbi1.show), bulk(dbi2.show))
          protocol.decode(str("OK")).right.value shouldBe OK
        }
      }

    }
  }
}
