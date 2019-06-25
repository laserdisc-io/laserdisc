package laserdisc
package protocol

final class ConnectionPSpec extends BaseSpec {
  import auto._
  import connection._
  import RESP._
  import show._

  "A ConnectionP" when {

    "using auth" should {

      "roundtrip successfully" when {
        "given non empty password" in forAll { key: Key =>
          val protocol = auth(key)

          protocol.encode shouldBe arr(bulk("AUTH"), bulk(key.value))
          protocol.decode(str(OK.value)).right.value shouldBe OK
        }
      }
    }

    "using echo" should {

      "roundtrip successfully" when {
        "given any String message" in forAll { s: String =>
          val protocol = echo(s)

          protocol.encode shouldBe arr(bulk("ECHO"), bulk(s))
          protocol.decode(bulk(s)).right.value shouldBe s
        }
        "given any Int message" in forAll { i: Int =>
          val protocol = echo(i)

          protocol.encode shouldBe arr(bulk("ECHO"), bulk(i.show))
          protocol.decode(bulk(i.show)).right.value shouldBe i
        }
      }
    }

    "using ping" should {

      "roundript successfully" when {
        "given any String message" in forAll { s: String =>
          val protocol = ping(s)

          protocol.encode shouldBe arr(bulk("PING"), bulk(s))
          protocol.decode(bulk(s)).right.value shouldBe s
        }
        "given any Int message" in forAll { i: Int =>
          val protocol = ping(i)

          protocol.encode shouldBe arr(bulk("PING"), bulk(i.show))
          protocol.decode(bulk(i.show)).right.value shouldBe i
        }
        "using val to get back PONG message" in {
          val protocol = ping

          protocol.encode shouldBe arr(bulk("PING"))
          protocol.decode(str(PONG.value)).right.value shouldBe PONG
        }
      }
    }

    "using quit" should {

      "roundtrip successfully" in {
        val protocol = quit

        protocol.encode shouldBe arr(bulk("QUIT"))
        protocol.decode(str(OK.value)).right.value shouldBe OK
      }
    }

    "using select" should {

      "roundtrip successfully" when {
        "given valid DbIndexes" in forAll { dbi: DbIndex =>
          val protocol = select(dbi)

          protocol.encode shouldBe arr(bulk("SELECT"), bulk(dbi.show))
          protocol.decode(str(OK.value)).right.value shouldBe OK
        }
      }
    }

    "using swapdb" should {

      "roundtrip successfully" when {
        "given valid DbIndexes" in forAll { (dbi1: DbIndex, dbi2: DbIndex) =>
          val protocol = swapdb(dbi1, dbi2)

          protocol.encode shouldBe arr(bulk("SWAPDB"), bulk(dbi1.show), bulk(dbi2.show))
          protocol.decode(str(OK.value)).right.value shouldBe OK
        }
      }
    }
  }
}
