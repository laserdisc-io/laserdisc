package laserdisc
package protocol

final class ConnectionPSpec extends BaseSpec {
  import auto._
  import connection._

  "A ConnectionP" when {

    "using auth" should {

      "roundtrip successfully" when {
        "given non empty password" in forAll { key: Key =>
          val protocol = auth(key)

          protocol.encode shouldBe Arr(Bulk("AUTH"), Bulk(key.value))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
      }
    }

    "using echo" should {

      "roundtrip successfully" when {
        "given any String message" in forAll { s: String =>
          val protocol = echo(s)

          protocol.encode shouldBe Arr(Bulk("ECHO"), Bulk(s))
          protocol.decode(Bulk(s)).right.value shouldBe s
        }
        "given any Int message" in forAll { i: Int =>
          val protocol = echo(i)

          protocol.encode shouldBe Arr(Bulk("ECHO"), Bulk(i))
          protocol.decode(Bulk(i)).right.value shouldBe i
        }
      }
    }

    "using ping" should {

      "roundript successfully" when {
        "given any String message" in forAll { s: String =>
          val protocol = ping(s)

          protocol.encode shouldBe Arr(Bulk("PING"), Bulk(s))
          protocol.decode(Bulk(s)).right.value shouldBe s
        }
        "given any Int message" in forAll { i: Int =>
          val protocol = ping(i)

          protocol.encode shouldBe Arr(Bulk("PING"), Bulk(i))
          protocol.decode(Bulk(i)).right.value shouldBe i
        }
        "using val to get back PONG message" in {
          val protocol = ping

          protocol.encode shouldBe Arr(Bulk("PING"))
          protocol.decode(Str(PONG.value)).right.value shouldBe PONG
        }
      }
    }

    "using quit" should {

      "roundtrip successfully" in {
        val protocol = quit

        protocol.encode shouldBe Arr(Bulk("QUIT"))
        protocol.decode(Str(OK.value)).right.value shouldBe OK
      }
    }

    "using select" should {

      "roundtrip successfully" when {
        "given valid DbIndexes" in forAll { dbi: DbIndex =>
          val protocol = select(dbi)

          protocol.encode shouldBe Arr(Bulk("SELECT"), Bulk(dbi))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
      }
    }

    "using swapdb" should {

      "roundtrip successfully" when {
        "given valid DbIndexes" in forAll { (dbi1: DbIndex, dbi2: DbIndex) =>
          val protocol = swapdb(dbi1, dbi2)

          protocol.encode shouldBe Arr(Bulk("SWAPDB"), Bulk(dbi1), Bulk(dbi2))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
      }
    }
  }
}
