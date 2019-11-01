package laserdisc
package protocol

final class ConnectionPSpec extends BaseSpec with ConnectionP {
  "The Connection protocol" when {
    "using auth" should {
      "roundtrip successfully" when {
        "given non empty password" in forAll("non empty password") { key: Key =>
          val protocol = auth(key)

          protocol.encode shouldBe Arr(Bulk("AUTH"), Bulk(key.value))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using echo" should {
      "roundtrip successfully" when {
        "given any String message" in forAll("string") { s: String =>
          val protocol = echo(s)

          protocol.encode shouldBe Arr(Bulk("ECHO"), Bulk(s))
          protocol.decode(Bulk(s)) onRight (_ shouldBe s)
        }
        "given any Int message" in forAll("int") { i: Int =>
          val protocol = echo(i)

          protocol.encode shouldBe Arr(Bulk("ECHO"), Bulk(i))
          protocol.decode(Bulk(i)) onRight (_ shouldBe i)
        }
      }
    }

    "using ping" should {
      "roundript successfully" when {
        "given any String message" in forAll("string") { s: String =>
          val protocol = ping(s)

          protocol.encode shouldBe Arr(Bulk("PING"), Bulk(s))
          protocol.decode(Bulk(s)) onRight (_ shouldBe s)
        }
        "given any Int message" in forAll("int") { i: Int =>
          val protocol = ping(i)

          protocol.encode shouldBe Arr(Bulk("PING"), Bulk(i))
          protocol.decode(Bulk(i)) onRight (_ shouldBe i)
        }
        "using val to get back PONG message" in {
          val protocol = ping

          protocol.encode shouldBe Arr(Bulk("PING"))
          protocol.decode(Str(PONG.value)) onRight (_ shouldBe PONG)
        }
      }
    }

    "using quit" should {
      "roundtrip successfully" in {
        val protocol = quit

        protocol.encode shouldBe Arr(Bulk("QUIT"))
        protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
      }
    }

    "using select" should {
      "roundtrip successfully" when {
        "given valid DbIndexes" in forAll("db index") { dbi: DbIndex =>
          val protocol = select(dbi)

          protocol.encode shouldBe Arr(Bulk("SELECT"), Bulk(dbi))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using swapdb" should {
      "roundtrip successfully" when {
        "given valid DbIndexes" in forAll("db index 1", "db index 2") { (dbi1: DbIndex, dbi2: DbIndex) =>
          val protocol = swapdb(dbi1, dbi2)

          protocol.encode shouldBe Arr(Bulk("SWAPDB"), Bulk(dbi1), Bulk(dbi2))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }
  }
}
