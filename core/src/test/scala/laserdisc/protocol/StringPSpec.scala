package laserdisc
package protocol

final class StringPSpec extends BaseSpec {
  import laserdisc.auto._

  "The String protocol" when {
    "decoding the wrong type" should {
      "give details about the decoding error" in {
        val correct = strings.set("a", 23)
        correct.decode(Arr(Bulk("wrong type"))) onLeft { e =>
          e.getMessage shouldBe "RESP type(s) did not match: Arr(Bulk(wrong type))"
        }
      }
    }

    "decoding the correct type with the wrong encoding" should {
      "give details about the decoding error" in {
        val correct = strings.set("a", 23)
        correct.decode(Str("wrong")) onLeft { e =>
          e.getMessage shouldBe "RESP type(s) of Str(wrong) matched but failed to deserialize correctly with error Read Error: expected Str(OK) but was Str(wrong)"
        }
      }
    }
  }
}
