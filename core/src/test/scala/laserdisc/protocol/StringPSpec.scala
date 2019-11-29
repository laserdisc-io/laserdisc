package laserdisc
package protocol

final class StringPSpec extends BaseSpec {
  import laserdisc.auto._

  "The String protocol" when {
    "decoding the correct type with the wrong encoding" should {
      "give details about the decoding error" in {
        val correct = strings.set("a", 23)
        correct.decode(Str("wrong")) onLeft { e =>
          e.getMessage shouldBe "RESP type(s) of Str(wrong) matched but failed to deserialize correctly with error TODO FILIPPO"
        }
      }
    }
  }
}
