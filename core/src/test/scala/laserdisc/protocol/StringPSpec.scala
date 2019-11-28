package laserdisc
package protocol

final class StringPSpec extends BaseSpec {
  import laserdisc.auto._

  "The String protocol" when {
    "aaaaaa" should {
      "bbbbbbbb" in {
        val correct = strings.set("a", 23)

        correct.encode shouldBe Arr(Bulk("SET"), Bulk("a"), Bulk(23))
        correct.decode(Str("wrong")) onRight (_ shouldBe "RESP type(s) of Str(wrong) matched but failed to deserialize correctly with error TODO FILIPPO")
      }
    }
  }
}
