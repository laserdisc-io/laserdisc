package laserdisc
package protocol

final class StringPSpec extends BaseSpec with EitherSyntax {
  import laserdisc.auto._

  test("The String protocol decoding the wrong type gives details about the decoding error") {
    val correct = strings.set("a", 23)
    assertLeftEquals(
      correct.decode(Arr(Bulk("wrong type"))) leftMap (_.getMessage),
      "RESP type(s) did not match: Arr(Bulk(wrong type))"
    )
  }

  test("The String protocol decoding the correct type with the wrong encoding gives details about the decoding error") {
    val correct = strings.set("a", 23)
    assertLeftEquals(
      correct.decode(Str("wrong")) leftMap (_.getMessage),
      "RESP type(s) of Str(wrong) matched but failed to deserialize correctly with error Read Error: expected Str(OK) but was Str(wrong)"
    )
  }
}
