package laserdisc
package protocol

import shapeless._

final class ArrSpec extends BaseSpec {
  import laserdisc.auto._

  "decoding the correct type Arr(Bulk) ==> Seq[A] with the wrong encoding" should {
    "give details about the decoding error" in {
      def protocol = Protocol("CUSTOM", _: String :: HNil).as[Arr, Seq[Long]]
      val request  = "id" :: HNil
      val response = Arr(Num(1L), Num(2L), Num(3L), Num(4L))

      protocol(request).decode(response) onLeft { e =>
        e.getMessage shouldBe "RESP type(s) of Arr(Num(1),Num(2),Num(3),Num(4)) matched but failed to deserialize correctly with error Arr(Bulk) ==> Seq[A] error at element 1: Unexpected for Bulk. Was Num(4)"
      }
    }
  }

  "decoding the correct type Arr(Bulk) ==> Seq[Option[A]] with the wrong encoding" should {
    "give details about the decoding error" in {
      def protocol = Protocol("CUSTOM", _: String :: HNil).as[Arr, Seq[Option[Long]]]
      val request  = "id" :: HNil
      val response = Arr(Num(1L), NullBulk, Num(3L), NullBulk)

      protocol(request).decode(response) onLeft { e =>
        e.getMessage shouldBe "RESP type(s) of Arr(Num(1),NullBulk,Num(3),NullBulk) matched but failed to deserialize correctly with error Arr(Bulk) ==> Seq[Option[A]] error at element 2: Unexpected for Bulk. Was Num(3)"
      }
    }
  }
}
