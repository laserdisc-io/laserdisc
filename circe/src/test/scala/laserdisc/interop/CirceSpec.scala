package laserdisc
package interop

import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import laserdisc.interop.circe._
import laserdisc.protocol.RESP.bulk
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, OptionValues, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

sealed trait Foo                        extends Product with Serializable
final case class Bar(x: Int)            extends Foo
final case class Baz(y: String, z: Foo) extends Foo

object Foo {
  implicit val decoder: Decoder[Foo] = deriveDecoder
  implicit val encoder: Encoder[Foo] = deriveEncoder
}
object Bar {
  implicit val decoder: Decoder[Bar] = deriveDecoder
  implicit val encoder: Encoder[Bar] = deriveEncoder
}
object Baz {
  implicit val decoder: Decoder[Baz] = deriveDecoder
  implicit val encoder: Encoder[Baz] = deriveEncoder
}

final class CirceSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks with OptionValues {
  private[this] val barGen: Gen[Bar] = Arbitrary.arbitrary[Int].map(Bar.apply)
  private[this] val bazGen: Gen[Baz] = for {
    s   <- Arbitrary.arbitrary[String]
    foo <- fooGen
  } yield Baz(s, foo)
  private[this] def fooGen: Gen[Foo] = Gen.oneOf(barGen, bazGen)

  private[this] implicit val barArbitrary: Arbitrary[Bar] = Arbitrary(barGen)
  private[this] implicit val bazArbitrary: Arbitrary[Baz] = Arbitrary(bazGen)

  "Circe interop" when {

    "handling a simple type" should {
      "round-trip with no errors" in forAll { bar: Bar =>
        Read[Bulk, Bar].read(bulk(Show[Bar].show(bar))).value shouldBe bar
      }
    }

    "handling a recursive type" should {
      "round-trip with no errors" in forAll { baz: Baz =>
        Read[Bulk, Baz].read(bulk(Show[Baz].show(baz))).value shouldBe baz
      }
    }

    "handling a json that does not respect the contract" should {
      "fail to decode" in {
        Read[Bulk, Bar].read(bulk("""{"i": null}""")) shouldBe empty
      }
    }

    "handling an invalid json" should {
      "fail to decode" in {
        Read[Bulk, Bar].read(bulk("{")) shouldBe empty
      }
    }
  }
}
