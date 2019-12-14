package laserdisc
package interop

import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import laserdisc.interop.circe._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, WordSpec}
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

final class CirceSpec extends WordSpec with Matchers with ScalaCheckPropertyChecks with EitherTestSyntax {
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
      "roundtrip with no errors" in forAll { bar: Bar =>
        Read[Bulk, Bar].read(Bulk(bar)) onRight (_ shouldBe bar)
      }
    }

    "handling a recursive type" should {
      "roundtrip with no errors" in forAll { baz: Baz =>
        Read[Bulk, Baz].read(Bulk(baz)) onRight (_ shouldBe baz)
      }
    }

    "handling a json that does not respect the contract" should {
      "fail to decode" in {
        Read[Bulk, Bar].read(Bulk("""{"i": null}""")) onLeft (_.message shouldBe "DecodingFailure at .x: Attempt to decode value on failed cursor")
      }
    }
  }
}
