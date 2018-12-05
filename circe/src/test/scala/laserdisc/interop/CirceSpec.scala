package laserdisc
package interop

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import laserdisc.interop.circe._
import laserdisc.protocol.NonNullBulkString
import laserdisc.protocol.RESP.bulk
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{MustMatchers, OptionValues, WordSpec}

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

final class CirceSpec extends WordSpec with MustMatchers with PropertyChecks with OptionValues {
  private[this] val barGen: Gen[Bar] = Arbitrary.arbitrary[Int].map(Bar.apply)
  private[this] val bazGen: Gen[Baz] = for {
    s   <- Arbitrary.arbitrary[String]
    foo <- fooGen
  } yield Baz(s, foo)
  private[this] def fooGen: Gen[Foo] = Gen.oneOf(barGen, bazGen)

  private[this] implicit val barArbitrary: Arbitrary[Bar] = Arbitrary(barGen)
  private[this] implicit val bazArbitrary: Arbitrary[Baz] = Arbitrary(bazGen)

  "Circe interop" when {

    "handling a simple type" must {
      "round-trip with no errors" in forAll { bar: Bar =>
        Read[NonNullBulkString, Bar].read(bulk(Show[Bar].show(bar))).value mustBe bar
      }
    }

    "handling a recursive type" must {
      "round-trip with no errors" in forAll { baz: Baz =>
        Read[NonNullBulkString, Baz].read(bulk(Show[Baz].show(baz))).value mustBe baz
      }
    }

    "handling a json that does not respect the contract" must {
      "fail to decode" in {
        Read[NonNullBulkString, Bar].read(bulk("""{"i": null}"""))
      }
    }

    "handling an invalid json" must {
      "fail to decode" in {
        Read[NonNullBulkString, Bar].read(bulk("{"))
      }
    }
  }
}
