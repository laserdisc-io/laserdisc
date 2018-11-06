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

final class CirceSpec extends WordSpec with MustMatchers with PropertyChecks with OptionValues {

  private[this] sealed trait Foo                        extends Product with Serializable
  private[this] final case class Bar(x: Int)            extends Foo
  private[this] final case class Baz(y: String, z: Foo) extends Foo

  private[this] implicit val barDecoder: Decoder[Bar] = deriveDecoder
  private[this] implicit val barEncoder: Encoder[Bar] = deriveEncoder
  private[this] implicit val bazDecoder: Decoder[Baz] = deriveDecoder
  private[this] implicit val bazEncoder: Encoder[Baz] = deriveEncoder
  private[this] implicit val fooDecoder: Decoder[Foo] = deriveDecoder
  private[this] implicit val fooEncoder: Encoder[Foo] = deriveEncoder

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
  }
}
