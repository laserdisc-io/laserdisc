package laserdisc
package interop

import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import laserdisc.interop.circe._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}

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

final class CirceSpec extends CirceCheckSettings with EitherTestSyntax {
  private[this] val barGen: Gen[Bar] = Arbitrary.arbitrary[Int].map(Bar.apply)
  private[this] val bazGen: Gen[Baz] = for {
    s   <- Arbitrary.arbitrary[String]
    foo <- fooGen
  } yield Baz(s, foo)
  private[this] def fooGen: Gen[Foo] = Gen.oneOf(barGen, bazGen)

  private[this] implicit val barArbitrary: Arbitrary[Bar] = Arbitrary(barGen)
  private[this] implicit val bazArbitrary: Arbitrary[Baz] = Arbitrary(bazGen)

  property("Circe interop roundtrips with no errors when handling a simple type") {
    forAll { bar: Bar =>
      assertEquals(Read[Bulk, Bar].read(Bulk(bar)), bar)
    }
  }

  property("Circe interop roundtrips with no errors when handling a recursive type") {
    forAll { baz: Baz =>
      assertEquals(Read[Bulk, Baz].read(Bulk(baz)), baz)
    }
  }

  test("Circe interop fails to decode when handling a json that does not respect the contract") {
    Read[Bulk, Bar].read(
      Bulk("""{"i": null}""")
    ) onLeft (e => assertEquals(e.message, "DecodingFailure at .x: Missing required field"))
  }
}
