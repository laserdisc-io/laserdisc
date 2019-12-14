package laserdisc

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

final case class Foo(x: Int)
object Foo {
  implicit final val fooRead: Bulk ==> Foo = Read.instance {
    case Bulk(ToInt(i)) => Right(Foo(i))
    case Bulk(other)    => Left(RESPDecErr(s"Boom: $other"))
  }
}

final case class Bar(x: String)

final case class Baz(f1: Int, f2: String)
object Baz {
  implicit final val bazArb: Arbitrary[Baz] = Arbitrary {
    for {
      i <- arbitrary[Int]
      s <- arbitrary[String]
    } yield Baz(i, s)
  }
}
