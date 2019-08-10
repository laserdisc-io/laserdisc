package laserdisc

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

final case class Foo(x: Int)
object Foo {
  implicit final val fooRead: Bulk ==> Foo = Read.instancePF { case Bulk(ToInt(i)) => Foo(i) }
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
