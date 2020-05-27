package laserdisc
package refined

import org.scalacheck.Gen.listOf
import org.scalacheck.{Arbitrary, Gen, Prop}

package object types {
  private[types] val ints: Gen[Int]           = implicitly[Arbitrary[Int]].arbitrary
  private[types] val longs: Gen[Long]         = implicitly[Arbitrary[Long]].arbitrary
  private[types] val doubles: Gen[Double]     = implicitly[Arbitrary[Double]].arbitrary
  private[types] val strings: Gen[String]     = implicitly[Arbitrary[String]].arbitrary
  private[types] val intLists: Gen[List[Int]] = implicitly[Arbitrary[List[Int]]].arbitrary

  private[types] def keyLists(implicit k: Arbitrary[Key]): Gen[List[Key]] = listOf(k.arbitrary)

  private[types] def wightedKeyLists(implicit kv: Arbitrary[(Key, ValidDouble)]): Gen[List[(Key, ValidDouble)]] =
    listOf(kv.arbitrary)

  private[types] implicit val illegalArgumentException: IllegalArgumentException => Prop = _ => Prop.passed
}
