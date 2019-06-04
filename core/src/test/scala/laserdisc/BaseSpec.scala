package laserdisc

import eu.timepit.refined.api._
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.scalacheck.reftype.arbitraryRefType
import eu.timepit.refined.scalacheck.{CollectionInstancesBinCompat1, NumericInstances, StringInstances}
import org.scalacheck.Gen.Choose
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{EitherValues, Matchers, OptionValues, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import shapeless._0

abstract class BaseSpec
    extends WordSpec
    with Matchers
    with EitherValues
    with OptionValues
    with ScalaCheckPropertyChecks
    with CollectionInstancesBinCompat1
    with NumericInstances
    with StringInstances {

  implicit final val geoHashArbitrary: Arbitrary[GeoHash] =
    arbitraryRefType(Gen.listOfN(11, Gen.alphaLowerChar).map(_.mkString))

    implicit final def nonZeroArbitrary[T: Numeric: Choose](implicit min: Min[T], max: Max[T]): Arbitrary[T Refined Not[Equal[_0]]] =
    arbitraryRefType(Gen.chooseNum(min.min, max.max).filter(_ != 0))

  implicit final val nonZeroDoubleArbitrary: Arbitrary[NonZeroDouble] =
    arbitraryRefType(Gen.choose(Double.MinValue, Double.MaxValue).filter(_ != 0.0d))

  implicit final val positionArbitrary: Arbitrary[protocol.GeoP.Position] = Arbitrary {
    for {
      m    <- Arbitrary.arbitrary[Key]
      lat  <- Arbitrary.arbitrary[Latitude]
      long <- Arbitrary.arbitrary[Longitude]
    } yield protocol.GeoP.Position(m, lat, long)
  }

  implicit final val validDoubleArbitrary: Arbitrary[ValidDouble] =
    arbitraryRefType(Gen.choose(Double.MinValue, Double.MaxValue).filter(_ != Double.NaN))
}
