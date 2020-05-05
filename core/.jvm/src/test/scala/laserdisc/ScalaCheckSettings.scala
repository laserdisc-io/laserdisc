package laserdisc

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

private[laserdisc] trait ScalaCheckSettings extends ScalaCheckPropertyChecks {
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = 200,
      maxDiscardedFactor = 10.0,
      minSize = 0,
      sizeRange = 100,
      workers = 16
    )
}
