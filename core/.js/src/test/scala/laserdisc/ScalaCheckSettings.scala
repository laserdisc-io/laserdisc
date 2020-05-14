package laserdisc

import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

private[laserdisc] trait ScalaCheckSettings extends ScalaCheckPropertyChecks {
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = 10,
      maxDiscardedFactor = 10.0,
      workers = 16
    )
}
