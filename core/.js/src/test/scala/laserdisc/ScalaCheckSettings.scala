package laserdisc

import munit.ScalaCheckSuite

private[laserdisc] trait ScalaCheckSettings extends ScalaCheckSuite {
  override val scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(10)
      .withWorkers(8)
}
