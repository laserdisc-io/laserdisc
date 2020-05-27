package laserdisc

import munit.ScalaCheckSuite

private[laserdisc] trait ScalaCheckSettings extends ScalaCheckSuite {
  override val scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(200)
      .withMaxDiscardRatio(20)
      .disableLegacyShrinking
      .withWorkers(32)
      .withMinSize(0)
      .withMaxSize(150)
}
