package laserdisc

import munit.ScalaCheckSuite

private[laserdisc] trait ScalaCheckSettings extends ScalaCheckSuite {
  override val scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(500)
      .withMaxDiscardRatio(20)
      .disableLegacyShrinking
      .withWorkers(16)
      .withMinSize(0)
      .withMaxSize(300)
}
