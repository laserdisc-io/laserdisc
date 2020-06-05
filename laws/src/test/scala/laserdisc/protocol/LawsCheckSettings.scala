package laserdisc
package protocol

import munit.ScalaCheckSuite

private[protocol] trait LawsCheckSettings extends ScalaCheckSuite {
  override val scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(200)
      .withMaxDiscardRatio(20)
      .disableLegacyShrinking
      .withWorkers(32)
      .withMinSize(0)
      .withMaxSize(150)
}
