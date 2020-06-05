package laserdisc
package interop

import munit.ScalaCheckSuite

private[laserdisc] trait CirceCheckSettings extends ScalaCheckSuite {
  override val scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(10)
      .withWorkers(8)
}
