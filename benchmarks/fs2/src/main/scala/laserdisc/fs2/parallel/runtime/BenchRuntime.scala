package laserdisc
package fs2
package parallel
package runtime

import cats.effect.unsafe.IORuntime.global
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}

import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.ExecutionContext.fromExecutor

object BenchRuntime {

  private[fs2] def createNewRuntime(): IORuntime = {
    val computeExecutor  = Executors.newFixedThreadPool(8)
    val blockingExecutor = Executors.newFixedThreadPool(4)

    IORuntime(
      compute = fromExecutor(computeExecutor),
      blocking = fromExecutor(blockingExecutor),
      scheduler = global.scheduler,
      shutdown = () => {
        computeExecutor.shutdown()
        blockingExecutor.shutdown()
        val _ = computeExecutor.awaitTermination(2, TimeUnit.SECONDS)
        val _ = blockingExecutor.awaitTermination(2, TimeUnit.SECONDS)
      },
      config = IORuntimeConfig()
    )
  }

  private[fs2] val fixedFixedRuntime = createNewRuntime()
}
