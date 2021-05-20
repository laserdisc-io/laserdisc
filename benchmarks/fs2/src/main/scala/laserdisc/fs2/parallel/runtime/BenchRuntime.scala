package laserdisc
package fs2
package parallel
package runtime

import cats.effect.unsafe.{IORuntime, IORuntimeConfig, Scheduler}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, ThreadFactory, TimeUnit}
import scala.concurrent.ExecutionContext.fromExecutor

object BenchRuntime {

  private[fs2] def createNewRuntime(): IORuntime = {

    def namedThreadFactory(name: String) = new ThreadFactory {
      val count = new AtomicInteger(0)
      override def newThread(r: Runnable): Thread =
        new Thread(r, s"$name-${count.incrementAndGet()}")
    }

    val computeExecutor  = Executors.newFixedThreadPool(8, namedThreadFactory("bench-compute-pool"))
    val blockingExecutor = Executors.newFixedThreadPool(4, namedThreadFactory("bench-blocking-pool"))

    val scheduler = Scheduler.fromScheduledExecutor(Executors.newSingleThreadScheduledExecutor(namedThreadFactory("bench-scheduler")))

    IORuntime(
      compute = fromExecutor(computeExecutor),
      blocking = fromExecutor(blockingExecutor),
      scheduler = scheduler,
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
