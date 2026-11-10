/*
 * Copyright (c) 2018-2026 LaserDisc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package laserdisc
package fs2
package parallel
package runtime

import cats.effect.unsafe.{IORuntime, IORuntimeConfig, Scheduler}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, ThreadFactory, TimeUnit}
import scala.annotation.nowarn
import scala.concurrent.ExecutionContext.fromExecutor

object BenchRuntime {

  private[fs2] def createNewRuntime(): IORuntime = {

    def namedThreadFactory(name: String) = new ThreadFactory {
      val count                                   = new AtomicInteger(0)
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
        @nowarn val b1 = computeExecutor.awaitTermination(2, TimeUnit.SECONDS)
        @nowarn val b2 = blockingExecutor.awaitTermination(2, TimeUnit.SECONDS)
      },
      config = IORuntimeConfig()
    )
  }

  private[fs2] val fixedFixedRuntime = createNewRuntime()
}
