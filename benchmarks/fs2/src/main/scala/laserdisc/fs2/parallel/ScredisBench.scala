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

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import laserdisc.fs2.parallel.SetUpScredis.ScredisSetUp
import laserdisc.fs2.parallel.runtime.BenchRuntime.createNewRuntime
import laserdisc.fs2.parallel.testcases.TestCasesScredis
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import scredis.Redis

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class ScredisBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad1(setUp: ScredisSetUp, bh: Blackhole): Unit =
    bh.consume(Await.result(setUp.testCases.case1, Duration.Inf))

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad2(setUp: ScredisSetUp, bh: Blackhole): Unit =
    bh.consume(Await.result(setUp.testCases.case2, Duration.Inf))

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad3(setUp: ScredisSetUp, bh: Blackhole): Unit =
    bh.consume(Await.result(setUp.testCases.case3, Duration.Inf))

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad4(setUp: ScredisSetUp, bh: Blackhole): Unit =
    bh.consume(Await.result(setUp.testCases.case4, Duration.Inf))

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad5(setUp: ScredisSetUp, bh: Blackhole): Unit =
    bh.consume(Await.result(setUp.testCases.case5, Duration.Inf))

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad6(setUp: ScredisSetUp, bh: Blackhole): Unit =
    bh.consume(Await.result(setUp.testCases.case6, Duration.Inf))
}

object SetUpScredis {

  @State(Scope.Benchmark)
  class ScredisSetUp {

    var runtime: IORuntime = null

    private[fs2] var client: Redis               = null
    private[fs2] var testCases: TestCasesScredis = null

    @Setup(Level.Trial)
    def setup(): Unit = {
      runtime = createNewRuntime()
      client = Redis(host = "localhost", port = 6379)
      testCases = TestCasesScredis(client)(using runtime.compute)
    }

    @TearDown(Level.Trial)
    def tearDown(): Unit = {
      (IO.fromFuture(IO(client.quit())) >> IO.unit).unsafeRunSync()(using runtime)
      runtime.shutdown()
    }
  }
}
