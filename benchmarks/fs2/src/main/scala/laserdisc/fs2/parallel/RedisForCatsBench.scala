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
import dev.profunktor.redis4cats.Redis
import dev.profunktor.redis4cats.effect.Log
import laserdisc.fs2.parallel.SetUpRedisForCats.RedisForCatsSetUp
import laserdisc.fs2.parallel.runtime.BenchRuntime.createNewRuntime
import laserdisc.fs2.parallel.testcases.RedisForCatsTestCases
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

class RedisForCatsBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad1(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case1
    val res = run.unsafeRunSync()(using setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad2(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case2
    val res = run.unsafeRunSync()(using setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad3(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case3
    val res = run.unsafeRunSync()(using setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad4(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case4
    val res = run.unsafeRunSync()(using setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad5(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case5
    val res = run.unsafeRunSync()(using setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad6(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case6
    val res = run.unsafeRunSync()(using setUp.runtime)

    bh.consume(res)
  }
}

object SetUpRedisForCats {

  @State(Scope.Benchmark)
  class RedisForCatsSetUp {
    import Log.Stdout.*

    var runtime: IORuntime = null

    private final val resource = Redis[IO].utf8("redis://localhost")

    private[fs2] var testCases: RedisForCatsTestCases[IO] = null
    private[fs2] var clientCleanUp: IO[Unit]              = null

    @Setup(Level.Trial)
    def setup(): Unit = {
      runtime = createNewRuntime()
      resource.allocated
        .map { case (rc, cu) =>
          testCases = RedisForCatsTestCases(rc)
          clientCleanUp = cu
        }
        .unsafeRunSync()(using runtime)
    }

    @TearDown(Level.Trial)
    def tearDown(): Unit = {
      clientCleanUp.unsafeRunSync()(using runtime)
      runtime.shutdown()
    }
  }
}
