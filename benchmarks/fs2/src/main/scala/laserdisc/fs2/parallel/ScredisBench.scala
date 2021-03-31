package laserdisc
package fs2
package parallel

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import laserdisc.fs2.parallel.SetUpScredis.ScredisSetUp
import laserdisc.fs2.parallel.runtime.BenchRuntime.createNewRuntime
import laserdisc.fs2.parallel.testcases.TestCasesScredis
import org.openjdk.jmh.annotations._
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

    var runtime: IORuntime = _

    private[fs2] var client: Redis               = _
    private[fs2] var testCases: TestCasesScredis = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      runtime = createNewRuntime()
      client = Redis(host = "localhost", port = 6379)
      testCases = TestCasesScredis(client)(runtime.compute)
    }

    @TearDown(Level.Trial)
    def tearDown(): Unit = {
      (IO.fromFuture(IO(client.quit())) >> IO.unit).unsafeRunSync()(runtime)
      runtime.shutdown()
    }
  }
}
