package laserdisc
package fs2
package parallel

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import laserdisc.auto._
import laserdisc.fs2.parallel.SetUpLaserdiscCats.LaserdiscCatsSetUp
import laserdisc.fs2.parallel.runtime.BenchRuntime.createNewRuntime
import laserdisc.fs2.parallel.testcases.TestCasesLaserdisc
import log.effect.fs2.SyncLogWriter
import log.effect.{LogLevels, LogWriter}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class LaserdiscCatsBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad1(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case1
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad2(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case2
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad3(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case3
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad4(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case4
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad5(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case5
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad6(setUp: LaserdiscCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case6
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }
}

object SetUpLaserdiscCats {

  @State(Scope.Benchmark)
  class LaserdiscCatsSetUp {
    implicit val logWriter: LogWriter[IO] =
      SyncLogWriter.consoleLogUpToLevel(LogLevels.Error)

    var runtime: IORuntime = _

    val resource = RedisClient[IO].to("localhost", 6379)

    private[fs2] var testCases: TestCasesLaserdisc[IO] = _
    private[fs2] var clientCleanUp: IO[Unit]           = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      runtime = createNewRuntime()
      resource.allocated
        .map { case (rc, cu) =>
          testCases = TestCasesLaserdisc(rc)
          clientCleanUp = cu
        }
        .unsafeRunSync()(runtime)
    }

    @TearDown(Level.Trial)
    def tearDown(): Unit = {
      clientCleanUp.unsafeRunSync()(runtime)
      runtime.shutdown()
    }
  }
}
