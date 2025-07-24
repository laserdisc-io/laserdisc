package laserdisc
package fs2
package parallel

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import laserdisc.auto._
import laserdisc.fs2.parallel.SetUpLaserdiscCatsResp.LaserdiscCatsRespSetUp
import laserdisc.fs2.parallel.runtime.BenchRuntime.createNewRuntime
import laserdisc.fs2.parallel.testcases.TestCasesLaserdiscResp
import log.effect.fs2.SyncLogWriter
import log.effect.{LogLevels, LogWriter}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class LaserdiscCatsRespInAndOutBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad1(setUp: LaserdiscCatsRespSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case1
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad2(setUp: LaserdiscCatsRespSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case2
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }
}

object SetUpLaserdiscCatsResp {

  @State(Scope.Benchmark)
  class LaserdiscCatsRespSetUp {
    implicit val logWriter: LogWriter[IO] =
      SyncLogWriter.consoleLogUpToLevel(LogLevels.Error)

    var runtime: IORuntime = _
    private val channel    = RedisAddress("localhost", 6379).toSocketAddress[IO].map { address =>
      RedisChannel[IO](address, receiveBufferSizeBytes = 8 * 1024 * 1024)
    }

    private[fs2] var testCases: TestCasesLaserdiscResp[IO] = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      runtime = createNewRuntime()
      channel
        .map(ch => testCases = TestCasesLaserdiscResp(ch))
        .unsafeRunSync()(runtime)
    }

    @TearDown
    def tearDown(): Unit =
      runtime.shutdown()
  }
}
