package laserdisc
package fs2
package parallel

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import laserdisc.auto._
import laserdisc.fs2.parallel.SetUpLaserdiscCatsByte.LaserdiscCatsByteSetUp
import laserdisc.fs2.parallel.channels.ByteInBitVectorOutChannel
import laserdisc.fs2.parallel.runtime.BenchRuntime.createNewRuntime
import laserdisc.fs2.parallel.testcases.TestCasesLaserdiscByteBitVector
import log.effect.fs2.SyncLogWriter
import log.effect.{LogLevels, LogWriter}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class LaserdiscCatsByteInBitVectorOutBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad1(setUp: LaserdiscCatsByteSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case1
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad2(setUp: LaserdiscCatsByteSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case2
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }
}

object SetUpLaserdiscCatsByte {
  @State(Scope.Benchmark)
  class LaserdiscCatsByteSetUp {
    implicit val logWriter: LogWriter[IO] =
      SyncLogWriter.consoleLogUpToLevel(LogLevels.Error)

    var runtime: IORuntime = _
    private val channel    = RedisAddress("localhost", 6379).toSocketAddress[IO] map { address =>
      ByteInBitVectorOutChannel[IO](address, receiveBufferSizeBytes = 8 * 1024 * 1024)
    }

    private[fs2] var testCases: TestCasesLaserdiscByteBitVector[IO] = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      runtime = createNewRuntime()
      channel
        .map(ch => testCases = TestCasesLaserdiscByteBitVector(ch))
        .unsafeRunSync()(runtime)
    }

    @TearDown
    def tearDown(): Unit =
      runtime.shutdown()
  }
}
