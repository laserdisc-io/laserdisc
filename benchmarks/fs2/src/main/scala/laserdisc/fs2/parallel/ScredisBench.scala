package laserdisc
package fs2
package parallel

import java.util.concurrent.{Executors, TimeUnit}

import cats.effect.{ContextShift, IO}
import cats.syntax.flatMap._
import laserdisc.fs2.parallel.SetUpScredis.ScredisSetUp
import laserdisc.fs2.parallel.testcases.TestCasesScredis
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scredis.Redis

import scala.concurrent.ExecutionContext.fromExecutor
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

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

    private[this] val commandsService           = Executors.newFixedThreadPool(8)
    private[fs2] val ec: ExecutionContext       = fromExecutor(commandsService)
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)

    private[fs2] var client: Redis               = _
    private[fs2] var testCases: TestCasesScredis = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      client = Redis(host = "localhost", port = 6379)
      testCases = TestCasesScredis(client)(ec)
    }

    @TearDown(Level.Trial)
    def tearDown(): Unit =
      (IO.fromFuture(IO(client.quit())) >>
        IO.delay(commandsService.shutdown()) >>
        IO.delay(commandsService.awaitTermination(2, TimeUnit.SECONDS)) >>
        IO.unit).unsafeRunSync()
  }
}
