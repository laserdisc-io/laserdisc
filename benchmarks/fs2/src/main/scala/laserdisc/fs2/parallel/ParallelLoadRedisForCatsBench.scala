package laserdisc
package fs2
package parallel

import java.util.concurrent.{Executors, TimeUnit}

import cats.effect.{ContextShift, IO, Timer}
import cats.syntax.flatMap._
import dev.profunktor.redis4cats.Redis
import dev.profunktor.redis4cats.effect.Log
import laserdisc.fs2.parallel.SetUpRedisForCats.RedisForCatsSetUp
import laserdisc.fs2.parallel.testcases.RedisForCatsTestCases
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor

class ParallelLoadRedisForCatsBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad1(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case1
    val res = run.unsafeRunSync()

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad2(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case2
    val res = run.unsafeRunSync()

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad3(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case3
    val res = run.unsafeRunSync()

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad4(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case4
    val res = run.unsafeRunSync()

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad5(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case5
    val res = run.unsafeRunSync()

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad6(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case6
    val res = run.unsafeRunSync()

    bh.consume(res)
  }
}

object SetUpRedisForCats {

  @State(Scope.Benchmark)
  class RedisForCatsSetUp {
    import Log.Stdout._

    private[this] val commandsService           = Executors.newFixedThreadPool(8)
    private[this] val ec: ExecutionContext      = fromExecutor(commandsService)
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)
    implicit val timer: Timer[IO]               = IO.timer(ec)

    val resource = Redis[IO].utf8("redis://localhost")

    private[fs2] var testCases: RedisForCatsTestCases[IO] = _
    private[fs2] var clientCleanUp: IO[Unit]              = _

    @Setup(Level.Trial)
    def setup(): Unit =
      resource.allocated
        .map { case (rc, cu) =>
          testCases = RedisForCatsTestCases(rc)
          clientCleanUp = cu
        }
        .unsafeRunSync()

    @TearDown(Level.Trial)
    def tearDown(): Unit =
      (clientCleanUp >>
        IO.delay(commandsService.shutdown()) >>
        IO.delay(commandsService.awaitTermination(2, TimeUnit.SECONDS)) >> IO.unit).unsafeRunSync()
  }
}
