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
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

class RedisForCatsBench() {

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad1(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case1
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad2(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case2
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad3(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case3
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad4(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case4
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad5(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case5
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(48)
  def parallelLoad6(setUp: RedisForCatsSetUp, bh: Blackhole): Unit = {
    val run = setUp.testCases.case6
    val res = run.unsafeRunSync()(setUp.runtime)

    bh.consume(res)
  }
}

object SetUpRedisForCats {

  @State(Scope.Benchmark)
  class RedisForCatsSetUp {
    import Log.Stdout._

    var runtime: IORuntime = _

    val resource = Redis[IO].utf8("redis://localhost")

    private[fs2] var testCases: RedisForCatsTestCases[IO] = _
    private[fs2] var clientCleanUp: IO[Unit]              = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      runtime = createNewRuntime()
      resource.allocated
        .map { case (rc, cu) =>
          testCases = RedisForCatsTestCases(rc)
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
