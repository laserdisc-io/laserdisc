package laserdisc
package fs2

import cats.effect.IO
import cats.syntax.flatMap._
import laserdisc.auto._
import laserdisc.fs2.parallel.runtime.BenchRuntime.fixedFixedRuntime
import laserdisc.fs2.parallel.testcases.TestCasesLaserdisc
import log.effect.fs2.SyncLogWriter.consoleLogUpToLevel
import log.effect.{LogLevels, LogWriter}

import scala.concurrent.duration.DurationInt

object CatsIoTestRunner {

  private[this] implicit val logWriter: LogWriter[IO] = consoleLogUpToLevel(LogLevels.Error)

  def main(args: Array[String]): Unit = {

    val runFor = 2.minutes
    val task = IO.monotonic >>= { start =>
      RedisClient[IO].to("localhost", 6379).use { cl =>
        val cases = TestCasesLaserdisc[IO](cl)
        def loop(count: Long): IO[Long] =
          cases.case1 >> IO.monotonic >>= { current =>
            if (current - start >= runFor) IO.pure(count)
            else loop(count + 1)
          }

        loop(0)
      }
    }

    println(s"Avg send/s: ${task.unsafeRunSync()(fixedFixedRuntime) * 24.0 / runFor.toMinutes / 60}")
    sys.exit()
  }
}
