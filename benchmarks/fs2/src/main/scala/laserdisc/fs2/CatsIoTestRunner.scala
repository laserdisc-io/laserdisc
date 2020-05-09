package laserdisc
package fs2

import java.util.concurrent.{Executors, TimeUnit}

import cats.effect.{ContextShift, IO, Timer}
import cats.syntax.flatMap._
import laserdisc.auto._
import log.effect.fs2.SyncLogWriter.consoleLogUpToLevel
import log.effect.{LogLevels, LogWriter}

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor

object CatsIoTestRunner extends TestCases {

  private[this] val ec: ExecutionContext = fromExecutor(Executors.newFixedThreadPool(8))

  private[this] implicit val timer: Timer[IO]               = IO.timer(ec)
  private[this] implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)
  private[this] implicit val logWriter: LogWriter[IO]       = consoleLogUpToLevel(LogLevels.Error)

  def main(args: Array[String]): Unit = {

    val task = timer.clock.monotonic(TimeUnit.MINUTES) >>= { start: Long =>
      RedisClient.to("localhost", 6379).use { cl =>
        def loop(count: Long): IO[Long] =
          case1(cl) >> timer.clock.monotonic(TimeUnit.MINUTES) >>= { current =>
            if (current - start >= 2) IO.pure(count)
            else loop(count + 1)
          }

        loop(0)
      }
    }

    println(s"Avg send/s: ${task.unsafeRunSync() * 24.0 / 2 / 60}")
    sys.exit()
  }
}
