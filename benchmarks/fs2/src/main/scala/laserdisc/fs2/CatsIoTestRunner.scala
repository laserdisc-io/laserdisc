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

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.flatMap.*
import laserdisc.auto.*
import laserdisc.fs2.parallel.runtime.BenchRuntime.fixedFixedRuntime
import laserdisc.fs2.parallel.testcases.TestCasesLaserdisc
import log.effect.fs2.SyncLogWriter.consoleLogUpToLevel
import log.effect.{LogLevels, LogWriter}

import scala.concurrent.duration.DurationInt

object CatsIoTestRunner {

  private implicit val logWriter: LogWriter[IO] = consoleLogUpToLevel(LogLevels.Error)

  def main(args: Array[String]): Unit = {

    val runFor = 15.minutes

    val task = IO.monotonic.flatMap { start =>
      RedisClient[IO].to("localhost", 6379).use { cl =>
        val cases = TestCasesLaserdisc[IO](cl)
        0.tailRecM[IO, Int] { count =>
          (cases.case1 >> IO.monotonic).map { current =>
            if (current - start >= runFor) count.asRight
            else (count + 1).asLeft
          }
        }
      }
    }

    println(s"Avg send/s: ${task.unsafeRunSync()(fixedFixedRuntime) * 24.0 / runFor.toMinutes / 60}")
    sys.exit()
  }
}
