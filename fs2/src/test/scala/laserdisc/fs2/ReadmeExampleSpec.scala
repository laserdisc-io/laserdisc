/*
 * Copyright (c) 2018-2025 LaserDisc
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

package laserdisc.fs2

import munit.CatsEffectSuite
import cats.effect.IO
import log.effect.{LogLevel, LogWriter}
import log.effect.internal.Show

final class ReadmeExampleSpec extends CatsEffectSuite {

  final val logged                   = collection.mutable.ListBuffer.empty[String]
  final val logWriter: LogWriter[IO] = new LogWriter[IO] {
    override final def write[A: Show](level: LogLevel, a: =>A): IO[Unit] = IO(logged.append(implicitly[Show[A]].show(a))).void
  }

  test("The readme example gives the expected output and logs when a LogWriter is in scope") {

    import laserdisc._
    import laserdisc.all._
    import laserdisc.auto._
    import laserdisc.fs2._

    def redisTest(implicit log: LogWriter[IO]): IO[Unit] =
      RedisClient[IO].to("localhost", 6379).use { client =>
        client
          .send(
            set("a", 23),
            set("b", 55),
            get[PosInt]("b"),
            get[PosInt]("a")
          )
          .flatMap {
            case (Right(OK), Right(OK), Right(Some(getOfb)), Right(Some(getOfa))) if getOfb.value == 55 && getOfa.value == 23 =>
              log.info("yay!")
            case other =>
              log.error(s"something went terribly wrong $other") >>
                IO.raiseError(new RuntimeException("boom"))
          }
      }

    redisTest(logWriter).map { _ =>
      assert(logged.contains("Starting connection"))
      assert(logged.contains("Connected to server localhost:6379"))
      assert(logged.contains("sending Arr(Bulk(SET),Bulk(a),Bulk(23))"))
      assert(logged.contains("receiving Str(OK)"))
      assert(logged.contains("sending Arr(Bulk(SET),Bulk(b),Bulk(55))"))
      assert(logged.contains("receiving Str(OK)"))
      assert(logged.contains("sending Arr(Bulk(GET),Bulk(b))"))
      assert(logged.contains("receiving Bulk(55)"))
      assert(logged.contains("sending Arr(Bulk(GET),Bulk(a))"))
      assert(logged.contains("receiving Bulk(23)"))
      assert(logged.contains("yay!"))
      assert(logged.contains("Shutting down connection"))
      assert(logged.contains("Shutdown complete"))
      assert(logged.contains("Connection terminated: No issues"))
    }
  }
}
