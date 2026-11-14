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

import _root_.fs2.Stream
import cats.effect.IO
import cats.syntax.all.*
import laserdisc.fs2.FakeRedisServer.*
import laserdisc.redis_6_2.*
import laserdisc.auto.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

// connections ending while sends are queued: a send taken off the queue just as its connection ends, or waiting for a connection closed
// while idle, must still be answered, with a reply or a failure, and never be left waiting for its timeout
final class RedisClientChurnSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 5.minutes

  private val timeout = 20.seconds

  // what each send got: a reply, a failure, or nothing before its timeout
  private def outcomes(client: RedisClient[IO], sends: Int, spacing: Option[FiniteDuration]): IO[List[Either[Throwable, Option[String]]]] =
    (0 until sends).toList.parTraverse { i =>
      spacing.fold(IO.unit)(s => IO.sleep(s * (i % 20).toLong)) >> client.send(get[String]("k"), timeout).attempt.map(_.flatten)
    }

  private def check(results: List[Either[Throwable, Option[String]]], took: FiniteDuration): Unit = {
    val timedOut = results.collect { case Left(e: RequestTimedOut[?]) => e }
    assertEquals(timedOut.size, 0, s"${timedOut.size} of ${results.size} sends waited for their timeout")
    assert(took < timeout, s"took $took")
  }

  test("sends racing onto connections that end after a few replies are each answered or failed, none left waiting") {
    // the n-th connection answers between 1 and 4 commands, then closes
    val closing = FakeRedisServer((n, requests) => requests.take((n % 4 + 1).toLong).as(bulk("v")))
    closing.use { port =>
      (1 to 10).toList
        .traverse { _ =>
          RedisClient[IO].to("127.0.0.1", port).use(client => outcomes(client, 200, None).timed).map { case (took, results) =>
            check(results, took)
            results
          }
        }
        .map(all => assert(all.flatten.contains(Right(Some("v"))), "no send was answered"))
    }
  }

  test("sends arriving while connections are closed for being idle are each answered or failed, none left waiting") {
    // answers each command, and closes a connection that's been sent nothing for 30ms since its last command, as a server with a timeout
    // does with a client gone idle
    def closingWhenIdle(requests: Stream[IO, RESP]): Stream[IO, String] =
      Stream.eval(IO.ref(Option.empty[FiniteDuration])).flatMap { last =>
        val idle = Stream.awakeEvery[IO](5.millis).evalMap(_ => (IO.monotonic, last.get).mapN((now, l) => l.exists(now - _ > 30.millis)))
        requests.evalTap(_ => IO.monotonic.flatMap(now => last.set(Some(now)))).as(bulk("v")).interruptWhen(idle)
      }
    FakeRedisServer((_, requests) => closingWhenIdle(requests)).use { port =>
      (1 to 10).toList
        .traverse { _ =>
          // groups of sends 33ms apart, so that each comes just as the connection is closed for having been idle for 30ms
          RedisClient[IO].to("127.0.0.1", port).use(client => outcomes(client, 100, Some(33.millis)).timed).map { case (took, results) =>
            check(results, took)
            results
          }
        }
        .map(all => assert(all.flatten.contains(Right(Some("v"))), "no send was answered"))
    }
  }
}
