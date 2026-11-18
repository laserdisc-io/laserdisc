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
import laserdisc.auto.*
import laserdisc.fs2.FakeRedisServer.*
import laserdisc.redis_6_2.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

// closing a client while it's in use: what's been sent, or queued, must end at once, not wait for a timeout or forever
final class RedisClientReleaseSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 5.minutes

  test("sends racing the client being closed each end at once, with a reply or ClientTerminated, never by timing out") {
    // answers each command after a moment, so that some are in flight, some queued and some on their way when the client closes
    val slow = FakeRedisServer((_, requests) => requests.evalMap(_ => IO.sleep(1.milli).as(bulk("v"))))
    slow.use { port =>
      (1 to 30).toList.traverse_ { round =>
        RedisClient[IO].to("127.0.0.1", port).allocated.flatMap { case (client, close) =>
          val sends = (1 to 200).toList.parTraverse(_ => client.send(get[String]("k"), 20.seconds).attempt.map(_.flatten))
          (sends, IO.sleep((round % 10).millis) >> close).parTupled.timed.map { case (took, (results, _)) =>
            val timedOut = results.count(_.left.exists(_.isInstanceOf[RequestTimedOut[?]]))
            assertEquals(timedOut, 0, s"round $round: $timedOut sends waited for their timeout")
            assert(took < 10.seconds, s"round $round took $took")
          }
        }
      }
    }
  }

  test("cancelling a forever command while the dedicated client is being closed ends, instead of waiting forever") {
    // reads commands and never answers, so the forever command stays on the server
    val silent = FakeRedisServer((_, requests) => requests.drain ++ Stream.never[IO])
    silent.use { port =>
      (1 to 30).toList.traverse_ { round =>
        RedisClient[IO].dedicated("127.0.0.1", port).allocated.flatMap { case (client, close) =>
          val keys = OneOrMoreKeys.unsafeFrom(List(Key("k")))
          client.send(blocking.forever.blpop[String](keys)).start.flatMap { waiting =>
            IO.sleep(50.millis) >> (waiting.cancel, IO.sleep((round % 5).millis) >> close).parTupled.void
              .timeoutTo(10.seconds, IO.raiseError(new Exception(s"round $round: cancelling didn't end")))
          }
        }
      }
    }
  }
}
