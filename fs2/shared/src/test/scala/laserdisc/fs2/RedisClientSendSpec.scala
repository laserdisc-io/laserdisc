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
import laserdisc.all.*
import laserdisc.auto.*
import laserdisc.fs2.FakeRedisServer.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

final class RedisClientSendSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 60.seconds

  test("the commands of a send reach the server together, without waiting for each other's replies") {
    // replies only once it has received three commands
    val repliesInThrees =
      FakeRedisServer((_, commands) => commands.chunkN(3).flatMap(cs => Stream.emits(cs.toList.map(c => bulk(keyOf(c))))))

    repliesInThrees.use { port =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        client
          .send(get[String]("a"), get[String]("b"), get[String]("c"), 2.seconds)
          .assertEquals((Right(Some("a")), Right(Some("b")), Right(Some("c"))))
      }
    }
  }
}
