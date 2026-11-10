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
import cats.syntax.all.*
import laserdisc.redis_6_2.*
import laserdisc.auto.*
import laserdisc.fs2.FakeRedisServer.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

// JVM only: on Node.js 26, fs2 socket reads can stall when several replies arrive at once
// (https://github.com/typelevel/fs2/issues/3793)
final class RedisClientConcurrentSendSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 60.seconds

  private def key(s: String): Key = Key.unsafeFrom(s)

  test("concurrent sends each get the replies to their own commands") {
    val echoesKeys = FakeRedisServer((_, commands) => commands.map(c => bulk(keyOf(c))))

    echoesKeys.use { port =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        (1 to 200).toList
          .parTraverse { i =>
            client
              .send(get[String](key(s"$i-1")), get[String](key(s"$i-2")), get[String](key(s"$i-3")), 5.seconds)
              .map(_ -> i)
          }
          .map(_.foreach { case (replies, i) =>
            assertEquals(replies, (Right(Some(s"$i-1")), Right(Some(s"$i-2")), Right(Some(s"$i-3"))))
          })
      }
    }
  }
}
