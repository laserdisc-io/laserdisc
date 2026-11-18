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
package usage

import cats.effect.IO
import laserdisc.fs2.{RedisClient, RedisSubscriber}
import munit.FunSuite

// what code outside of laserdisc.fs2 sees of it
final class ConnectorsSpec extends FunSuite {
  test("the connectors can be named outside of laserdisc.fs2") {
    val client: RedisClient.RedisClientConnector[IO]             = RedisClient[IO]
    val subscriber: RedisSubscriber.RedisSubscriberConnector[IO] = RedisSubscriber[IO]
    assert(client != null && subscriber != null)
  }

  test("the connectors can't be built outside of laserdisc.fs2") {
    assert(compileErrors("new RedisClient.RedisClientConnector[IO]()").nonEmpty)
    assert(compileErrors("new RedisSubscriber.RedisSubscriberConnector[IO](scala.concurrent.duration.Duration.Zero)").nonEmpty)
  }
}
