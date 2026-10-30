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
import laserdisc.all.*
import laserdisc.auto.*
import laserdisc.fs2.FakeRedisServer.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

final class RedisClientReconnectSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 60.seconds

  test("toNodes connects to the next address when one is unreachable") {
    (FakeRedisServer((_, requests) => requests.as(bulk("alive"))), unusedPort).tupled.use { case (alive, dead) =>
      RedisClient[IO].toNodes(Set(address(alive), address(dead))).use { client =>
        untilReplied(client.send(get[String]("k"), 2.seconds)).assertEquals(Some("alive"))
      }
    }
  }

  test("toNodes connects to the next address when the connected one goes away") {
    val leaving   = FakeRedisServer((n, requests) => if (n == 1) requests.take(1).as(bulk("first")) else Stream.empty)
    val remaining = FakeRedisServer((_, requests) => requests.as(bulk("second")))

    (leaving, remaining).tupled.use { case (first, second) =>
      RedisClient[IO].toNodes(Set(address(first), address(second))).use { client =>
        for {
          r1 <- untilReplied(client.send(get[String]("k"), 2.seconds))
          r2 <- untilReplied(
            client.send(get[String]("k"), 2.seconds).map(_.flatMap(r => if (r == r1) Left(new Exception(s"still $r")) else Right(r)))
          )
        } yield assertEquals(Set(r1, r2), Set(Option("first"), Option("second")))
      }
    }
  }

  test("a request in flight when the connection drops fails, and later replies go to the right requests") {
    val dropsFirstConnection = FakeRedisServer { (n, requests) =>
      if (n == 1) requests.take(1).drain
      else requests.zipWithIndex.map { case (_, i) => bulk(s"reply-$i") }
    }

    dropsFirstConnection.use { port =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        for {
          dropped <- untilReplied(client.send(get[String]("k"), 2.seconds).map(r => Right(r)))
          r1      <- untilReplied(client.send(get[String]("k"), 2.seconds))
          r2      <- client.send(get[String]("k"), 2.seconds)
        } yield {
          assert(dropped.isLeft, s"expected the dropped request to fail, got $dropped")
          assertEquals(r1, Some("reply-0"))
          assertEquals(r2, Right(Some("reply-1")))
        }
      }
    }
  }
}
