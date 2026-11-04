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
import laserdisc.redis_6_2.*
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

  test("a multi gets the results of its commands, and the commands around it get their own replies") {
    // answers GET key with key, and keeps the keys of the GETs between MULTI and EXEC to answer EXEC with
    val transactional =
      FakeRedisServer { (_, commands) =>
        commands
          .mapAccumulate(Option.empty[List[String]]) {
            case (None, Arr(Bulk("MULTI") :: Nil))      => Some(Nil)                    -> "+OK\r\n"
            case (Some(keys), Arr(Bulk("EXEC") :: Nil)) => None                         -> s"*${keys.size}\r\n${keys.map(bulk).mkString}"
            case (Some(keys), command)                  => Some(keys :+ keyOf(command)) -> "+QUEUED\r\n"
            case (None, command)                        => None                         -> bulk(keyOf(command))
          }
          .map(_._2)
      }

    transactional.use { port =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        client
          .send(get[String]("a"), multi(get[String]("b"), get[String]("c")), get[String]("d"), 2.seconds)
          .assertEquals((Right(Some("a")), Right(Some((Right(Some("b")), Right(Some("c"))))), Right(Some("d"))))
      }
    }
  }

  test("commands still waiting to be sent fail when the client is closed") {
    // a connection that never takes commands off the queue
    val neverSends = RedisClient.impl.connection[IO]((_, _) => _ => Stream.never[IO], Seq(address(Port.unsafeFrom(6379))))

    neverSends.flatMap(RedisClient.impl.mkClient[IO]).allocated.flatMap { case (client, close) =>
      for {
        waiting <- client.send(get[String]("k"), 30.seconds).start
        _       <- IO.sleep(200.millis)
        _       <- close
        out     <- waiting.joinWithNever.timeout(2.seconds)
        after   <- client.send(get[String]("k"), 1.second).attempt
      } yield {
        assertEquals(out, Left(ClientTerminated))
        assertEquals(after, Left(ClientTerminated))
      }
    }
  }
}
