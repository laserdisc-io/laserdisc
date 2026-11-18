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

import _root_.fs2.{Pipe, Stream}
import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import laserdisc.auto.*
import laserdisc.fs2.FakeRedisServer.*
import laserdisc.redis_6_2.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

final class DedicatedClientSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 60.seconds

  private val keys = OneOrMoreKeys.unsafeFrom(List(Key("k")))

  private def name(command: RESP): String = command match {
    case Arr(Bulk(name) :: _) => name
    case other                => fail(s"not a command: $other")
  }

  // answers BLPOP only once released, and anything else at once, keeping what it got and counting the connections
  private def server(
      released: Deferred[IO, Unit],
      seen: Ref[IO, List[String]],
      connections: Ref[IO, Int]
  ): (RedisAddress, IO[Unit]) => Pipe[IO, RESP, RESP] =
    (_, onConnected) =>
      commands =>
        Stream.exec(connections.update(_ + 1) >> onConnected) ++
          Stream.eval(cats.effect.std.Queue.unbounded[IO, RESP]).flatMap { taken =>
            Stream
              .fromQueueUnterminated(taken)
              .evalMap(command =>
                if (name(command) == "BLPOP") released.get.as(Arr(Bulk("k"), Bulk("x")): RESP) else IO.pure(Bulk("v"): RESP)
              )
              .concurrently(commands.evalMap(command => seen.update(_ :+ name(command)) >> taken.offer(command)))
          }

  private def dedicated(released: Deferred[IO, Unit], seen: Ref[IO, List[String]], connections: Ref[IO, Int]) =
    RedisClient.impl.connection[IO](server(released, seen, connections), List(address(6379))).flatMap(RedisClient.impl.mkDedicated[IO](_))

  private val fixture = (Deferred[IO, Unit], IO.ref(List.empty[String]), IO.ref(0)).tupled

  test("a command that waits for as long as it takes isn't taken for a server that stopped answering") {
    fixture.flatMap { case (released, seen, connections) =>
      dedicated(released, seen, connections).use { client =>
        for {
          blpop  <- client.send(blocking.forever.blpop[String](keys)).start
          _      <- IO.sleep(3.seconds)
          _      <- released.complete(())
          reply  <- blpop.joinWithNever
          opened <- connections.get
        } yield {
          assertEquals(reply, Right(Some(KV(Key("k"), "x"))))
          assertEquals(opened, 1)
        }
      }
    }
  }

  test("a dedicated client sends one command at a time, each once the one before it has returned") {
    fixture.flatMap { case (released, seen, connections) =>
      dedicated(released, seen, connections).use { client =>
        for {
          blpop   <- client.send(blocking.forever.blpop[String](keys)).start
          _       <- IO.sleep(200.millis)
          get     <- client.send(get[String]("a"), 500.millis).start
          _       <- IO.sleep(1.second)
          waiting <- seen.get
          _       <- released.complete(())
          _       <- blpop.joinWithNever
          got     <- get.joinWithNever
        } yield {
          assertEquals(waiting, List("BLPOP"))
          assertEquals(got, Right(Some("v")))
        }
      }
    }
  }

  test("cancelling a command waiting on the server closes the connection, and the next command connects again") {
    fixture.flatMap { case (released, seen, connections) =>
      dedicated(released, seen, connections).use { client =>
        for {
          blpop  <- client.send(blocking.forever.blpop[String](keys)).start
          _      <- IO.sleep(200.millis)
          _      <- blpop.cancel
          next   <- client.send(get[String]("a"), 2.seconds)
          opened <- connections.get
        } yield {
          assertEquals(next, Right(Some("v")))
          assertEquals(opened, 2)
        }
      }
    }
  }

  test("a dedicated client refuses commands once released") {
    fixture.flatMap { case (released, seen, connections) =>
      dedicated(released, seen, connections).allocated.flatMap { case (client, release) =>
        release >> client.send(blocking.forever.blpop[String](keys)).attempt.map(result => assertEquals(result, Left(ClientTerminated)))
      }
    }
  }

  test("a shared client can't send a command that waits for as long as it takes, as it isn't a Protocol") {
    assert(compileErrors("val protocol: Protocol[Any] = blocking.forever.blpop[String](keys)").nonEmpty)
  }
}
