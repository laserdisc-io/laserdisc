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

import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import laserdisc.redis_6_2.*
import laserdisc.auto.*
import laserdisc.fs2.FakeRedisServer.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

final class RedisClientSendSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 60.seconds

  test("when the connection goes before MULTI's OK, the commands after the transaction fail too, without waiting for their timeout") {
    val closesOnMulti = FakeRedisServer { (n, commands) =>
      if (n == 1) commands.takeWhile(c => name(c) != "MULTI").drain else commands.as(bulk("v"))
    }

    closesOnMulti.use { port =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        client.send(multi(get[String]("a")), get[String]("b"), 30.seconds).attempt.timed.map { case (took, result) =>
          assert(took < 10.seconds, s"took $took")
          result match {
            case Right((transaction, after)) =>
              assert(transaction.isLeft, s"expected the transaction to fail, got $transaction")
              assert(after.isLeft, s"expected the GET after it to fail, got $after")
            case Left(error) => fail(s"expected each command to fail on its own, got $error")
          }
        }
      }
    }
  }

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

  private def name(command: RESP): String = command match {
    case Arr(Bulk(name) :: _) => name
    case other                => fail(s"not a command: $other")
  }

  test("a multi whose MULTI the server refuses sends nothing else, so none of its commands run") {
    // refuses MULTI, as for a user an ACL rule doesn't let run it, and keeps the names of the commands it gets
    def refusingMulti(seen: cats.effect.Ref[IO, List[String]]) =
      FakeRedisServer { (_, commands) =>
        commands.evalTap(c => seen.update(_ :+ name(c))).map { c =>
          if (name(c) == "MULTI") "-NOPERM User has no permissions to run the 'multi' command\r\n" else bulk(keyOf(c))
        }
      }

    IO.ref(List.empty[String]).flatMap { seen =>
      refusingMulti(seen).use { port =>
        RedisClient[IO].to("127.0.0.1", port).use { client =>
          for {
            refused <- client.send(multi(set("a", 1), set("b", 2)), 2.seconds)
            after   <- client.send(get[String]("c"), 2.seconds)
            names   <- seen.get
          } yield {
            assert(refused.isLeft, s"expected the refusal, got $refused")
            assertEquals(after, Right(Some("c")))
            assertEquals(names, List("MULTI", "GET"))
          }
        }
      }
    }
  }

  // the first connection answers WATCH and a GET, then closes; the next ones run transactions, keeping the names of what they get
  private def droppingWatch(seen: cats.effect.Ref[IO, List[String]]) =
    FakeRedisServer { (n, commands) =>
      def ok(c: RESP): Boolean = Set("WATCH", "UNWATCH", "DISCARD")(name(c))
      if (n == 1) commands.take(2).map(c => if (ok(c)) "+OK\r\n" else bulk("v"))
      else
        commands
          .evalTap(c => seen.update(_ :+ name(c)))
          .mapAccumulate(false) {
            case (_, c) if name(c) == "MULTI"   => true  -> "+OK\r\n"
            case (_, c) if name(c) == "EXEC"    => false -> s"*1\r\n+OK\r\n"
            case (_, c) if name(c) == "DISCARD" => false -> "+OK\r\n"
            case (true, _)                      => true  -> "+QUEUED\r\n"
            case (false, c)                     => false -> (if (ok(c)) "+OK\r\n" else bulk("w"))
          }
          .map(_._2)
    }

  // watches k and reads it on the first connection, then waits until the client is on the next one
  private def watchedThenReconnected(client: RedisClient[IO]): IO[Unit] =
    for {
      watched <- client.send(watch("k"), 2.seconds)
      read    <- client.send(get[String]("k"), 2.seconds)
      _       <- untilReplied(
        client
          .send(get[String]("k"), 2.seconds)
          .map(_.flatMap(r => if (r.contains("w")) Right(r) else Left(new Exception(s"still $r"))))
      )
    } yield {
      assertEquals(watched, Right(OK))
      assertEquals(read, Right(Some("v")))
    }

  private def assertNotRun(lost: Maybe[Option[Any]], names: List[String]): Unit = {
    assert(lost.left.exists(_.isInstanceOf[WatchLost]), s"expected WatchLost, got $lost")
    assertEquals(names, List("MULTI", "DISCARD", "UNWATCH"))
  }

  test("a multi doesn't run when its keys were watched on a connection that closed, as the server dropped the watch with it") {
    IO.ref(List.empty[String]).flatMap { seen =>
      droppingWatch(seen).use { port =>
        RedisClient[IO].to("127.0.0.1", port).use { client =>
          for {
            _     <- watchedThenReconnected(client)
            _     <- seen.set(Nil)
            lost  <- client.send(multi(set("k", 2)), 2.seconds)
            names <- seen.getAndSet(Nil)
            again <- client.send(watch("k"), 2.seconds)
            ran   <- client.send(multi(set("k", 2)), 2.seconds)
          } yield {
            assertNotRun(lost, names)
            assertEquals(again, Right(OK))
            assertEquals(ran, Right(Some(Right(OK))))
          }
        }
      }
    }
  }

  test("watching more keys on the new connection doesn't bring back the watch that closed with the old one") {
    IO.ref(List.empty[String]).flatMap { seen =>
      droppingWatch(seen).use { port =>
        RedisClient[IO].to("127.0.0.1", port).use { client =>
          for {
            _     <- watchedThenReconnected(client)
            more  <- client.send(watch("other"), 2.seconds)
            _     <- seen.set(Nil)
            lost  <- client.send(multi(set("k", 2)), 2.seconds)
            names <- seen.get
          } yield {
            assertEquals(more, Right(OK))
            assertNotRun(lost, names)
          }
        }
      }
    }
  }

  test("an UNWATCH and a WATCH sent with the multi count before it's checked") {
    IO.ref(List.empty[String]).flatMap { seen =>
      droppingWatch(seen).use { port =>
        RedisClient[IO].to("127.0.0.1", port).use { client =>
          for {
            _                            <- watchedThenReconnected(client)
            (unwatched, watched, result) <- client.send(unwatch, watch("k"), multi(set("k", 2)), 2.seconds)
          } yield {
            assertEquals(unwatched, Right(OK))
            assertEquals(watched, Right(OK))
            assertEquals(result, Right(Some(Right(OK))))
          }
        }
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

  test("sending no commands sends nothing") {
    unusedPort.use { port =>
      RedisClient[IO].to("127.0.0.1", port).use(client => client.send(EmptyTuple, 1.second).assertEquals(EmptyTuple))
    }
  }

  test("a command its caller gave up on while it was queued isn't sent") {
    (Deferred[IO, Unit], IO.ref(Vector.empty[RESP])).tupled.flatMap { case (connected, sent) =>
      // takes nothing from the queue until it's told to
      val channel: (RedisAddress, IO[Unit]) => Pipe[IO, RESP, RESP] = (_, onConnected) =>
        commands =>
          Stream.exec(onConnected >> connected.get) ++ commands.evalMap(command => sent.update(_ :+ command).as(Bulk(keyOf(command)): RESP))

      RedisClient.impl.connection[IO](channel, List(address(6379))).flatMap(RedisClient.impl.mkClient[IO](_)).use { client =>
        for {
          gaveUp   <- client.send(get[String]("late"), 200.millis).attempt
          _        <- connected.complete(())
          answered <- client.send(get[String]("k"), 2.seconds)
          commands <- sent.get
        } yield {
          assert(gaveUp.left.exists(_.isInstanceOf[RequestTimedOut[?]]), s"expected a timeout, got $gaveUp")
          assertEquals(answered, Right(Some("k")))
          assertEquals(commands, Vector[RESP](Arr(Bulk("GET"), Bulk("k"))))
        }
      }
    }
  }
}
