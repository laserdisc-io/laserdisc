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

  // takes what it's sent and never answers
  private val silent: (RedisAddress, IO[Unit]) => Pipe[IO, RESP, RESP] =
    (_, onConnected) => commands => Stream.exec(onConnected) ++ commands.drain ++ Stream.never[IO]

  test("when the server stops answering, the commands after a transaction waiting for MULTI's OK fail with the same error as it") {
    RedisClient.impl.connection[IO](silent, List(address(6379))).flatMap(RedisClient.impl.mkClient[IO](_)).use { client =>
      for {
        // once it's overdue, the client sees the server has stopped answering
        _                    <- client.send(get[String]("x"), 1.second).attempt.start
        _                    <- IO.sleep(100.millis)
        (transaction, after) <- client.send(multi(get[String]("a")), get[String]("b"), 30.seconds)
      } yield {
        assertEquals(transaction.left.toOption, Some(ServerNotResponding(address(6379)): Throwable))
        assertEquals(after.left.toOption, Some(ServerNotResponding(address(6379)): Throwable))
      }
    }
  }

  test("when the connection goes once MULTI's OK is in, before the transaction's commands are sent, the multi fails at once") {
    // takes MULTI, says OK to it, and goes without taking anything else
    val goingAfterMulti: (RedisAddress, IO[Unit]) => Pipe[IO, RESP, RESP] =
      (_, onConnected) =>
        commands => Stream.exec(onConnected) ++ commands.take(1).as(Str("OK"): RESP) ++ Stream.raiseError[IO](new Exception("gone"))

    RedisClient.impl.connection[IO](goingAfterMulti, List(address(6379))).flatMap(RedisClient.impl.mkClient[IO](_)).use { client =>
      client.send(multi(get[String]("a")), 30.seconds).timeout(10.seconds).map { result =>
        assert(result.left.exists(_.getMessage == "gone"), s"expected the connection's error, got $result")
      }
    }
  }

  test("when the client is closed, the commands after a transaction waiting for MULTI's OK fail with ClientTerminated") {
    RedisClient.impl.connection[IO](silent, List(address(6379))).flatMap(RedisClient.impl.mkClient[IO](_)).allocated.flatMap {
      case (client, close) =>
        for {
          sending              <- client.send(multi(get[String]("a")), get[String]("b"), 30.seconds).start
          _                    <- IO.sleep(200.millis)
          _                    <- close
          (transaction, after) <- sending.joinWithNever.timeout(2.seconds)
        } yield {
          assertEquals(transaction.left.toOption, Some(ClientTerminated: Throwable))
          assertEquals(after.left.toOption, Some(ClientTerminated: Throwable))
        }
    }
  }

  // takes each command as it comes, and answers them in order, BLPOP 1.5 seconds after the reply before it and anything else a second
  // after it, counting the connections
  private def slow(connections: Ref[IO, Int]): (RedisAddress, IO[Unit]) => Pipe[IO, RESP, RESP] =
    (_, onConnected) =>
      commands =>
        Stream.exec(connections.update(_ + 1) >> onConnected) ++
          Stream.eval(cats.effect.std.Queue.unbounded[IO, RESP]).flatMap { taken =>
            Stream
              .fromQueueUnterminated(taken)
              .evalMap { command =>
                if (name(command) == "BLPOP") IO.sleep(1500.millis).as(NilArr: RESP) else IO.sleep(1.second).as(Bulk("v"): RESP)
              }
              .concurrently(commands.evalMap(taken.offer))
          }

  test("a blocking command's wait is added to its send's timeout") {
    IO.ref(0).flatMap { connections =>
      RedisClient.impl.connection[IO](slow(connections), List(address(6379))).flatMap(RedisClient.impl.mkClient[IO](_)).use { client =>
        client.send(blocking.blpop[String](Key("k"), BlockingSeconds.unsafeFrom(1d)), 1.second).assertEquals(Right(None))
      }
    }
  }

  test("a command sent behind a blocking one, which holds up its reply, isn't taken for a server that stopped answering") {
    IO.ref(0).flatMap { connections =>
      RedisClient.impl.connection[IO](slow(connections), List(address(6379))).flatMap(RedisClient.impl.mkClient[IO](_)).use { client =>
        for {
          blpop <- client.send(blocking.blpop[String](Key("k"), BlockingSeconds.unsafeFrom(2d)), 1.second).start
          _     <- IO.sleep(100.millis)
          // its caller gives up after a second, as it's only answered a second after BLPOP is, 1.5 seconds in
          _     <- client.send(get[String]("a"), 1.second).attempt
          _     <- IO.sleep(1.second)
          reply <- blpop.joinWithNever
          // a connection the watchdog closed would be opened again for this one
          next   <- client.send(get[String]("b"), 2.seconds)
          opened <- connections.get
        } yield {
          assertEquals(reply, Right(None))
          assertEquals(next, Right(Some("v")))
          assertEquals(opened, 1)
        }
      }
    }
  }

  test("a send waiting for room in the queue times out, as the time it waits counts towards its timeout") {
    // never takes anything, so the queue fills up
    val stuck: (RedisAddress, IO[Unit]) => Pipe[IO, RESP, RESP] = (_, _) => _ => Stream.never[IO]

    RedisClient.impl.connection[IO](stuck, List(address(6379))).flatMap(RedisClient.impl.mkClient[IO](_)).use { client =>
      (1 to 1100).toList
        .parTraverse(_ => client.send(get[String]("k"), 500.millis).attempt)
        .timeout(10.seconds)
        .map(results => assert(results.forall(_.left.exists(_.isInstanceOf[RequestTimedOut[?]])), results.distinct.toString))
    }
  }

  test("a send once the connection has ended fails with ClientTerminated at once, instead of waiting for its timeout") {
    RedisClient.impl.connection[IO](silent, List(address(6379))).allocated.flatMap { case (connection, end) =>
      RedisClient.impl.mkClient[IO](connection).use { client =>
        end >> client.send(get[String]("k"), 5.seconds).attempt.timed.map { case (took, result) =>
          assertEquals(result.flatMap(identity).left.toOption, Some(ClientTerminated: Throwable))
          assert(took < 2.seconds, s"took $took")
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

  test("a reader that throws fails only its own command, and the commands sent with it and after it still get their replies") {
    val thrown                                       = new IllegalStateException("thrown")
    implicit val throwing: Read[Bulk, Throwing.type] = Read.instance(_ => throw thrown)
    val echoing                                      = FakeRedisServer((_, commands) => commands.map(c => bulk(keyOf(c))))

    echoing.use { port =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        for {
          (failed, sentWith) <- client.send(get[Throwing.type]("a"), get[String]("b"), 2.seconds)
          after              <- client.send(get[String]("c"), 2.seconds)
        } yield {
          assertEquals(failed, Left(thrown))
          assertEquals(sentWith, Right(Some("b")))
          assertEquals(after, Right(Some("c")))
        }
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

  // the first connection answers WATCH and a GET, then closes; the next ones run transactions, keeping the names of what they get. The
  // connection numbered refusingDiscardOn refuses DISCARD, as for a user an ACL rule doesn't let run it, so it stays in the transaction
  private def droppingWatch(seen: cats.effect.Ref[IO, List[String]], refusingDiscardOn: Int = 0) =
    FakeRedisServer { (n, commands) =>
      def ok(c: RESP): Boolean = Set("WATCH", "UNWATCH", "DISCARD")(name(c))
      if (n == 1) commands.take(2).map(c => if (ok(c)) "+OK\r\n" else bulk("v"))
      else
        commands
          .evalTap(c => seen.update(_ :+ name(c)))
          .mapAccumulate(false) {
            case (_, c) if name(c) == "MULTI"                             => true  -> "+OK\r\n"
            case (_, c) if name(c) == "EXEC"                              => false -> s"*1\r\n+OK\r\n"
            case (_, c) if name(c) == "DISCARD" && n == refusingDiscardOn =>
              true -> "-NOPERM User has no permissions to run the 'discard' command\r\n"
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

  test("when the server refuses to discard the transaction of a lost watch, the client connects again, as it may still be inside it") {
    IO.ref(List.empty[String]).flatMap { seen =>
      droppingWatch(seen, refusingDiscardOn = 2).use { port =>
        RedisClient[IO].to("127.0.0.1", port).use { client =>
          for {
            _    <- watchedThenReconnected(client)
            lost <- client.send(multi(set("k", 2)), 2.seconds)
            // on a connection still inside the transaction, the GET would be queued instead of run
            after <- untilReplied(client.send(get[String]("k"), 2.seconds))
            ran   <- client.send(multi(set("k", 2)), 2.seconds)
          } yield {
            assert(lost.left.exists(_.isInstanceOf[WatchLost]), s"expected WatchLost, got $lost")
            assertEquals(after, Some("w"))
            assertEquals(ran, Right(Some(Right(OK))))
          }
        }
      }
    }
  }

  // watches k on the first connection, and answers the next command with ending, then closes it
  private def endingWatchWith(ending: String, seen: cats.effect.Ref[IO, List[String]]) =
    FakeRedisServer { (n, commands) =>
      if (n == 1) commands.take(2).map(c => if (name(c) == "WATCH") "+OK\r\n" else ending)
      else
        commands.evalTap(c => seen.update(_ :+ name(c))).map { c =>
          name(c) match {
            case "MULTI" | "DISCARD" | "UNWATCH" => "+OK\r\n"
            case "SET"                           => "+QUEUED\r\n"
            case "EXEC"                          => "*1\r\n+OK\r\n"
            case _                               => bulk("w")
          }
        }
    }

  List(
    ("EXEC", "*0\r\n", true),
    ("EXEC", "-EXECABORT Transaction discarded because of previous errors.\r\n", true),
    ("DISCARD", "+OK\r\n", true),
    ("RESET", "+RESET\r\n", true),
    ("EXEC", "-ERR EXEC without MULTI\r\n", false),
    ("DISCARD", "-ERR DISCARD without MULTI\r\n", false)
  ).foreach { case (command, reply, ends) =>
    test(s"a $command sent as a plain command, answered ${reply.trim}, ${if (ends) "ends" else "doesn't end"} the watch") {
      IO.ref(List.empty[String]).flatMap { seen =>
        endingWatchWith(reply, seen).use { port =>
          RedisClient[IO].to("127.0.0.1", port).use { client =>
            for {
              _ <- client.send(watch("k"), 2.seconds)
              _ <- client.send(Protocol(command, EmptyTuple).using(laserdisc.protocol.RESPRead.raw), 2.seconds)
              _ <- untilReplied(
                client
                  .send(get[String]("k"), 2.seconds)
                  .map(_.flatMap(r => if (r.contains("w")) Right(r) else Left(new Exception(s"still $r"))))
              )
              _     <- seen.set(Nil)
              ran   <- client.send(multi(set("k", 2)), 2.seconds)
              names <- seen.get
            } yield
              if (ends) {
                assert(ran.exists(_.exists(_ == Right(OK))), s"expected the multi to run, got $ran")
                assertEquals(names, List("MULTI", "SET", "EXEC"))
              } else assertNotRun(ran, names)
          }
        }
      }
    }
  }

  // answers like a server that runs transactions, reading k as x
  private def running(c: RESP): String = name(c) match {
    case "WATCH" | "MULTI" | "DISCARD" | "UNWATCH" => "+OK\r\n"
    case "SET"                                     => "+QUEUED\r\n"
    case "EXEC"                                    => "*1\r\n+OK\r\n"
    case _                                         => bulk("x")
  }

  // sends watch k and a multi with it after the reconnection
  private def watchedAgainThenRun(client: RedisClient[IO]): IO[Unit] =
    for {
      _ <- untilReplied(
        client.send(get[String]("k"), 2.seconds).map(_.flatMap(r => if (r.contains("x")) Right(r) else Left(new Exception(s"still $r"))))
      )
      watched <- client.send(watch("k"), 2.seconds)
      ran     <- client.send(multi(set("k", 2)), 2.seconds)
    } yield {
      assertEquals(watched, Right(OK))
      assert(ran.exists(_.exists(_ == Right(OK))), s"expected the multi after watching again to run, got $ran")
    }

  test("when the connection goes before MULTI's OK, with the keys watched on an earlier one, the multi fails with WatchLost") {
    // the first connection watches k and closes, the second closes on MULTI, and the third runs everything
    val closingOnMulti = FakeRedisServer { (n, commands) =>
      n match {
        case 1 => commands.take(2).map(c => if (name(c) == "WATCH") "+OK\r\n" else bulk("v"))
        case 2 => commands.takeWhile(c => name(c) != "MULTI").as(bulk("w"))
        case _ => commands.map(running)
      }
    }

    closingOnMulti.use { port =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        for {
          _    <- watchedThenReconnected(client)
          lost <- client.send(multi(set("k", 2)), 2.seconds)
          _    <- watchedAgainThenRun(client)
        } yield assert(lost.left.exists(_.isInstanceOf[WatchLost]), s"expected WatchLost, got $lost")
      }
    }
  }

  test("when the connection the keys were watched on goes before MULTI's OK, the multi fails with WatchLost") {
    // the first connection watches k and closes on MULTI, and the second runs everything
    val closingOnMulti = FakeRedisServer { (n, commands) =>
      if (n == 1) commands.takeWhile(c => name(c) != "MULTI").as("+OK\r\n") else commands.map(running)
    }

    closingOnMulti.use { port =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        for {
          watched <- client.send(watch("k"), 2.seconds)
          lost    <- client.send(multi(set("k", 2)), 2.seconds)
          _       <- watchedAgainThenRun(client)
        } yield {
          assertEquals(watched, Right(OK))
          assert(lost.left.exists(_.isInstanceOf[WatchLost]), s"expected WatchLost, got $lost")
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

  // the first connection takes the queued sends together, answers the first command, and goes once it has the second
  private def goingAfterTwo(taking: Deferred[IO, Unit], connections: cats.effect.Ref[IO, Int], next: Pipe[IO, RESP, RESP]) =
    RedisClient.impl.connection[IO](
      (_, onConnected) =>
        commands =>
          Stream.eval(connections.updateAndGet(_ + 1)).flatMap { n =>
            if (n == 1)
              Stream.exec(onConnected >> taking.get) ++
                commands.take(2).zipWithIndex.collect { case (_, 0L) => Bulk("v"): RESP } ++
                Stream.raiseError[IO](new Exception("gone"))
            else next(commands)
          },
      List(address(6379))
    )

  test("when the connection goes, the sends taken off the queue with the ones it was sending, but not sent, go on the next connection") {
    (Deferred[IO, Unit], IO.ref(0), IO.ref(Vector.empty[RESP])).tupled.flatMap { case (taking, connections, again) =>
      val answering: Pipe[IO, RESP, RESP] = _.evalTap(c => again.update(_ :+ c)).as(Bulk("v"))

      goingAfterTwo(taking, connections, answering).flatMap(RedisClient.impl.mkClient[IO](_)).use { client =>
        val keys = List(Key("a"), Key("b"), Key("c"))
        for {
          sends    <- keys.parTraverse(k => client.send(get[String](k), 30.seconds).attempt.map(_.flatten)).start
          _        <- IO.sleep(200.millis)
          _        <- taking.complete(())
          outcomes <- sends.joinWithNever.timeout(10.seconds)
          sent     <- again.get
        } yield {
          // the one sent and not answered may have run, so it isn't sent again
          assertEquals(outcomes.count(_.isLeft), 1, s"$outcomes")
          assertEquals(outcomes.count(_ == Right(Some("v"))), 2, s"$outcomes")
          assertEquals(sent.size, 1)
        }
      }
    }
  }

  test("the sends waiting to go on the next connection fail when the client is closed") {
    (Deferred[IO, Unit], IO.ref(0)).tupled.flatMap { case (taking, connections) =>
      goingAfterTwo(taking, connections, _ => Stream.never[IO]).flatMap(RedisClient.impl.mkClient[IO](_)).allocated.flatMap {
        case (client, close) =>
          for {
            sends    <- List(Key("a"), Key("b"), Key("c")).parTraverse(k => client.send(get[String](k), 30.seconds).attempt).start
            _        <- IO.sleep(200.millis)
            _        <- taking.complete(())
            _        <- IO.sleep(200.millis)
            _        <- close
            outcomes <- sends.joinWithNever.timeout(2.seconds)
          } yield assert(outcomes.exists(_.fold(_ == ClientTerminated, _.left.exists(_ == ClientTerminated))), s"$outcomes")
      }
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

  // queues each of two requests as a send of its own would, without waiting for replies
  private implicit val queuedApart: Handler.Aux[IO, Env[IO], Request[IO] *: Request[IO] *: EmptyTuple, EmptyTuple] =
    new Handler[IO, Env[IO], Request[IO] *: Request[IO] *: EmptyTuple] {
      override type LOut = EmptyTuple
      override def apply(env: Env[IO], in: Request[IO] *: Request[IO] *: EmptyTuple): IO[EmptyTuple] = in match {
        case first *: second *: _ =>
          List(first, second)
            .traverse_(r => Deferred[IO, Unit].flatMap(claim => env._1(Batch(Vector(r), claim, 30.seconds))))
            .as(EmptyTuple)
      }
    }

  test("when the server is unavailable, a send failed slowly doesn't stop the sends queued with it from failing") {
    // reads nothing for a moment, so that both sends are queued, and goes without connecting
    val unreachable: (RedisAddress, IO[Unit]) => Pipe[IO, RESP, RESP] =
      (_, _) => _ => Stream.sleep_[IO](200.millis) ++ Stream.raiseError[IO](new Exception("unreachable"))

    RedisClient.impl.connection[IO](unreachable, List(address(6379))).use { connection =>
      Deferred[IO, Maybe[Option[String]]].flatMap { answer =>
        val slow = Request[IO, Option[String]](get[String]("a"), _ => IO.sleep(4.seconds))
        val next = Request[IO, Option[String]](get[String]("b"), answer.complete(_).void)
        connection.send(slow *: next *: EmptyTuple, 30.seconds) >>
          answer.get.timeout(10.seconds).map { reply =>
            assertEquals(reply.left.toOption.collect { case ServerUnavailable(cause) => cause.getMessage }, Some("unreachable"))
          }
      }
    }
  }
}

private object Throwing
