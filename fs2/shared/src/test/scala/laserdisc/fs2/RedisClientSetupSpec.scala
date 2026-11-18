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
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import laserdisc.redis_6_2.*
import laserdisc.auto.*
import laserdisc.fs2.FakeRedisServer.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

final class RedisClientSetupSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 60.seconds

  private def words(command: RESP): List[String] =
    command match {
      case Arr(elements) => elements.collect { case Bulk(word) => word }
      case other         => sys.error(s"unexpected command $other")
    }

  private def isSetup(command: List[String]): Boolean = command.headOption.exists(w => w == "AUTH" || w == "SELECT")

  // a server that keeps the words of each command it gets, answering AUTH and SELECT as `setup` says and anything else with `reply`
  private def recording(setup: String, reply: String = bulk("value")): Resource[IO, (Port, Ref[IO, Vector[List[String]]])] =
    Resource.eval(IO.ref(Vector.empty[List[String]])).flatMap { received =>
      FakeRedisServer { (_, requests) =>
        requests.evalMap { command =>
          val ws = words(command)
          received.update(_ :+ ws).as(if (isSetup(ws)) setup else reply)
        }
      }.map(_ -> received)
    }

  private val loggedIn = Some(Credentials("user", "secret"))

  test("a client without credentials or a database sends nothing before the first command") {
    recording(setup = "+OK\r\n").use { case (port, received) =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        client.send(get[String]("k")).assertEquals(Right(Some("value"))) >> received.get.assertEquals(Vector(List("GET", "k")))
      }
    }
  }

  test("a client logs in with AUTH and selects the database before anything else") {
    recording(setup = "+OK\r\n").use { case (port, received) =>
      RedisClient[IO].to("127.0.0.1", port, credentials = loggedIn, database = Some(DbIndex(2))).use { client =>
        client.send(get[String]("k")).assertEquals(Right(Some("value"))) >>
          received.get.assertEquals(Vector(List("AUTH", "user", "secret"), List("SELECT", "2"), List("GET", "k")))
      }
    }
  }

  test("given only a password, a client logs in as the default user") {
    recording(setup = "+OK\r\n").use { case (port, received) =>
      RedisClient[IO].to("127.0.0.1", port, credentials = Some(Credentials("secret"))).use { client =>
        client.send(get[String]("k")) >> received.get.map(_.head).assertEquals(List("AUTH", "secret"))
      }
    }
  }

  test("a refused setup stops the client, failing what was sent and what's sent after with the server's reason") {
    recording(setup = "-WRONGPASS invalid username-password pair or user is disabled.\r\n").use { case (port, _) =>
      RedisClient[IO].to("127.0.0.1", port, credentials = loggedIn).use { client =>
        for {
          first <- client.send(get[String]("k"), 10.seconds).attempt.map(_.flatMap(identity))
          later <- client.send(get[String]("k"), 10.seconds).attempt.map(_.flatMap(identity)).timed
        } yield {
          List(first, later._2).foreach {
            case Left(ConnectionSetupRefused(_, reason)) => assert(reason.getMessage.startsWith("WRONGPASS"), reason.getMessage)
            case other                                   => fail(s"expected the setup to be refused, got $other")
          }
          assert(later._1 < 2.seconds, s"took ${later._1}")
        }
      }
    }
  }

  test("a setup refused only for now, as by a server loading its data or with too many clients, is tried again on a new connection") {
    val forNow = List(
      "-ERR max number of clients reached\r\n",
      "-LOADING Redis is loading the dataset in memory\r\n",
      "-BUSY Redis is busy running a script. You can only call SCRIPT KILL or SHUTDOWN NOSCRIPT.\r\n",
      "-MASTERDOWN Link with MASTER is down and replica-serve-stale-data is set to 'no'.\r\n",
      "-TRYAGAIN Multiple keys request during rehashing of slot\r\n"
    )
    forNow.parTraverse_ { refusal =>
      val refusingFirst = FakeRedisServer { (n, requests) =>
        requests.map(command => if (!isSetup(words(command))) bulk("value") else if (n == 1) refusal else "+OK\r\n")
      }
      refusingFirst.use { port =>
        RedisClient[IO].to("127.0.0.1", port, credentials = loggedIn).use { client =>
          untilReplied(client.send(get[String]("k"), 2.seconds)).assertEquals(Some("value"), refusal)
        }
      }
    }
  }

  test(
    "a server with too many clients saying so to the first command refuses the connection for now, so the client waits to connect again"
  ) {
    IO.ref(0).flatMap { opened =>
      val full = FakeRedisServer { (n, requests) =>
        Stream.exec(opened.set(n)) ++ requests.take(1).as("-ERR max number of clients reached\r\n")
      }
      full.use { port =>
        RedisClient[IO].to("127.0.0.1", port).use { client =>
          (client.send(get[String]("k")), client.send(get[String]("k")), opened.get).tupled.map { case (first, second, connections) =>
            assert(first.left.exists(_.isInstanceOf[ConnectionSetupRefused]), first)
            assert(second.left.exists(_.isInstanceOf[ServerUnavailable]), second)
            assertEquals(connections, 1)
          }
        }
      }
    }
  }

  test("a command isn't sent when the server refused to log in but agreed to select the database") {
    IO.ref(Vector.empty[(Int, List[String])]).flatMap { received =>
      // the first connection's login is refused, for good or for now, and its SELECT agreed to; the next connections set up fine
      def refusingLogin(refusal: String) = FakeRedisServer { (n, requests) =>
        requests.evalMap { command =>
          val ws = words(command)
          received
            .update(_ :+ (n -> ws))
            .as(ws.headOption match {
              case Some("AUTH") if n == 1 => refusal
              case _ if isSetup(ws)       => "+OK\r\n"
              case _                      => bulk("value")
            })
        }
      }
      def sentOnFirst = received.getAndSet(Vector.empty).map(_.collect { case (1, ws) if !isSetup(ws) => ws })
      val forGood     = "-WRONGPASS invalid username-password pair or user is disabled.\r\n"
      val forNow      = "-LOADING Redis is loading the dataset in memory\r\n"

      refusingLogin(forGood).use { port =>
        RedisClient[IO].to("127.0.0.1", port, credentials = loggedIn, database = Some(DbIndex(2))).use { client =>
          client.send(get[String]("k"), 5.seconds).attempt >> sentOnFirst.assertEquals(Vector.empty)
        }
      } >> refusingLogin(forNow).use { port =>
        RedisClient[IO].to("127.0.0.1", port, credentials = loggedIn, database = Some(DbIndex(2))).use { client =>
          untilReplied(client.send(get[String]("k"), 2.seconds)).assertEquals(Some("value")) >> sentOnFirst.assertEquals(Vector.empty)
        }
      }
    }
  }

  test("a command isn't sent before the server has agreed to the setup, so it can't run in the wrong database") {
    recording(setup = "-ERR DB index is out of range\r\n").use { case (port, received) =>
      RedisClient[IO].to("127.0.0.1", port, database = Some(DbIndex(99))).use { client =>
        client.send(set("k", "v"), 10.seconds).attempt >> received.get.assertEquals(Vector(List("SELECT", "99")))
      }
    }
  }

  test("commands wait for the setup's replies") {
    Resource
      .eval((IO.ref(Vector.empty[List[String]]), Deferred[IO, Unit]).tupled)
      .flatMap { case (received, answer) =>
        FakeRedisServer { (_, requests) =>
          requests.evalMap { command =>
            val ws = words(command)
            received.update(_ :+ ws) >> (if (isSetup(ws)) answer.get.as("+OK\r\n") else IO.pure(bulk("value")))
          }
        }.map(port => (port, received, answer))
      }
      .use { case (port, received, answer) =>
        RedisClient[IO].to("127.0.0.1", port, credentials = loggedIn).use { client =>
          for {
            sending <- client.send(get[String]("k"), 10.seconds).start
            _       <- IO.sleep(500.millis)
            before  <- received.get
            _       <- answer.complete(())
            reply   <- sending.joinWithNever
          } yield {
            assertEquals(before, Vector(List("AUTH", "user", "secret")))
            assertEquals(reply, Right(Some("value")))
          }
        }
      }
  }

  test("each new connection is set up again") {
    Resource
      .eval(IO.ref(Vector.empty[(Int, List[String])]))
      .flatMap { received =>
        FakeRedisServer { (n, requests) =>
          requests
            .evalMap(command => received.update(_ :+ (n -> words(command))).as(words(command)))
            .flatMap { ws =>
              if (isSetup(ws)) Stream.emit("+OK\r\n")
              else if (n == 1) Stream.raiseError[IO](new RuntimeException("dropped"))
              else Stream.emit(bulk("value"))
            }
            .handleErrorWith(_ => Stream.empty)
        }.map(_ -> received)
      }
      .use { case (port, received) =>
        RedisClient[IO].to("127.0.0.1", port, credentials = loggedIn, database = Some(DbIndex(2))).use { client =>
          untilReplied(client.send(get[String]("k"), 2.seconds)).assertEquals(Some("value")) >>
            received.get
              .map(_.filter(_._1 == 2).map(_._2).take(3))
              .assertEquals(
                Vector(List("AUTH", "user", "secret"), List("SELECT", "2"), List("GET", "k"))
              )
        }
      }
  }

  test("a connection closed once set up, with nothing sent on it, is opened again only for the next command") {
    Resource
      .eval(IO.ref(0))
      .flatMap { connections =>
        FakeRedisServer { (n, requests) =>
          Stream.exec(connections.set(n)) ++ requests
            .map(words)
            .flatMap { ws =>
              if (isSetup(ws) && n == 1) Stream.emit("+OK\r\n") ++ Stream.raiseError[IO](new RuntimeException("idle"))
              else if (isSetup(ws)) Stream.emit("+OK\r\n")
              else Stream.emit(bulk("value"))
            }
            .handleErrorWith(_ => Stream.empty)
        }.map(_ -> connections)
      }
      .use { case (port, connections) =>
        RedisClient[IO].to("127.0.0.1", port, credentials = loggedIn).use { client =>
          for {
            _     <- IO.sleep(1.second)
            idle  <- connections.get
            reply <- client.send(get[String]("k"), 10.seconds)
            after <- connections.get
          } yield {
            assertEquals(idle, 1)
            assertEquals(reply, Right(Some("value")))
            assertEquals(after, 2)
          }
        }
      }
  }

  test("a server that closes each connection before answering the setup makes the client give up, instead of reconnecting forever") {
    val closesOnSetup = FakeRedisServer((_, requests) => requests.take(1).drain)

    closesOnSetup.use { port =>
      RedisClient[IO].to("127.0.0.1", port, credentials = loggedIn).use { client =>
        client.send(get[String]("k"), 30.seconds).attempt.map(_.flatMap(identity)).timed.map { case (took, result) =>
          assertEquals(
            result.left.toOption.collect { case ServerUnavailable(cause) => cause.getClass },
            Some(classOf[ServerTerminatedConnection])
          )
          assert(took < 10.seconds, s"took $took")
        }
      }
    }
  }

  // a server that keeps each connection's commands, agreeing to those that set the connection up, refusing CLIENT TRACKING on the
  // connections refuseTracking says, and closing the connection on GET bye
  private def resetting(
      refuseTracking: Int => Boolean = _ => false
  ): Resource[IO, (Port, Ref[IO, Vector[(Int, List[String])]])] =
    Resource.eval(IO.ref(Vector.empty[(Int, List[String])])).flatMap { received =>
      FakeRedisServer { (n, requests) =>
        requests
          .map(words)
          .evalTap(ws => received.update(_ :+ (n -> ws)))
          .takeWhile(_ != List("GET", "bye"))
          .map {
            case "CLIENT" :: "TRACKING" :: _ if refuseTracking(n) => "-ERR The client ID you want redirect to does not exist\r\n"
            case "HELLO" :: _                                     =>
              "*14\r\n" + List("server", "redis", "version", "6.2.0").map(bulk).mkString + bulk("proto") + ":2\r\n" + bulk("id") +
                ":5\r\n" + List("mode", "standalone", "role", "master", "modules").map(bulk).mkString + "*0\r\n"
            case ("AUTH" | "SELECT" | "CLIENT" | "READONLY" | "READWRITE") :: _ => "+OK\r\n"
            case _                                                              => bulk("value")
          }
      }.map(_ -> received)
    }

  // the commands each connection after the first got
  private def after(received: Ref[IO, Vector[(Int, List[String])]], connection: Int): IO[Vector[List[String]]] =
    received.get.map(_.collect { case (`connection`, ws) => ws })

  // tracking with no connection to send its notices to, as the command line can turn it on
  private val trackingBcast: Protocol.Single[OK] = Protocol("CLIENT", List("TRACKING", "ON", "BCAST")).as[Str, OK]

  private def reconnected(client: RedisClient[IO]): IO[String] =
    client.send(get[String]("bye"), 2.seconds).attempt >> untilReplied(client.send(get[String]("k"), 2.seconds)).map(_.getOrElse(""))

  test("tracking, reading from a replica and the connection's name are set again on a new connection, after it has logged in") {
    resetting().use { case (port, received) =>
      RedisClient[IO].to("127.0.0.1", port, credentials = loggedIn).use { client =>
        val ok = (c: IO[Maybe[OK]]) => c.flatMap(IO.fromEither).void
        ok(client.send(trackingBcast)) >>
          ok(client.send(laserdisc.redis_6_2.readonly)) >>
          ok(client.send(laserdisc.redis_6_2.client.setname(ConnectionName("ld")))) >>
          reconnected(client).assertEquals("value") >>
          after(received, 2).assertEquals(
            Vector(
              List("AUTH", "user", "secret"),
              List("CLIENT", "SETNAME", "ld"),
              List("READONLY"),
              List("CLIENT", "TRACKING", "ON", "BCAST"),
              List("GET", "k")
            )
          )
      }
    }
  }

  test("tracking whose notices go to another connection isn't turned on again on a new one, as that connection's id may be another's then") {
    resetting().use { case (port, received) =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        client
          .send(laserdisc.redis_6_2.client.tracking(servertypes.trackingOptions.redirect(NonNegLong(7L)).bcast))
          .flatMap(IO.fromEither) >>
          reconnected(client).assertEquals("value") >>
          after(received, 2).assertEquals(Vector(List("GET", "k"))) >>
          reconnected(client).assertEquals("value") >>
          after(received, 3).assertEquals(Vector(List("GET", "k")))
      }
    }
  }

  test("what's set back as a connection starts, and a name given with HELLO, are set again as such") {
    resetting().use { case (port, received) =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        val ok = (c: IO[Maybe[OK]]) => c.flatMap(IO.fromEither).void
        ok(client.send(laserdisc.redis_6_2.client.tracking(servertypes.trackingOptions.redirect(NonNegLong(7L))))) >>
          ok(client.send(laserdisc.redis_6_2.client.trackingoff)) >>
          ok(client.send(laserdisc.redis_6_2.readonly)) >>
          ok(client.send(laserdisc.redis_6_2.readwrite)) >>
          client.send(laserdisc.redis_6_2.connection.hello(ConnectionName("ld-hello"))).flatMap(IO.fromEither).void >>
          reconnected(client).assertEquals("value") >>
          after(received, 2).assertEquals(Vector(List("CLIENT", "SETNAME", "ld-hello"), List("GET", "k")))
      }
    }
  }

  test("a new connection refusing what's set again carries on without it, and the next ones aren't set to it") {
    resetting(refuseTracking = _ == 2).use { case (port, received) =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        client.send(trackingBcast).flatMap(IO.fromEither) >>
          reconnected(client).assertEquals("value") >>
          after(received, 2).assertEquals(Vector(List("CLIENT", "TRACKING", "ON", "BCAST"), List("GET", "k"))) >>
          reconnected(client).assertEquals("value") >>
          after(received, 3).assertEquals(Vector(List("GET", "k")))
      }
    }
  }

  test("the password stays out of what the credentials show") {
    assertEquals(Credentials("user", "secret").toString, "Credentials(user, ***)")
    assertEquals(Credentials("secret").toString, "Credentials(default, ***)")
  }
}
