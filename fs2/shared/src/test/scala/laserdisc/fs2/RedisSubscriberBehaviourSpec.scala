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
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import laserdisc.auto.*
import laserdisc.protocol.Feed
import laserdisc.protocol.Feed.Message
import munit.CatsEffectSuite

import scala.concurrent.duration.*

final class RedisSubscriberBehaviourSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 30.seconds

  // a server that reads this many commands, keeping them, and then sends the reply and closes the connection
  private def serving(commands: Int)(reply: String): Resource[IO, (Port, IO[List[RESP]])] =
    Resource.eval(IO.ref(List.empty[RESP])).flatMap { seen =>
      FakeRedisServer((_, in) => in.take(commands.toLong).evalMap(c => seen.update(_ :+ c)).drain ++ Stream.emit(reply)).map(_ -> seen.get)
    }

  private def command(params: String*): RESP = Arr(params.toList.map(Bulk(_)))

  private val feed: Feed[Message[String]] = laserdisc.redis_6_2.subscribe[String](OneOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))))
  private val subscribing                 = command("SUBSCRIBE", "a", "b")

  private def confirmation(name: String, count: Int)  = s"*3\r\n$$9\r\nsubscribe\r\n$$${name.length}\r\n$name\r\n:$count\r\n"
  private def message(channel: String, value: String) =
    s"*3\r\n$$7\r\nmessage\r\n$$${channel.length}\r\n$channel\r\n$$${value.length}\r\n$value\r\n"

  private val confirmed = confirmation("a", 1) + confirmation("b", 2)

  private def read(channel: String, value: String): Either[Feed.Unreadable, Message[String]] =
    Right(Message(Key.unsafeFrom(channel), value))

  test("a message that arrives between two confirmations isn't lost") {
    serving(1)(confirmation("a", 1) + message("a", "early") + confirmation("b", 2) + message("b", "late")).use { case (port, _) =>
      RedisSubscriber[IO]
        .to("127.0.0.1", port, feed)
        .use(_.take(2).compile.toList)
        .assertEquals(List(read("a", "early"), read("b", "late")))
    }
  }

  test("a run that stops early leaves the next messages for the next run") {
    serving(1)(confirmed + message("a", "first") + message("b", "second")).use { case (port, _) =>
      RedisSubscriber[IO]
        .to("127.0.0.1", port, feed)
        .use { messages =>
          IO.sleep(200.millis) >> (messages.take(1).compile.toList, messages.take(1).compile.toList).tupled
        }
        .assertEquals(List(read("a", "first")) -> List(read("b", "second")))
    }
  }

  test("a message that can't be read is handed over as it arrived, and the next ones still come, also while confirming") {
    val noChannel = message("", "lost")
    serving(1)(confirmation("a", 1) + noChannel + confirmation("b", 2) + noChannel + message("a", "next")).use { case (port, _) =>
      RedisSubscriber[IO]
        .to("127.0.0.1", port, feed)
        .use(_.take(3).compile.toList)
        .map { received =>
          val unreadable: RESP = Arr(Bulk("message"), Bulk(""), Bulk("lost"))
          assertEquals(received.map(_.left.map(_.message)), List(Left(unreadable), Left(unreadable), read("a", "next")))
        }
    }
  }

  test("a message whose reader throws is handed over as it arrived, and the next ones still come") {
    implicit val throwingOnBad: Read[Bulk, Checked] = Read.instance {
      case Bulk("bad") => throw new IllegalStateException("thrown")
      case Bulk(s)     => Right(Checked(s))
    }
    val checking = laserdisc.redis_6_2.subscribe[Checked](OneOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))))
    serving(1)(confirmed + message("a", "bad") + message("a", "next")).use { case (port, _) =>
      RedisSubscriber[IO]
        .to("127.0.0.1", port, checking)
        .use(_.take(2).compile.toList)
        .map { received =>
          assertEquals(received.map(_.left.map(_.error.getMessage)), List(Left("thrown"), Right(Message(Key("a"), Checked("next")))))
        }
    }
  }

  test("the stream fails when the server replies with an error") {
    serving(1)(confirmed + "-ERR boom\r\n").use { case (port, _) =>
      RedisSubscriber[IO].to("127.0.0.1", port, feed).use(_.compile.toList).attempt.map { outcome =>
        assertEquals(outcome.left.map(_.getMessage), Left("ERR boom"))
      }
    }
  }

  test("a message that arrives between two confirmations comes in the first run only") {
    serving(1)(confirmation("a", 1) + message("a", "early") + confirmation("b", 2) + message("b", "late")).use { case (port, _) =>
      RedisSubscriber[IO]
        .to("127.0.0.1", port, feed)
        .use { messages =>
          (messages.take(1).compile.toList, messages.take(1).compile.toList).tupled
        }
        .assertEquals(List(read("a", "early")) -> List(read("b", "late")))
    }
  }

  test("running the stream after the subscriber is closed fails with ClientTerminated, instead of waiting for messages that won't come") {
    serving(1)(confirmed).use { case (port, _) =>
      RedisSubscriber[IO].to("127.0.0.1", port, feed).use(IO.pure).flatMap { messages =>
        messages.compile.toList.attempt.timeout(5.seconds).map(outcome => assertEquals(outcome, Left(ClientTerminated)))
      }
    }
  }

  test("a stream running when the subscriber is closed fails with ClientTerminated") {
    // a server that confirms, then keeps the connection open with nothing more to say
    val staying = FakeRedisServer((_, in) => in.take(1).drain ++ Stream.emit(confirmed) ++ Stream.never[IO])
    staying.use { port =>
      RedisSubscriber[IO].to("127.0.0.1", port, feed).allocated.flatMap { case (messages, close) =>
        messages.compile.toList.attempt.start.flatMap { running =>
          IO.sleep(200.millis) >> close >> running.joinWithNever
            .timeout(5.seconds)
            .map(outcome => assertEquals(outcome, Left(ClientTerminated)))
        }
      }
    }
  }

  test("a slow stream with a full queue fails with ClientTerminated once the subscriber is closed, instead of waiting forever") {
    // confirms, then sends more messages than the queue holds, and stays connected
    val many    = (1 to 2000).map(i => message("a", s"m$i")).mkString
    val staying = FakeRedisServer((_, in) => in.take(1).as(confirmed + many) ++ Stream.never[IO])
    staying.use { port =>
      RedisSubscriber[IO].to("127.0.0.1", port, feed).allocated.flatMap { case (messages, close) =>
        // takes one message, then waits until the subscriber is closed before taking the rest
        IO.deferred[Unit].flatMap { closed =>
          messages.zipWithIndex.evalTap { case (_, i) => closed.get.whenA(i == 0L) }.compile.drain.attempt.start.flatMap { running =>
            IO.sleep(500.millis) >> close >> closed.complete(()) >> running.joinWithNever
              .timeout(5.seconds)
              .map(outcome => assertEquals(outcome, Left(ClientTerminated)))
          }
        }
      }
    }
  }

  test("two runs of the stream at once both fail when the connection drops, instead of one waiting forever") {
    serving(1)(confirmed).use { case (port, _) =>
      RedisSubscriber[IO].to("127.0.0.1", port, feed).use { messages =>
        (messages.compile.toList.attempt, messages.compile.toList.attempt).parTupled.timeout(5.seconds).map { case (first, second) =>
          // both fail, with the connection's failure: the end of the stream on the JVM, a reset on Node.js 26
          assert(first.isLeft, first.toString)
          assertEquals(second.left.map(_.getMessage), first.left.map(_.getMessage))
        }
      }
    }
  }

  test("logs in before subscribing") {
    serving(2)("+OK\r\n" + confirmed + message("a", "m")).use { case (port, seen) =>
      RedisSubscriber[IO]
        .to("127.0.0.1", port, Credentials(Key("secret")), feed)
        .use(_.take(1).compile.toList)
        .assertEquals(List(read("a", "m"))) >>
        seen.assertEquals(List(command("AUTH", "secret"), subscribing))
    }
  }

  test("a refused login fails opening with ConnectionSetupRefused, as with the client") {
    serving(1)("-WRONGPASS invalid username-password pair\r\n").use { case (port, seen) =>
      RedisSubscriber[IO].to("127.0.0.1", port, Credentials(Key("wrong")), feed).use(_ => IO.unit).attempt.flatMap { outcome =>
        val refused = outcome.left.toOption.collect { case ConnectionSetupRefused(address, reason) =>
          address.port.value -> reason.getMessage
        }
        assertEquals(refused, Some(port.value -> "WRONGPASS invalid username-password pair"))
        seen.map(commands => assertEquals(commands.take(1), List(command("AUTH", "wrong"))))
      }
    }
  }

  test("identified gives the connection's id, asked for before subscribing") {
    serving(2)(":42\r\n" + confirmed).use { case (port, seen) =>
      RedisSubscriber[IO].identified("127.0.0.1", port, feed).use { case (id, _) => IO.pure(id.value) }.assertEquals(42L) >>
        seen.assertEquals(List(command("CLIENT", "ID"), subscribing))
    }
  }

  test("identified logs in first") {
    serving(3)("+OK\r\n:42\r\n" + confirmed).use { case (port, seen) =>
      RedisSubscriber[IO]
        .identified("127.0.0.1", port, Credentials(Key("user"), Key("secret")), feed)
        .use { case (id, _) => IO.pure(id.value) }
        .assertEquals(42L) >>
        seen.assertEquals(List(command("AUTH", "user", "secret"), command("CLIENT", "ID"), subscribing))
    }
  }

  test("identified fails when what comes back isn't an id") {
    serving(2)("+OK\r\n" + confirmed).use { case (port, _) =>
      RedisSubscriber[IO].identified("127.0.0.1", port, feed).use(_ => IO.unit).attempt.map { outcome =>
        assert(outcome.left.exists(_.getMessage.contains("Expected the connection's id")), outcome.toString)
      }
    }
  }

  test("the stream fails when the server closes the connection") {
    serving(1)(confirmed).use { case (port, _) =>
      RedisSubscriber[IO].to("127.0.0.1", port, feed).use(_.compile.toList).attempt.map { outcome =>
        assert(outcome.left.exists(_.isInstanceOf[ServerTerminatedConnection]), outcome.toString)
      }
    }
  }

  test("running the stream again after the connection went fails again, instead of waiting for messages that won't come") {
    serving(1)(confirmed).use { case (port, _) =>
      RedisSubscriber[IO]
        .to("127.0.0.1", port, feed)
        .use(messages => (messages.compile.toList.attempt, messages.compile.toList.attempt.timeout(5.seconds)).tupled)
        .map { case (first, second) =>
          assert(first.left.exists(_.isInstanceOf[ServerTerminatedConnection]), first.toString)
          assert(second.left.exists(_.isInstanceOf[ServerTerminatedConnection]), second.toString)
        }
    }
  }

  private def keptAliveEvery(interval: FiniteDuration) = new RedisSubscriber.RedisSubscriberConnector[IO](interval)
  private val pong                                     = "*2\r\n$4\r\npong\r\n$0\r\n\r\n"

  test("the stream fails when the server stops answering, even though the connection is still open") {
    FakeRedisServer((_, in) => in.take(1).as(confirmed) ++ Stream.never[IO]).use { port =>
      keptAliveEvery(200.millis).to("127.0.0.1", port, feed).use(_.compile.toList).attempt.timeout(5.seconds).map { outcome =>
        assert(outcome.left.exists(_.isInstanceOf[ServerNotResponding]), outcome.toString)
      }
    }
  }

  test("a server that answers the pings keeps the stream going, and its answers aren't messages") {
    val answering = FakeRedisServer { (_, in) =>
      in.zipWithIndex.map {
        case (_, 0L) => confirmed
        case (_, 5L) => pong + message("a", "late")
        case _       => pong
      }
    }
    answering.use { port =>
      keptAliveEvery(100.millis).to("127.0.0.1", port, feed).use(_.take(1).compile.toList).assertEquals(List(read("a", "late")))
    }
  }

  test("a stream not taking its messages for a while isn't taken for a server that's gone") {
    val many = (1 to 2000).map(i => message("a", s"m$i")).mkString
    // the server answers each PING, as a real one does, so that only a full queue holding up reading could make it look gone
    FakeRedisServer((_, in) => in.zipWithIndex.map { case (_, i) => if (i == 0) confirmed + many else pong }).use { port =>
      keptAliveEvery(100.millis)
        .to("127.0.0.1", port, feed)
        .use(messages => IO.sleep(1.second) >> messages.take(2000).compile.toList.map(_.size))
        .assertEquals(2000)
    }
  }

  test("opening fails when the server closes the connection before confirming") {
    serving(1)(confirmation("a", 1)).use { case (port, _) =>
      RedisSubscriber[IO].to("127.0.0.1", port, feed).use(_ => IO.unit).attempt.map { outcome =>
        assert(outcome.left.exists(_.isInstanceOf[ServerTerminatedConnection]), outcome.toString)
      }
    }
  }

  test("opening fails with the server's error when it refuses the subscription itself") {
    serving(1)("-NOAUTH Authentication required.\r\n").use { case (port, _) =>
      RedisSubscriber[IO].to("127.0.0.1", port, feed).use(_ => IO.unit).attempt.map { outcome =>
        assert(outcome.left.exists(_.getMessage.startsWith("NOAUTH")), outcome.toString)
      }
    }
  }
}

private final case class Checked(value: String)
