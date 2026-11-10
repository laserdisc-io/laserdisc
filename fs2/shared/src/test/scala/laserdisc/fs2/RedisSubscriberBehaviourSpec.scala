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

  test("a message that arrives between two confirmations isn't lost") {
    serving(1)(confirmation("a", 1) + message("a", "early") + confirmation("b", 2) + message("b", "late")).use { case (port, _) =>
      RedisSubscriber[IO]
        .to("127.0.0.1", port, feed)
        .use(_.take(2).compile.toList)
        .assertEquals(List(Message(Key("a"), "early"), Message(Key("b"), "late")))
    }
  }

  test("a run that stops early leaves the next messages for the next run") {
    serving(1)(confirmed + message("a", "first") + message("b", "second")).use { case (port, _) =>
      RedisSubscriber[IO]
        .to("127.0.0.1", port, feed)
        .use { messages =>
          IO.sleep(200.millis) >> (messages.take(1).compile.toList, messages.take(1).compile.toList).tupled
        }
        .assertEquals(List(Message(Key("a"), "first")) -> List(Message(Key("b"), "second")))
    }
  }

  test("a message that arrives between two confirmations comes in the first run only") {
    serving(1)(confirmation("a", 1) + message("a", "early") + confirmation("b", 2) + message("b", "late")).use { case (port, _) =>
      RedisSubscriber[IO]
        .to("127.0.0.1", port, feed)
        .use { messages =>
          (messages.take(1).compile.toList, messages.take(1).compile.toList).tupled
        }
        .assertEquals(List(Message(Key("a"), "early")) -> List(Message(Key("b"), "late")))
    }
  }

  test("logs in before subscribing") {
    serving(2)("+OK\r\n" + confirmed + message("a", "m")).use { case (port, seen) =>
      RedisSubscriber[IO]
        .to("127.0.0.1", port, Key("secret"), feed)
        .use(_.take(1).compile.toList)
        .assertEquals(List(Message(Key("a"), "m"))) >>
        seen.assertEquals(List(command("AUTH", "secret"), subscribing))
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
      RedisSubscriber[IO].identified("127.0.0.1", port, Key("secret"), feed).use { case (id, _) => IO.pure(id.value) }.assertEquals(42L) >>
        seen.assertEquals(List(command("AUTH", "secret"), command("CLIENT", "ID"), subscribing))
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

  test("opening fails when the server closes the connection before confirming") {
    serving(1)(confirmation("a", 1)).use { case (port, _) =>
      RedisSubscriber[IO].to("127.0.0.1", port, feed).use(_ => IO.unit).attempt.map { outcome =>
        assert(outcome.left.exists(_.isInstanceOf[ServerTerminatedConnection]), outcome.toString)
      }
    }
  }

  test("opening fails when the server refuses") {
    serving(1)("-NOAUTH Authentication required.\r\n").use { case (port, _) =>
      RedisSubscriber[IO].to("127.0.0.1", port, feed).use(_ => IO.unit).attempt.map { outcome =>
        assert(outcome.left.exists(_.getMessage.startsWith("NOAUTH")), outcome.toString)
      }
    }
  }
}
