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
import laserdisc.auto.*
import laserdisc.protocol.Feed
import laserdisc.protocol.Feed.Message

import java.io.{BufferedReader, InputStreamReader}
import java.net.{InetAddress, ServerSocket}
import scala.concurrent.duration.*

final class RedisSubscriberBehaviourSpec extends munit.FunSuite {
  import cats.effect.unsafe.implicits.global

  private def serving(reply: BufferedReader => String): Int = {
    val server = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    val thread = new Thread(() => {
      val socket = server.accept()
      val in     = new BufferedReader(new InputStreamReader(socket.getInputStream))
      socket.getOutputStream.write(reply(in).getBytes("UTF-8"))
      socket.getOutputStream.flush()
      Thread.sleep(100L)
      socket.close()
      server.close()
    })
    thread.setDaemon(true)
    thread.start()
    server.getLocalPort
  }

  // the arguments of the next command it reads
  private def command(in: BufferedReader): List[String] = {
    val count = in.readLine().drop(1).toInt
    List.fill(count) {
      in.readLine()
      in.readLine()
    }
  }

  private val feed: Feed[Message[String]] = laserdisc.redis_6_2.subscribe[String](OneOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"))))

  private def confirmation(name: String, count: Int)  = s"*3\r\n$$9\r\nsubscribe\r\n$$${name.length}\r\n$name\r\n:$count\r\n"
  private def message(channel: String, value: String) =
    s"*3\r\n$$7\r\nmessage\r\n$$${channel.length}\r\n$channel\r\n$$${value.length}\r\n$value\r\n"

  private def port(n: Int): Port = Port.unsafeFrom(n)

  test("a message that arrives between two confirmations isn't lost") {
    val p = serving { in =>
      command(in)
      confirmation("a", 1) + message("a", "early") + confirmation("b", 2) + message("b", "late")
    }
    val received = RedisSubscriber[IO].to("127.0.0.1", port(p), feed).use(_.take(2).compile.toList).timeout(5.seconds).unsafeRunSync()
    assertEquals(received, List(Message(Key("a"), "early"), Message(Key("b"), "late")))
  }

  test("logs in before subscribing") {
    var seen: List[List[String]] = Nil
    val p                        = serving { in =>
      val auth      = command(in)
      val subscribe = command(in)
      seen = List(auth, subscribe)
      "+OK\r\n" + confirmation("a", 1) + confirmation("b", 2) + message("a", "m")
    }
    val received =
      RedisSubscriber[IO].to("127.0.0.1", port(p), Key("secret"), feed).use(_.take(1).compile.toList).timeout(5.seconds).unsafeRunSync()
    assertEquals(received, List(Message(Key("a"), "m")))
    assertEquals(seen, List(List("AUTH", "secret"), List("SUBSCRIBE", "a", "b")))
  }

  test("identified gives the connection's id, asked for before subscribing") {
    var seen: List[List[String]] = Nil
    val p                        = serving { in =>
      val id        = command(in)
      val subscribe = command(in)
      seen = List(id, subscribe)
      ":42\r\n" + confirmation("a", 1) + confirmation("b", 2)
    }
    val id =
      RedisSubscriber[IO].identified("127.0.0.1", port(p), feed).use { case (id, _) => IO.pure(id) }.timeout(5.seconds).unsafeRunSync()
    assertEquals(id.value, 42L)
    assertEquals(seen, List(List("CLIENT", "ID"), List("SUBSCRIBE", "a", "b")))
  }

  test("the stream fails when the server closes the connection") {
    val p = serving { in =>
      command(in)
      confirmation("a", 1) + confirmation("b", 2)
    }
    val outcome = RedisSubscriber[IO].to("127.0.0.1", port(p), feed).use(_.compile.toList).attempt.timeout(5.seconds).unsafeRunSync()
    assert(outcome.left.exists(_.isInstanceOf[ServerTerminatedConnection]), outcome.toString)
  }

  test("opening fails when the server refuses") {
    val p = serving { in =>
      command(in)
      "-NOAUTH Authentication required.\r\n"
    }
    val outcome = RedisSubscriber[IO].to("127.0.0.1", port(p), feed).use(_ => IO.unit).attempt.timeout(5.seconds).unsafeRunSync()
    assert(outcome.left.exists(_.getMessage.startsWith("NOAUTH")), outcome.toString)
  }
}
