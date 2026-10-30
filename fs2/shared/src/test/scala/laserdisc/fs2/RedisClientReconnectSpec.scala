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

import _root_.fs2.Chunk
import _root_.fs2.io.net.ServerSocket
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.comcast.ip4s
import laserdisc.all.*
import laserdisc.auto.*
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.*

final class RedisClientReconnectSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 60.seconds

  private def bulk(s: String): String = s"$$${s.length}\r\n$s\r\n"

  private val bindInRange: Resource[IO, (ServerSocket[IO], Port)] =
    Resource.eval(IO(20000 + scala.util.Random.nextInt(20000))).flatMap { p =>
      Network[IO]
        .bind(ip4s.SocketAddress(ip4s.Ipv4Address.fromString("127.0.0.1").get, ip4s.Port.fromInt(p).get))
        .map(_ -> Port.unsafeFrom(p))
    }

  private def withRetries[A](r: Resource[IO, A], attempts: Int = 10): Resource[IO, A] =
    r.handleErrorWith { (e: Throwable) =>
      if (attempts > 1) withRetries(r, attempts - 1) else Resource.raiseError[IO, A, Throwable](e)
    }

  // a server that answers the commands of its n-th connection (from 1) with the replies of `handle`
  private def fakeServer(handle: (Int, Stream[IO, RESP]) => Stream[IO, String]): Resource[IO, Port] =
    withRetries(bindInRange).flatMap { case (server, port) =>
      Resource.eval(IO.ref(0)).flatMap { connections =>
        server.accept
          .map { socket =>
            Stream.eval(connections.updateAndGet(_ + 1)).flatMap { n =>
              handle(n, socket.reads.through(RedisChannel.impl.receiveResp[IO]))
                .flatMap(reply => Stream.chunk(Chunk.array(reply.getBytes(UTF_8))))
                .through(socket.writes) ++ Stream.exec(socket.endOfOutput)
            }
          }
          .parJoinUnbounded
          .compile
          .drain
          .background
          .as(port)
      }
    }

  private val unusedPort: Resource[IO, Port] =
    Resource.eval(withRetries(bindInRange).use(bound => IO.pure(bound._2)))

  private def address(port: Port): RedisAddress = RedisAddress("127.0.0.1", port)

  // retries until the server replies, as the client fails commands while it is disconnected
  private def eventually[A](send: IO[Maybe[A]]): IO[A] =
    IO.monotonic.flatMap { start =>
      def loop: IO[A] =
        send.timeout(5.seconds).attempt.flatMap {
          case Right(Right(a)) => IO.pure(a)
          case other           =>
            IO.monotonic.flatMap { now =>
              if (now - start < 15.seconds) IO.sleep(200.millis) >> loop
              else IO.raiseError(new RuntimeException(s"no reply in time, last: $other"))
            }
        }
      loop
    }

  test("toNodes connects to the next address when one is unreachable") {
    (fakeServer((_, requests) => requests.as(bulk("alive"))), unusedPort).tupled.use { case (alive, dead) =>
      RedisClient[IO].toNodes(Set(address(alive), address(dead))).use { client =>
        eventually(client.send(get[String]("k"), 2.seconds)).assertEquals(Some("alive"))
      }
    }
  }

  test("toNodes connects to the next address when the connected one goes away") {
    val leaving   = fakeServer((n, requests) => if (n == 1) requests.take(1).as(bulk("first")) else Stream.empty)
    val remaining = fakeServer((_, requests) => requests.as(bulk("second")))

    (leaving, remaining).tupled.use { case (first, second) =>
      RedisClient[IO].toNodes(Set(address(first), address(second))).use { client =>
        for {
          r1 <- eventually(client.send(get[String]("k"), 2.seconds))
          r2 <- eventually(
            client.send(get[String]("k"), 2.seconds).map(_.flatMap(r => if (r == r1) Left(new Exception(s"still $r")) else Right(r)))
          )
        } yield assertEquals(Set(r1, r2), Set(Option("first"), Option("second")))
      }
    }
  }

  test("a request in flight when the connection drops fails, and later replies go to the right requests") {
    val dropsFirstConnection = fakeServer { (n, requests) =>
      if (n == 1) requests.take(1).drain
      else requests.zipWithIndex.map { case (_, i) => bulk(s"reply-$i") }
    }

    dropsFirstConnection.use { port =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        for {
          dropped <- eventually(client.send(get[String]("k"), 2.seconds).map(r => Right(r)))
          r1      <- eventually(client.send(get[String]("k"), 2.seconds))
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
