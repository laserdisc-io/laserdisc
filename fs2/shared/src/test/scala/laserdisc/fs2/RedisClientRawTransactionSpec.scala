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
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import laserdisc.auto.*
import laserdisc.protocol.RESPRead
import laserdisc.fs2.FakeRedisServer.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

// a transaction typed as plain commands, as at the CLI, whose connection is replaced before it ends
final class RedisClientRawTransactionSpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 30.seconds

  private def typed(words: String*): Protocol[RESP] = Protocol(words.head, words.tail.toList).using(RESPRead.raw)

  private def words(command: RESP): List[String] =
    command match {
      case Arr(elements) => elements.collect { case Bulk(word) => word }
      case other         => sys.error(s"unexpected command $other")
    }

  // the first connection answers this many commands and closes, as a server closing idle clients does; the others answer every command,
  // keeping what they got
  private def closingFirst(answered: Int): Resource[IO, (Port, Ref[IO, Vector[List[String]]])] =
    Resource.eval(IO.ref(Vector.empty[List[String]])).flatMap { later =>
      FakeRedisServer { (n, requests) =>
        val replies = requests.map(words).evalMap { ws =>
          val reply = ws.headOption.map(_.toUpperCase) match {
            case Some("SET" | "MULTI" | "WATCH" | "DISCARD") if n == 1 => if (ws.head == "SET") "+QUEUED\r\n" else "+OK\r\n"
            case Some("GET")                                           => bulk("v")
            case _                                                     => "+OK\r\n"
          }
          later.update(_ :+ ws).whenA(n > 1).as(reply)
        }
        if (n == 1) replies.take(answered.toLong) else replies
      }.map(_ -> later)
    }

  private def after(client: RedisClient[IO], command: Protocol[RESP]): IO[Either[Throwable, RESP]] =
    IO.sleep(300.millis) >> client.send(command, 5.seconds)

  test("a connection that sent nothing, as everything was refused with TransactionLost, closing is no failure: the next send is answered") {
    // the first connection answers MULTI and closes, the second closes on its own a moment after it opens, and the others answer
    val server = FakeRedisServer { (n, requests) =>
      if (n == 1) requests.take(1).as("+OK\r\n")
      else if (n == 2) Stream.sleep_[IO](300.millis)
      else requests.as(bulk("v"))
    }
    server.use { port =>
      RedisClient[IO].to("127.0.0.1", port, reconnect = Reconnect(5.seconds, 5.seconds)).use { client =>
        for {
          multi   <- client.send(typed("MULTI"))
          discard <- after(client, typed("DISCARD"))
          get     <- IO.sleep(600.millis) >> client.send(typed("GET", "a"), 2.seconds)
        } yield {
          assertEquals(multi, Right(Str("OK")))
          assert(discard.left.exists(_.isInstanceOf[TransactionLost]), discard)
          assertEquals(get, Right(Bulk("v")))
        }
      }
    }
  }

  test("what's typed after MULTI on a connection that's gone fails with TransactionLost until EXEC or DISCARD, without being sent") {
    closingFirst(answered = 2).use { case (port, later) =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        for {
          multi  <- client.send(typed("MULTI"))
          queued <- client.send(typed("SET", "a", "1"))
          set    <- after(client, typed("SET", "b", "2"))
          exec   <- client.send(typed("EXEC"))
          get    <- client.send(typed("GET", "c"))
          sent   <- later.get
        } yield {
          assertEquals(multi, Right(Str("OK")))
          assertEquals(queued, Right(Str("QUEUED")))
          assert(set.left.exists(_.isInstanceOf[TransactionLost]), set)
          assert(exec.left.exists(_.isInstanceOf[TransactionLost]), exec)
          assertEquals(get, Right(Bulk("v")))
          assertEquals(sent, Vector(List("GET", "c")))
        }
      }
    }
  }

  test("MULTI typed after a WATCH on a connection that's gone fails with WatchLost, and what follows it until EXEC isn't sent") {
    closingFirst(answered = 1).use { case (port, later) =>
      RedisClient[IO].to("127.0.0.1", port).use { client =>
        for {
          watch <- client.send(typed("WATCH", "k"))
          multi <- after(client, typed("MULTI"))
          set   <- client.send(typed("SET", "k", "mine"))
          exec  <- client.send(typed("EXEC"))
          get   <- client.send(typed("GET", "k"))
          sent  <- later.get
        } yield {
          assertEquals(watch, Right(Str("OK")))
          assert(multi.left.exists(_.isInstanceOf[WatchLost]), multi)
          assert(set.left.exists(_.isInstanceOf[TransactionLost]), set)
          assert(exec.left.exists(_.isInstanceOf[TransactionLost]), exec)
          assertEquals(get, Right(Bulk("v")))
          assertEquals(sent, Vector(List("GET", "k")))
        }
      }
    }
  }
}
