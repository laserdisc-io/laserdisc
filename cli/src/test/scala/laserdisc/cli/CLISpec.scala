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
package cli

import _root_.fs2.Chunk
import cats.effect.{ExitCode, IO}
import laserdisc.fs2.*
import laserdisc.fs2.FakeRedisServer.*
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.*

final class CLISpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 60.seconds

  // what's on the screen after typing these lines, without the logo and with the timings blanked
  private def session(args: List[String], lines: String*): IO[(ExitCode, String)] =
    IO.ref(Vector.empty[Byte]).flatMap { screen =>
      val typed = Stream.chunk(Chunk.array(lines.mkString("\n").getBytes(UTF_8))).covary[IO]
      val shown = (bytes: Stream[IO, Byte]) => bytes.chunks.foreach(chunk => screen.update(_ ++ chunk.toList))

      CLI.impl.session(args, typed, shown).flatMap { code =>
        screen.get.map(bytes => code -> new String(bytes.toArray, UTF_8).replaceAll("\\[[0-9.,]+ms\\]", "[]"))
      }
    }

  private val replying = FakeRedisServer { (_, commands) =>
    commands.map {
      case Arr(Bulk("GET") :: _)  => bulk("value")
      case Arr(Bulk("BOOM") :: _) => "-ERR boom\r\n"
      case Arr(Bulk("KEYS") :: _) => "*2\r\n$1\r\na\r\n$1\r\nb\r\n"
      case Arr(Bulk(other) :: _)  => s"+$other\r\n"
      case other                  => sys.error(s"unexpected command $other")
    }
  }

  private def afterLogo(screen: String): String = screen.substring(screen.indexOf("127.0.0.1:"))

  test("without a host and a port it says so") {
    session(Nil).assertEquals(ExitCode.Error -> "please supply host and port (space separated)\n")
  }

  test("with an invalid host or port it says so") {
    val message = ExitCode.Error -> "please supply valid host and port (space separated)\n"
    session(List("not a host", "6379")).assertEquals(message) >>
      session(List("localhost", "not a port")).assertEquals(message) >>
      session(List("localhost", "0")).assertEquals(message)
  }

  test("shows the reply to each command typed, formatted, then prompts again") {
    replying.use { port =>
      session(List("127.0.0.1", port.value.toString), "PING", "GET a", "KEYS *").map { case (code, screen) =>
        val prompt = s"127.0.0.1:${port.value}> "
        assertEquals(code, ExitCode.Success)
        assertEquals(
          afterLogo(screen),
          s"""$prompt<<< PING - []
             |$prompt<<< "value" - []
             |$prompt<<< 1) "a"
             |2) "b" - []
             |$prompt""".stripMargin
        )
      }
    }
  }

  test("shows an error the server replies with") {
    replying.use { port =>
      session(List("127.0.0.1", port.value.toString), "BOOM").map { case (code, screen) =>
        assertEquals(code, ExitCode.Success)
        assert(screen.contains("<<< (error) ERR boom - []"), screen)
      }
    }
  }

  test("a blank line prompts again, and a line that can't be parsed says so, without sending anything") {
    replying.use { port =>
      session(List("127.0.0.1", port.value.toString), "", "SET \"unbalanced").map { case (code, screen) =>
        val prompt = s"127.0.0.1:${port.value}> "
        assertEquals(code, ExitCode.Success)
        assertEquals(afterLogo(screen), s"$prompt$prompt(error) Invalid argument(s)\n$prompt")
      }
    }
  }

  test("commands that would leave the shared connection out of step, or closed, say so, without sending anything") {
    val refused = List(
      "subscribe a"        -> "subscribe isn't supported here, as its messages keep coming after the reply",
      "PSUBSCRIBE a*"      -> "PSUBSCRIBE isn't supported here, as its messages keep coming after the reply",
      "SSUBSCRIBE a"       -> "SSUBSCRIBE isn't supported here, as its messages keep coming after the reply",
      "MONITOR"            -> "MONITOR isn't supported here, as its messages keep coming after the reply",
      "UNSUBSCRIBE a b"    -> "UNSUBSCRIBE isn't supported here, as it replies once for each channel",
      "PUNSUBSCRIBE"       -> "PUNSUBSCRIBE isn't supported here, as it replies once for each channel",
      "SUNSUBSCRIBE a"     -> "SUNSUBSCRIBE isn't supported here, as it replies once for each channel",
      "SYNC"               -> "SYNC isn't supported here, as it turns the connection over to replication",
      "PSYNC ? -1"         -> "PSYNC isn't supported here, as it turns the connection over to replication",
      "REPLCONF ACK 0"     -> "REPLCONF isn't supported here, as it's for replicas, and some of its forms get no reply",
      "client reply off"   -> "client reply off isn't supported here, as the server would stop replying",
      "CLIENT REPLY SKIP"  -> "CLIENT REPLY SKIP isn't supported here, as the server would stop replying",
      "HELLO 3 SETNAME me" -> "HELLO 3 isn't supported here, as the prompt reads RESP2 replies only",
      "QUIT"               -> "QUIT isn't supported here, as the server would close the connection. To leave, press Ctrl-D"
    )
    val allowed = List("CLIENT REPLY ON", "HELLO 2", "HELLO", "CLIENT LIST", "PING")

    IO.ref(List.empty[String]).flatMap { sent =>
      val recording = FakeRedisServer { (_, commands) =>
        commands.evalMap {
          case Arr(Bulk(name) :: _) => sent.update(_ :+ name).as(s"+$name\r\n")
          case other                => IO.raiseError(new Exception(s"unexpected command $other"))
        }
      }
      recording.use { port =>
        session(List("127.0.0.1", port.value.toString), (refused.map(_._1) ++ allowed)*).flatMap { case (code, screen) =>
          val prompt = s"127.0.0.1:${port.value}> "
          assertEquals(code, ExitCode.Success)
          assertEquals(
            afterLogo(screen),
            refused.map { case (_, message) => s"$prompt(error) $message\n" }.mkString +
              allowed.map(line => s"$prompt<<< ${line.split(' ').head} - []\n").mkString + prompt
          )
          sent.get.assertEquals(allowed.map(_.split(' ').head))
        }
      }
    }
  }

  test("shows an error when no server can be reached") {
    unusedPort.use { port =>
      session(List("127.0.0.1", port.value.toString), "PING").map { case (code, screen) =>
        assertEquals(code, ExitCode.Success)
        assert(screen.contains("<<< ERROR "), screen)
      }
    }
  }
}
