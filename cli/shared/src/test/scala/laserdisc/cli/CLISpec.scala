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

import _root_.fs2.{Chunk, Stream}
import cats.effect.{ExitCode, IO}
import laserdisc.fs2.*
import laserdisc.fs2.FakeRedisServer.*
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.*

final class CLISpec extends CatsEffectSuite {

  override val munitIOTimeout: Duration = 60.seconds

  // what's on the screen after typing these lines, without the logo and with the timings blanked
  private def session(args: List[String], lines: String*): IO[(ExitCode, String)] = sessionWithin(20.seconds, args, lines*)

  // the same, with each command given this long, on top of its wait
  private def sessionWithin(timeout: FiniteDuration, args: List[String], lines: String*): IO[(ExitCode, String)] =
    IO.ref(Vector.empty[Byte]).flatMap { screen =>
      val typed = Stream.chunk(Chunk.array(lines.mkString("\n").getBytes(UTF_8))).covary[IO]
      val shown = (bytes: Stream[IO, Byte]) => bytes.chunks.foreach(chunk => screen.update(_ ++ chunk.toList))

      CLI.impl.session(args, Map.empty, typed, shown, timeout).flatMap { code =>
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

  // what's wrong with the options given after the host and the port, or instead of them
  private def errors(args: List[String]): List[String] = CLI.impl.options(args).left.toOption.toList.flatMap(_.errors)

  test("without a host and a port, or with an invalid one, it says so, with what to type, and exits with an error") {
    session(Nil).map { case (code, screen) =>
      assertEquals(code, ExitCode.Error)
      assert(screen.contains("Usage: laserdisc-cli"), screen)
    } >>
      IO {
        assertEquals(errors(Nil), List("Missing expected positional argument!"))
        assertEquals(errors(List("not a host", "6379")), List("not a host isn't a host"))
        assertEquals(errors(List("localhost", "not a port")), List("Invalid integer: not a port"))
        assertEquals(errors(List("localhost", "0")), List("0 isn't a port from 1 to 65535"))
      }
  }

  test("--help shows what to type, and exits without an error") {
    session(List("--help")).map { case (code, screen) =>
      assertEquals(code, ExitCode.Success)
      List(
        "Usage: laserdisc-cli",
        "--password <password>, -a <password>",
        "--user <user>",
        "--database <number>, -n <number>",
        "REDISCLI_AUTH"
      )
        .foreach(expected => assert(screen.contains(expected), screen))
    }
  }

  test("takes a password, a user and a database after the host and port, in any order") {
    import laserdisc.auto.*
    val parsed = CLI.impl.options(List("localhost", "6379", "-n", "2", "--user", "me", "-a", "secret"))
    assertEquals(parsed, Right(CLI.impl.Options("localhost", 6379, Some(Credentials("me", "secret")), Some(DbIndex(2)))))
    assertEquals(CLI.impl.options(List("localhost", "6379", "--password=secret")).map(_.credentials), Right(Some(Credentials("secret"))))
    assertEquals(CLI.impl.options(List("localhost", "6379", "-a", "secret")).map(_.credentials), Right(Some(Credentials("secret"))))
  }

  test("connects with TLS given --tls, trusting what the system trusts, the authorities of --cacert or, with --insecure, anything") {
    def tls(options: String*) = CLI.impl.options(List("localhost", "6379") ++ options).map(_.tls)
    assertEquals(tls(), Right(None))
    assertEquals(tls("--tls"), Right(Some(CLI.impl.TlsWith.SystemTrust)))
    assertEquals(tls("--tls", "--cacert", "ca.pem"), Right(Some(CLI.impl.TlsWith.Authorities("ca.pem"))))
    assertEquals(tls("--cacert", "ca.pem", "--tls"), Right(Some(CLI.impl.TlsWith.Authorities("ca.pem"))))
    assertEquals(tls("--tls", "--insecure"), Right(Some(CLI.impl.TlsWith.NoChecks)))
  }

  test("a file of authorities that can't be read says so, and exits with an error, without connecting") {
    session(List("127.0.0.1", "1", "--tls", "--cacert", "/no/such/ca.pem"), "PING").map { case (code, screen) =>
      assertEquals(code, ExitCode.Error)
      assert(screen.contains("ERROR can't read the certificate authorities in /no/such/ca.pem: "), screen)
      assert(!screen.contains("127.0.0.1:1>"), screen)
    }
  }

  test("takes the password from REDISCLI_AUTH, unless it's given with -a") {
    import laserdisc.auto.*
    def credentials(env: Map[String, String], options: String*) =
      CLI.impl.options(List("localhost", "6379") ++ options, env).map(_.credentials)
    val fromEnv = Map("REDISCLI_AUTH" -> "fromenv")
    assertEquals(credentials(fromEnv), Right(Some(Credentials("fromenv"))))
    assertEquals(credentials(fromEnv, "--user", "me"), Right(Some(Credentials("me", "fromenv"))))
    assertEquals(credentials(fromEnv, "-a", "secret"), Right(Some(Credentials("secret"))))
    assertEquals(credentials(Map("REDISCLI_AUTH" -> "-starts-with-a-dash")), Right(Some(Credentials("-starts-with-a-dash"))))
    assertEquals(credentials(Map.empty), Right(None))
  }

  test("says what's wrong with the options") {
    val wrong = List(
      List("-a")                                        -> "Missing value for option: -a",
      List("-n")                                        -> "Missing value for option: -n",
      List("--user", "me")                              -> "please supply the password with -a or REDISCLI_AUTH as well as --user",
      List("-a", "--user", "me")                        -> "Unexpected argument: me",
      List("-a", "-x")                                  -> "please supply a password after -a, which can't start with -",
      List("--user", "-a", "x")                         -> "Unexpected argument: x",
      List("-n", "two")                                 -> "Invalid integer: two",
      List("-n", "-1")                                  -> "-1 isn't a database number from 0",
      List("--bogus", "x")                              -> "Unexpected option: --bogus",
      List("--cacert", "ca.pem")                        -> "please supply --tls as well as --cacert",
      List("--insecure")                                -> "please supply --tls as well as --insecure",
      List("--tls", "--insecure", "--cacert", "ca.pem") -> "please supply --cacert or --insecure, not both"
    )
    wrong.foreach { case (options, message) =>
      assertEquals(errors(List("localhost", "6379") ++ options), List(message), options)
    }
  }

  test("logs in and selects the database before the first command, and shows the database in the prompt") {
    IO.ref(List.empty[List[String]]).flatMap { sent =>
      val recording = FakeRedisServer { (_, commands) =>
        commands.evalMap {
          case Arr(words) => sent.update(_ :+ words.collect { case Bulk(w) => w }).as("+OK\r\n")
          case other      => IO.raiseError(new Exception(s"unexpected command $other"))
        }
      }
      recording.use { port =>
        session(List("127.0.0.1", port.value.toString, "--user", "me", "-a", "secret", "-n", "2"), "PING").flatMap { case (code, screen) =>
          val prompt = s"127.0.0.1:${port.value}[2]> "
          assertEquals(code, ExitCode.Success)
          assertEquals(afterLogo(screen), s"$prompt<<< OK - []\n$prompt")
          sent.get.assertEquals(List(List("AUTH", "me", "secret"), List("SELECT", "2"), List("PING")))
        }
      }
    }
  }

  test("stops with an error when the server refuses the password, without sending what's typed next") {
    IO.ref(List.empty[String]).flatMap { sent =>
      val refusing = FakeRedisServer { (_, commands) =>
        commands.evalMap {
          case Arr(Bulk(name) :: _) =>
            sent.update(_ :+ name).as(if (name == "AUTH") "-WRONGPASS invalid username-password pair\r\n" else s"+$name\r\n")
          case other => IO.raiseError(new Exception(s"unexpected command $other"))
        }
      }
      refusing.use { port =>
        session(List("127.0.0.1", port.value.toString, "-a", "wrong"), "PING", "GET a").flatMap { case (code, screen) =>
          assertEquals(code, ExitCode.Error)
          assert(afterLogo(screen).contains("ERROR Server 127.0.0.1"), screen)
          assert(screen.endsWith("refused to set up the connection: WRONGPASS invalid username-password pair\n"), screen)
          sent.get.map(names => assert(!names.contains("PING") && !names.contains("GET"), names))
        }
      }
    }
  }

  test("shows a refusal's control characters as \\xHH, as for a reply") {
    val refusing = FakeRedisServer((_, commands) => commands.as("-WRONGPASS \u001b[2J\r\n"))
    refusing.use { port =>
      session(List("127.0.0.1", port.value.toString, "-a", "wrong"), "PING").map { case (_, screen) =>
        assert(screen.endsWith("refused to set up the connection: WRONGPASS \\x1b[2J\n"), screen)
      }
    }
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
      "QUIT"               -> "QUIT isn't supported here, as the server would close the connection. To leave, press Ctrl-D",
      "AUTH me secret"     ->
        "AUTH isn't supported here, as a new connection would forget it. Start the prompt with -a, and --user if there's one",
      "select 2"               -> "select isn't supported here, as a new connection would forget it. Start the prompt with -n",
      "hello 2 auth me secret" ->
        "hello AUTH isn't supported here, as a new connection would forget it. Start the prompt with -a, and --user if there's one",
      "RESET" -> "RESET isn't supported here, as it would log out and go back to database 0. Start the prompt again to change them"
    )
    val allowed = List("CLIENT REPLY ON", "HELLO 2", "HELLO", "HELLO 2 SETNAME me", "CLIENT LIST", "PING")

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

  test("a command typed with a wait of 0 waits for as long as it takes, and one with a wait gets its wait on top of the timeout") {
    IO.ref(List.empty[List[String]]).flatMap { sent =>
      val slow = FakeRedisServer { (_, commands) =>
        commands.evalMap {
          case Arr(words) =>
            val typed = words.collect { case Bulk(w) => w }
            sent.update(_ :+ typed) >> IO.sleep(1500.millis).as(s"*2\r\n${bulk(typed(1))}${bulk("v")}")
          case other => IO.raiseError(new Exception(s"unexpected command $other"))
        }
      }
      slow.use { port =>
        sessionWithin(500.millis, List("127.0.0.1", port.value.toString), "BLPOP a 0", "BLPOP b 2").flatMap { case (code, screen) =>
          val prompt = s"127.0.0.1:${port.value}> "
          assertEquals(code, ExitCode.Success)
          assertEquals(
            afterLogo(screen),
            s"""$prompt<<< 1) "a"
               |2) "v" - []
               |$prompt<<< 1) "b"
               |2) "v" - []
               |$prompt""".stripMargin
          )
          sent.get.assertEquals(List(List("BLPOP", "a", "0"), List("BLPOP", "b", "2")))
        }
      }
    }
  }

  test("shows an error when a command times out, and prompts again") {
    val silent = FakeRedisServer((_, commands) => commands.evalMap(_ => IO.never[String]))

    silent.use { port =>
      sessionWithin(500.millis, List("127.0.0.1", port.value.toString), "GET a").map { case (code, screen) =>
        val prompt = s"127.0.0.1:${port.value}> "
        assertEquals(code, ExitCode.Success)
        assertEquals(afterLogo(screen), s"$prompt<<< ERROR The request Protocol(GET) timed-out - []\n$prompt")
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
