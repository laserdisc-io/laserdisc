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

import java.nio.charset.StandardCharsets.UTF_8
import _root_.fs2.{Pipe, Stream, io, text}
import cats.effect.*
import cats.data.ValidatedNel
import cats.syntax.all.*
import com.monovore.decline.{Command, Help, Opts}
import laserdisc.fs2.*
import laserdisc.protocol.{Forever, RESPRead}

import scala.concurrent.duration.*

object CLI extends IOApp { self =>

  private final val logo =
    """
      |                                            ,,,,'''''',,,,
      |                                   ./oydmNmmmmmmmmmmmmmmmmmmNmdyo/-
      |                              `/sdNmmmmmmNNNNMMMMMMMMMMMMMMNNNmmmmNNds/`
      |                           -odNmddmNNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMNNmmmNms:
      |                        -smmddmNNNNNNNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNmmNNy-
      |                     `+dmddNNNNNNNNNNNNNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNmmNmo`
      |                   `oNddmNNNNNNNNNNNNNNNNNNNNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNmmNs.
      |                  +NddNNNNNNNNNNNNNNNNNNNNNNNNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNmmNs`
      |                -mdhNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNdmN/
      |               sNhmNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNdNh`
      |             `ddhNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMNdNN-
      |            .mhdNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMmmM:
      |           `mymNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMMmmM:
      |           dhdNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMMMmmN-
      |          odhNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNmddddddddmNMMNNNNMMMMMMMMMMMMMMMMMMMMMMMMMMdmh
      |         .NsNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNhyhdmmNNNmmmdhhdNMNNNNMMMMMMMMMMMMMMMMMMMNNNNNNss
      |         symNNNNNNNNNNNNNNNNNNNNNNNNNNNNNyydNNmNNNNmmNNNNmdydMMNNNMMMMMMMMMNNNNNNNNNNNNNNNd.
      |         NsNNNNNNNNNNNNNNNNNNNNNNNNNNNNmohNNNNmmmmmmmmmNNNNmyyNMNNNNNNNNNNNNNNNNNNNNNNNNNNN/
      |        -dhNNNNNNNNNNNNNNNNNNNNNNNNNNNNohNNNNNmy+:--:oyNNNNNNysNNNNNNNNNNNNNNNNNNNNNNNNNNNNs`
      |        +hdNNNNNNNNNNNNNNNNNNNNNNNNNNNysNNNNNd:`      ./dNNNNdodNNNNNNNNNNNNNNNNNNNNNNNNNNNh.
      |        /ymNNNNNNNNNNNNNNNNNNNNNNNNNNNsyNNNNN/`        .+NNNNmshNNNNNNNNNNNNNNNNNNNNNNNNNNNd.
      |        -ymNNNNNNNNNNNNNNNNNNNNNNNNNNNssNNNNNo.        -sNNNNmodNNNNNNNNNNNNNNNNNNNNNNNNNNNd.
      |         shNNNNNNNNNNNNNNNNNNNNNNNNNNNm+dNNNNms-`    `:sNNNNNh+NNNNNNNNNNNNNNNNNNNNNNNNNNNNy`
      |         :sNNNNNNNNNNNNNNNNNNNNNNNNNNNNhodNNNNNmhyssyhNNNNNNh+mNNNNNNNNNNNNNNNNNNNNNNNNNNNNo
      |          /NNNNNNNNNNNNNNNNNNNNNNNNNNNNNdoymNNNNNNNNNNNNNNmssmNNNNNNNNNNNNNNNNNNNNNNNNNNNNm:
      |          .hNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNhssdmNNNNNNNmmhssdNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNy`
      |           :mNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNmhysssssssyydNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNm-
      |           `oNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNm+
      |            `sNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNo`
      |             `sNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNmo`
      |              `omNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNm+`
      |                /dNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNd:
      |                 .smNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNmo`
      |                   -ymNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNmy-
      |                     :ymNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNmy-
      |                       -sdNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNdo.
      |                         `:ydNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNds:`
      |                            `:ohmmmNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNmmmho-`
      |                                .:oydmmmmmmmmmmmmmmmmmmmmmmmmmmmmdy+:`
      |                                    ``-/+syhddmmmmmmmmmmddhys+/-`
      |                                             ````....````
      |""".stripMargin

  private final val bufSize = 10 * 1024

  // how long a command may take, on top of the wait it's given
  private final val timeout = 20.seconds

  // a password can also come from REDISCLI_AUTH, as with redis-cli, so that it isn't shown in the list of running programs
  override final def run(args: List[String]): IO[ExitCode] =
    std.Env[IO].get("REDISCLI_AUTH").flatMap { password =>
      val env = password.filter(_.nonEmpty).map("REDISCLI_AUTH" -> _).toMap
      impl.session(args, env, io.stdin[IO](bufSize), io.stdout[IO], timeout)
    }

  private[cli] object impl {
    // what's typed is sent on a connection the prompt shares with the next commands, each taking the next reply: these would leave it
    // out of step, or closed
    def refusal(command: String, args: List[String]): Option[String] = {
      def is(names: String*)                     = names.exists(_.equalsIgnoreCase(command))
      def refused(typed: String, reason: String) = Some(s"$typed isn't supported here, as $reason")

      if (is("SUBSCRIBE", "PSUBSCRIBE", "SSUBSCRIBE", "MONITOR")) refused(command, "its messages keep coming after the reply")
      else if (is("UNSUBSCRIBE", "PUNSUBSCRIBE", "SUNSUBSCRIBE")) refused(command, "it replies once for each channel")
      else if (is("SYNC", "PSYNC")) refused(command, "it turns the connection over to replication")
      else if (is("REPLCONF")) refused(command, "it's for replicas, and some of its forms get no reply")
      else if (is("CLIENT") && args.size >= 2 && args.head.equalsIgnoreCase("REPLY") && !args(1).equalsIgnoreCase("ON"))
        refused(s"$command ${args.take(2).mkString(" ")}", "the server would stop replying")
      else if (is("HELLO") && args.headOption.contains("3")) refused(s"$command 3", "the prompt reads RESP2 replies only")
      else if (is("HELLO") && args.exists(_.equalsIgnoreCase("AUTH")))
        refused(s"$command AUTH", "a new connection would forget it. Start the prompt with -a, and --user if there's one")
      else if (is("RESET")) refused(command, "it would log out and go back to database 0. Start the prompt again to change them")
      else if (is("QUIT")) refused(command, "the server would close the connection. To leave, press Ctrl-D")
      else if (is("AUTH")) refused(command, "a new connection would forget it. Start the prompt with -a, and --user if there's one")
      else if (is("SELECT")) refused(command, "a new connection would forget it. Start the prompt with -n")
      else None
    }

    // an error's message, which can hold what the server sent, or the error itself when it has none
    private def messageOf(error: Throwable): String = RESPFormat.printable(Option(error.getLocalizedMessage).getOrElse(error.toString))

    // what the prompt connects to and how: each connection logs in and selects the database first, so a new one does too
    final case class Options(host: Host, port: Port, credentials: Option[Credentials], database: Option[DbIndex])

    // what comes after -a or --user can't start with -, as it's more likely the next option than a password or a user. What came isn't
    // shown, as it may be the password
    private def notAnOption(what: String)(value: String): ValidatedNel[String, String] =
      if (value.startsWith("-")) s"please supply $what, which can't start with -".invalidNel else value.validNel

    private def key(what: String)(value: String): ValidatedNel[String, Key] =
      Key.from(value).leftMap(_ => s"please supply $what").toValidatedNel

    private val host = Opts.argument[String]("host").mapValidated(h => Host.from(h).leftMap(_ => s"$h isn't a host").toValidatedNel)
    private val port =
      Opts.argument[Int]("port").mapValidated(p => Port.from(p).leftMap(_ => s"$p isn't a port from 1 to 65535").toValidatedNel)

    // -a is used instead of REDISCLI_AUTH, which can hold a password starting with -
    private val password: Opts[Option[Key]] = (
      Opts.option[String]("password", "The password to log in with.", short = "a", metavar = "password").orNone,
      Opts.env[String]("REDISCLI_AUTH", "The password to log in with, if -a isn't given.", metavar = "password").orNone
    ).tupled.mapValidated {
      case (Some(typed), _)  => notAnOption("a password after -a")(typed).andThen(key("a password after -a")).map(Some(_))
      case (None, Some(env)) => key("a password in REDISCLI_AUTH")(env).map(Some(_))
      case (None, None)      => None.validNel
    }

    private val user = Opts
      .option[String]("user", "The user to log in as, with the password of -a or REDISCLI_AUTH.", metavar = "user")
      .mapValidated(notAnOption("a user after --user")(_).andThen(key("a user after --user")))
      .orNone

    private val database = Opts
      .option[Int]("database", "The database to use, from 0. It's 0 if not given.", short = "n", metavar = "number")
      .mapValidated(n => DbIndex.from(n).leftMap(_ => s"$n isn't a database number from 0").toValidatedNel)
      .orNone

    private val command =
      Command("laserdisc-cli", "An interactive prompt for sending commands to a Redis, Valkey, KeyDB or Dragonfly server.")(
        (host, port, password, user, database).tupled.mapValidated {
          case (_, _, None, Some(_), _)      => "please supply the password with -a or REDISCLI_AUTH as well as --user".invalidNel
          case (h, p, login, name, database) => Options(h, p, login.map(Credentials(name, _)), database).validNel
        }
      )

    // the host and the port, then -a for the password, or else REDISCLI_AUTH in env, --user for the user, which needs a password too, and
    // -n for the database. --help, or a mistake, gives what to type
    def options(args: List[String], env: Map[String, String] = Map.empty): Either[Help, Options] = command.parse(args, env)

    def session(
        args: List[String],
        env: Map[String, String],
        in: Stream[IO, Byte],
        out: Pipe[IO, Byte, Nothing],
        timeout: FiniteDuration
    ): IO[ExitCode] = {
      def write(msg: String): IO[Unit] = Stream.emit(msg).through(text.utf8.encode).through(out).compile.drain

      options(args, env) match {
        case Right(chosen) => write(s"$logo$LF") >> mkStream(chosen, in, out, timeout).compile.lastOrError
        case Left(help)    => write(s"$help$LF").as(if (help.errors.isEmpty) ExitCode.Success else ExitCode.Error)
      }
    }

    // one command at a time, on a client of its own, so that a command typed with a wait of 0 can wait for as long as it takes
    def mkStream(options: Options, in: Stream[IO, Byte], out: Pipe[IO, Byte, Nothing], timeout: FiniteDuration): Stream[IO, ExitCode] =
      Stream
        .resource(
          RedisClient[IO].dedicated(options.host, options.port, credentials = options.credentials, database = options.database)
        )
        .evalMap { redisClient =>
          val database                         = options.database.filter(_.value != 0).fold("")(db => s"[${db.value}]")
          val promptStream: Stream[IO, String] = Stream.emit(s"${options.host}:${options.port}$database> ").repeat

          val emptyPrompt: IO[Unit] =
            promptStream.head
              .through(text.utf8.encode)
              .through(out)
              .compile
              .drain

          def prompt(msg: String): IO[Unit] =
            Stream
              .emit(msg)
              .append(promptStream.head)
              .through(text.utf8.encode)
              .through(out)
              .compile
              .drain

          val protocol: Pipe[IO, String, Protocol.Single[RESP]] =
            _.map(CommandLine.parse).flatMap {
              case Left(error)            => Stream.exec(prompt(s"(error) $error$LF"))
              case Right(Nil)             => Stream.exec(emptyPrompt)
              case Right(command :: args) =>
                val name = new String(command.toArray, UTF_8)
                refusal(name, args.map(arg => new String(arg.toArray, UTF_8))) match {
                  case Some(reason) => Stream.exec(prompt(s"(error) $reason$LF"))
                  case None         => Stream.emit(Protocol(name, args.map(Bulk(_))).using(RESPRead.raw))
                }
            }

          emptyPrompt >>
            in
              .through(text.utf8.decode)
              .through(text.lines)
              .through(protocol)
              .evalMap { protocol =>
                // a send that fails, as when it times out, shows the error like a reply would, and the prompt carries on
                val sending = Forever.from(protocol).fold(redisClient.send(protocol, timeout))(redisClient.send(_)).attempt.map(_.flatten)
                for {
                  startTime             <- IO(System.nanoTime())
                  maybeProtocolResponse <- sending
                  endTime               <- IO(System.nanoTime())
                } yield maybeProtocolResponse -> (endTime - startTime)
              }
              .evalMap {
                // the server refusing the password or the database would refuse every command, so the prompt stops
                case (Left(refused: ConnectionSetupRefused), _) =>
                  Stream
                    .emit(s"ERROR ${messageOf(refused)}$LF")
                    .through(text.utf8.encode)
                    .through(out)
                    .compile
                    .drain
                    .as(true)
                case (reply, nanos) =>
                  val ms    = nanos.toDouble / 1000000d
                  val shown = reply match {
                    case Left(err: Err) => RESPFormat.format(err)
                    case Left(e)        => s"ERROR ${messageOf(e)}"
                    case Right(resp)    => RESPFormat.format(resp)
                  }
                  prompt(f"<<< $shown - [$ms%.2fms]$LF").as(false)
              }
              .exists(identity)
              .compile
              .lastOrError
              .map(refused => if (refused) ExitCode.Error else ExitCode.Success)
        }
  }
}
