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
import _root_.fs2.{io, text}
import cats.effect.*
import cats.syntax.all.*
import laserdisc.fs2.*
import laserdisc.protocol.RESPRead

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

  override final def run(args: List[String]): IO[ExitCode] = impl.session(args, io.stdin[IO](bufSize), io.stdout[IO])

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
      else if (is("QUIT")) refused(command, "the server would close the connection. To leave, press Ctrl-D")
      else if (is("AUTH")) refused(command, "a new connection would forget it. Start the prompt with -a, and --user if there's one")
      else if (is("SELECT")) refused(command, "a new connection would forget it. Start the prompt with -n")
      else None
    }

    // what the prompt connects to and how: each connection logs in and selects the database first, so a new one does too
    final case class Options(host: Host, port: Port, credentials: Option[Credentials], database: Option[DbIndex])

    // the host and the port, then -a for the password, --user for the user, which needs -a too, and -n for the database
    def options(args: List[String]): Either[String, Options] = {
      def withOptions(rest: List[String], options: Options, user: Option[Key]): Either[String, Options] = rest match {
        case "-a" :: password :: more =>
          Key.from(password).leftMap(_ => "please supply a password after -a").flatMap { key =>
            withOptions(more, options.copy(credentials = Some(Credentials(None, key))), user)
          }
        case "--user" :: name :: more =>
          Key.from(name).leftMap(_ => "please supply a user after --user").flatMap(key => withOptions(more, options, Some(key)))
        case "-n" :: index :: more =>
          Either
            .catchNonFatal(index.toInt)
            .leftMap(_.getMessage)
            .flatMap(DbIndex.from)
            .leftMap(_ => "please supply a database number from 0 after -n")
            .flatMap(db => withOptions(more, options.copy(database = Some(db)), user))
        case "-a" :: Nil     => Left("please supply a password after -a")
        case "--user" :: Nil => Left("please supply a user after --user")
        case "-n" :: Nil     => Left("please supply a database number from 0 after -n")
        case Nil             =>
          (user, options.credentials) match {
            case (Some(_), None)           => Left("please supply the password with -a as well as --user")
            case (Some(name), Some(login)) => Right(options.copy(credentials = Some(login.copy(username = Some(name)))))
            case (None, _)                 => Right(options)
          }
        case other :: _ => Left(s"$other isn't an option: use -a <password>, --user <user> and -n <database>")
      }

      args match {
        case host :: port :: rest =>
          (Host.from(host).toOption, Either.catchNonFatal(port.toInt).flatMap(Port.from).toOption) match {
            case (Some(h), Some(p)) => withOptions(rest, Options(h, p, None, None), None)
            case _                  => Left("please supply valid host and port (space separated)")
          }
        case _ => Left("please supply host and port (space separated)")
      }
    }

    def session(args: List[String], in: Stream[IO, Byte], out: Pipe[IO, Byte, Nothing]): IO[ExitCode] = {
      def write(msg: String): IO[Unit] = Stream.emit(msg).through(text.utf8.encode).through(out).compile.drain

      options(args) match {
        case Right(chosen) => write(s"$logo$LF") >> mkStream(chosen, in, out).compile.drain.as(ExitCode.Success)
        case Left(error)   => write(s"$error$LF").as(ExitCode.Error)
      }
    }

    def mkStream(options: Options, in: Stream[IO, Byte], out: Pipe[IO, Byte, Nothing]): Stream[IO, ExitCode] =
      Stream
        .resource(
          RedisClient[IO].to(options.host, options.port, credentials = options.credentials, database = options.database)
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

          val protocol: Pipe[IO, String, Protocol[RESP]] =
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
                for {
                  startTime             <- IO(System.nanoTime())
                  maybeProtocolResponse <- redisClient.send(protocol)
                  endTime               <- IO(System.nanoTime())
                } yield maybeProtocolResponse -> (endTime - startTime)
              }
              .evalMap { case (reply, nanos) =>
                val ms    = nanos.toDouble / 1000000d
                val shown = reply match {
                  case Left(err: Err) => RESPFormat.format(err)
                  case Left(e)        => s"ERROR ${e.getLocalizedMessage}"
                  case Right(resp)    => RESPFormat.format(resp)
                }
                prompt(f"<<< $shown - [$ms%.2fms]$LF")
              }
              .compile
              .drain
              .as(ExitCode.Success)
        }
  }
}
