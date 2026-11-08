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
    def session(args: List[String], in: Stream[IO, Byte], out: Pipe[IO, Byte, Nothing]): IO[ExitCode] = {
      def write(msg: String): IO[Unit] = Stream.emit(msg).through(text.utf8.encode).through(out).compile.drain

      args match {
        case arg1 :: arg2 :: Nil =>
          val maybeHost = Host.from(arg1).toOption
          val maybePort = Either.catchNonFatal(arg2.toInt).flatMap(Port.from).toOption

          (maybeHost, maybePort) match {
            case (Some(ip), Some(port)) =>
              write(s"$logo$LF") >> mkStream(ip, port, in, out).compile.drain.as(ExitCode.Success)
            case _ => write(s"please supply valid host and port (space separated)$LF").as(ExitCode.Error)
          }

        case _ => write(s"please supply host and port (space separated)$LF").as(ExitCode.Error)
      }
    }

    def mkStream(host: Host, port: Port, in: Stream[IO, Byte], out: Pipe[IO, Byte, Nothing]): Stream[IO, ExitCode] =
      Stream.resource(RedisClient[IO].to(host, port)).evalMap { redisClient =>
        val promptStream: Stream[IO, String] = Stream.emit(s"$host:$port> ").repeat

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
            case Right(command :: args) => Stream.emit(Protocol(command, args).using(RESPRead.raw))
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
