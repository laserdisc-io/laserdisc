package laserdisc
package cli

import java.util.concurrent.Executors._

import _root_.fs2.{io, text}
import cats.effect._
import cats.syntax.all._
import laserdisc.fs2._
import log.effect.LogWriter
import log.effect.fs2.SyncLogWriter

import scala.concurrent.ExecutionContext.fromExecutorService
import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

object CLI extends IOApp { self =>

  private[this] final val logo =
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

  override final def run(args: List[String]): IO[ExitCode] = args match {
    case arg1 :: arg2 :: Nil =>
      val maybeHost = Host.from(arg1).toOption
      val maybePort = Either.catchNonFatal(arg2.toInt).flatMap(Port.from).toOption

      (maybeHost, maybePort) match {
        case (Some(ip), Some(port)) =>
          IO(println(logo)) >> impl.mkStream(ip, port)(SyncLogWriter.noOpLog[IO]).compile.drain.as(ExitCode.Success)
        case _ => IO(println("please supply valid host and port (space separated)")).as(ExitCode.Error)
      }

    case _ => IO(println("please supply host and port (space separated)")).as(ExitCode.Error)
  }

  private[cli] final object impl {
    private[this] val tb = universe.runtimeMirror(self.getClass.getClassLoader).mkToolBox()

    def mkStream(host: Host, port: Port)(implicit `_`: LogWriter[IO]): Stream[IO, ExitCode] =
      Stream.resource(Blocker.fromExecutorService[IO](IO(fromExecutorService(newSingleThreadExecutor)))) >>= { replBlockingPool =>
        Stream.resource(RedisClient.toNode[IO](host, port)) evalMap { redisClient =>
          val promptStream: Stream[IO, String] = Stream.emit(s"$host:$port> ").repeat

          val emptyPrompt: IO[Unit] =
            promptStream.head
              .through(text.utf8Encode)
              .through(io.stdout(replBlockingPool))
              .compile
              .drain

          def prompt(msg: String): IO[Unit] =
            Stream
              .emit(msg)
              .append(promptStream.head)
              .through(text.utf8Encode)
              .through(io.stdout(replBlockingPool))
              .compile
              .drain

          final object ToMillis {
            def unapply(nanos: Long): Option[Double] = Some(nanos.toDouble / 1000000d)
          }

          val protocol: Pipe[IO, String, Protocol] = {
            def mkProtocol(code: String): IO[Maybe[Protocol]] =
              IO.delay {
                tb.eval {
                    tb.parse {
                      s"""import laserdisc._
                        |import laserdisc.auto._
                        |import laserdisc.all._
                        |import laserdisc.fs2._
                        |import shapeless._
                        |
                        |$code
                        |""".stripMargin
                    }
                  }
                  .asInstanceOf[Protocol]
              }.attempt

            _.evalMap(mkProtocol).flatMap {
              case Left(t)  => Stream.eval_(prompt(s"${t.getLocalizedMessage}$LF"))
              case Right(p) => Stream.emit(p)
            }
          }

          emptyPrompt >>
            io.stdin[IO](bufSize = 10 * 1024, replBlockingPool)
              .through(text.utf8Decode)
              .through(text.lines)
              .through(protocol)
              .evalMap { protocol =>
                for {
                  startTime             <- IO(System.nanoTime())
                  maybeProtocolResponse <- redisClient.send(protocol.asInstanceOf[Protocol.Aux[protocol.A]])
                  endTime               <- IO(System.nanoTime())
                } yield maybeProtocolResponse.asInstanceOf[Maybe[Any]] -> (endTime - startTime)
              }
              .evalMap {
                case (Left(t), ToMillis(ms))         => prompt(f"<<< ERROR ${t.getLocalizedMessage} - [$ms%.2fms]$LF")
                case (Right(response), ToMillis(ms)) => prompt(f"<<< $response - [$ms%.2fms]$LF")
              }
              .compile
              .drain
              .as(ExitCode.Success)
        }
      }
  }
}
