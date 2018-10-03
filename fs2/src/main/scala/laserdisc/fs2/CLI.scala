package laserdisc
package fs2

import java.nio.channels.AsynchronousChannelGroup.withThreadPool
import java.util.concurrent.Executors._

import _root_.fs2.{io, text}
import cats.effect._
import cats.syntax.all._
import log.effect.fs2.Fs2LogWriter

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutorService
import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

object CLI extends IOApp.WithContext { self =>

  private[this] final val logo =
    """
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

  override final protected val executionContextResource: Resource[SyncIO, ExecutionContext] =
    MkResource(SyncIO(fromExecutorService(newWorkStealingPool()))).widenRight[ExecutionContext]

  override final def run(args: List[String]): IO[ExitCode] = args match {
    case arg1 :: arg2 :: Nil =>
      val maybeHost = Host.from(arg1).toOption
      val maybePort = Either.catchNonFatal(arg2.toInt).flatMap(Port.from).toOption

      (maybeHost, maybePort) match {
        case (Some(ip), Some(port)) => IO(println(logo)) *> impl.mkStream(ip, port).compile.drain.as(ExitCode.Success)
        case _                      => IO(println("please supply valid host and port (space separated)")).as(ExitCode.Error)
      }

    case _ => IO(println("please supply host and port (space separated)")).as(ExitCode.Error)
  }

  private[fs2] final object impl {
    private[this] val tb = universe.runtimeMirror(self.getClass.getClassLoader).mkToolBox()

    def mkStream(host: Host, port: Port): Stream[IO, ExitCode] =
      Stream.resource(MkResource(IO(withThreadPool(newFixedThreadPool(8))))).flatMap { implicit acg =>
        Stream.resource(MkResource(IO(fromExecutorService(newSingleThreadExecutor())))).flatMap { blockingEC =>
          Fs2LogWriter.consoleLogStream[IO].flatMap { implicit log =>
            RedisClient[IO](Set(RedisAddress(host, port))).evalMap { redisClient =>
              val promptStream: Stream[IO, String] = Stream.emit(s"$host:$port> ").repeat

              val emptyPrompt: IO[Unit] =
                promptStream.head.through(text.utf8Encode).to(io.stdout(blockingEC)).compile.drain

              def prompt(msg: String): IO[Unit] =
                Stream
                  .emit(msg)
                  .append(promptStream.head)
                  .through(text.utf8Encode)
                  .to(io.stdout(blockingEC))
                  .compile
                  .drain

              final object ToMillis {
                def unapply(nanos: Long): Option[Double] = Some(nanos.toDouble / 1000000d)
              }

              val toProtocol: Pipe[IO, String, Protocol] = {
                def mkProtocol(code: String): IO[Maybe[Protocol]] =
                  IO.delay {
                    tb.eval {
                        tb.parse {
                          s"""import laserdisc._
                             |import laserdisc.auto._
                             |import laserdisc.all._
                             |import shapeless._
                             |
                             |$code
                             |""".stripMargin
                        }
                      }
                      .asInstanceOf[Protocol]
                  }.attempt

                _.evalMap(mkProtocol).flatMap {
                  case Left(t)         => Stream.eval_(prompt(s"${t.getLocalizedMessage}\n"))
                  case Right(protocol) => Stream.emit(protocol)
                }
              }

              emptyPrompt >>
                io.stdin[IO](bufSize = 10 * 1024, blockingEC)
                  .through(text.utf8Decode)
                  .through(text.lines)
                  .through(toProtocol)
                  .evalMap { protocol =>
                    for {
                      startTime             <- IO(System.nanoTime())
                      maybeProtocolResponse <- redisClient.send1(protocol.asInstanceOf[Protocol.Aux[protocol.A]])
                      endTime               <- IO(System.nanoTime())
                    } yield maybeProtocolResponse.asInstanceOf[Maybe[Any]] -> (endTime - startTime)
                  }
                  .evalMap {
                    case (Left(t), ToMillis(ms))         => prompt(f"<<< ERROR ${t.getLocalizedMessage} - [$ms%.2fms]\n")
                    case (Right(response), ToMillis(ms)) => prompt(f"<<< $response - [$ms%.2fms]\n")
                  }
                  .compile
                  .drain
                  .as(ExitCode.Success)
            }
          }
        }
      }
  }
}
