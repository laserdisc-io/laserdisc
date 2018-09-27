package laserdisc
package fs2

import java.nio.channels.AsynchronousChannelGroup
import java.nio.channels.AsynchronousChannelGroup.withThreadPool
import java.util.concurrent.Executors.newFixedThreadPool
import java.util.concurrent.{Executors, TimeUnit}

import _root_.fs2.{io, text}
import cats.effect._
import cats.syntax.all._
import log.effect.fs2.Fs2LogWriter

import scala.concurrent.ExecutionContext

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
    Resource
      .make(SyncIO(ExecutionContext.fromExecutorService(Executors.newWorkStealingPool()))) { pool =>
        SyncIO {
          pool.shutdown()
          pool.awaitTermination(10, TimeUnit.SECONDS)
          ()
        }
      }
      .asInstanceOf[Resource[SyncIO, ExecutionContext]]

  private[this] final val blockingExecutionContextResource: Resource[IO, ExecutionContext] =
    Resource
      .make(IO(ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor()))) { pool =>
        IO {
          pool.shutdown()
          pool.awaitTermination(10, TimeUnit.SECONDS)
          ()
        }
      }
      .asInstanceOf[Resource[IO, ExecutionContext]]

  private[this] final val asyncChannelGroupResource: Resource[IO, AsynchronousChannelGroup] =
    Resource.make(IO(withThreadPool(newFixedThreadPool(8)))) { acg =>
      IO {
        acg.shutdown()
        acg.awaitTermination(10, TimeUnit.SECONDS)
        ()
      }
    }

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

    import scala.reflect.runtime.universe
    import scala.tools.reflect.ToolBox

    private val tb = universe.runtimeMirror(self.getClass.getClassLoader).mkToolBox()

    def mkStream(host: Host, port: Port): Stream[IO, ExitCode] =
      Stream.resource(asyncChannelGroupResource).flatMap { implicit asyncChannelGroup =>
        Stream.resource(blockingExecutionContextResource).flatMap { blockingExecutionContext =>
          Fs2LogWriter.consoleLogStream[IO].flatMap { implicit log =>
            RedisClient[IO](Set(RedisAddress(host, port))).evalMap { redisClient =>
              val promptStream: Stream[IO, String] = Stream.emit(s"$host:$port> ").repeat

              val emptyPrompt: IO[Unit] =
                promptStream.head.through(text.utf8Encode).to(io.stdout(blockingExecutionContext)).compile.drain

              def prompt(msg: String): IO[Unit] =
                Stream
                  .emit(msg)
                  .append(promptStream.head)
                  .through(text.utf8Encode)
                  .to(io.stdout(blockingExecutionContext))
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

              emptyPrompt.flatMap { _ =>
                io.stdin[IO](10 * 1024, blockingExecutionContext)
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
                  .map(_ => ExitCode.Success)
              }
            }
          }
        }
      }
  }
}
