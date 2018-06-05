package laserdisc
package fs2

import java.nio.channels.AsynchronousChannelGroup
import java.nio.channels.AsynchronousChannelGroup.withThreadPool
import java.util.concurrent.Executors.{newFixedThreadPool, newScheduledThreadPool}

import cats.effect._
import cats.syntax.all._
import _root_.fs2._
import _root_.fs2.Scheduler.fromScheduledExecutorService
import _root_.fs2.StreamApp.ExitCode

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

object CLI extends StreamApp[IO] {

  implicit val scheduler: Scheduler                        = fromScheduledExecutorService(newScheduledThreadPool(4))
  implicit val asyncChannelGroup: AsynchronousChannelGroup = withThreadPool(newFixedThreadPool(8))
  implicit val logger: Logger[IO] = new Logger[IO] {
    final def log(level: Logger.Level, msg: => String, maybeT: Logger.MaybeT): IO[Unit] = IO {
      println(f"[${level.value}%-5s]: $msg")
      maybeT.foreach(_.printStackTrace())
    }
  }

  override final def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
    val maybeHost = args.headOption.flatMap(Host.from(_).toOption)
    val maybePort = args.tail.headOption.flatMap(s => Try(s.toInt).toEither.flatMap(Port.from).toOption)
    (maybeHost, maybePort) match {
      case (Some(ip), Some(port)) => impl.mkStream[IO](ip, port)
      case _                      => Stream.emit(ExitCode.Error)
    }
  }

  private[fs2] final object impl {
    import scala.reflect.runtime.universe
    import scala.tools.reflect.ToolBox

    private val tb = universe.runtimeMirror(CLI.getClass.getClassLoader).mkToolBox()

    def mkStream[F[_]: Logger](host: Host, port: Port)(implicit F: Effect[F]): Stream[F, ExitCode] = {
      val promptStream: Stream[F, String] = Stream.emit(s"$host:$port> ").repeat

      val emptyPrompt: F[Unit] = promptStream.head.through(text.utf8Encode).to(io.stdout).compile.drain

      def prompt(msg: String): F[Unit] =
        Stream
          .emit(msg)
          .append(promptStream.head)
          .through(text.utf8Encode)
          .to(io.stdout)
          .compile
          .drain

      final object ToMillis {
        def unapply(nanos: Long): Option[Double] = Some(nanos.toDouble / 1000000d)
      }

      val toProtocol: Pipe[F, String, Protocol] = {
        def mkProtocol(code: String): F[Maybe[Protocol]] =
          F.delay {
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

      RedisClient(Set(RedisAddress(host, port))).evalMap { redisClient =>
        emptyPrompt.flatMap { _ =>
          io.stdin(10 * 1024)
            .through(text.utf8Decode)
            .through(text.lines)
            .through(toProtocol)
            .evalMap { protocol =>
              for {
                startTime             <- F.delay(System.nanoTime())
                maybeProtocolResponse <- redisClient.send1(protocol.asInstanceOf[Protocol.Aux[protocol.A]])
                endTime               <- F.delay(System.nanoTime())
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
