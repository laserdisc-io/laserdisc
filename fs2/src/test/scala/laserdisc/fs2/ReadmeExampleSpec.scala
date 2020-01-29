package laserdisc.fs2

import java.io.{ByteArrayOutputStream, PrintStream}
import java.util.concurrent.ForkJoinPool

import cats.effect.{Concurrent, ContextShift, IO, Timer}
import org.scalatest.{Matchers, WordSpecLike}

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor

final class ReadmeExampleSpec extends WordSpecLike with Matchers {
  private[this] val ec: ExecutionContext = fromExecutor(new ForkJoinPool())

  private[this] implicit val timer: Timer[IO]               = IO.timer(ec)
  private[this] implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)
  private[this] implicit val concurrent: Concurrent[IO]     = IO.ioConcurrentEffect

  private[this] def capturedConsoleOutOf(aWrite: IO[Unit]): String = {
    val lowerStream = new ByteArrayOutputStream()
    val outStream   = new PrintStream(lowerStream)

    Console.withOut(outStream)(aWrite.unsafeRunSync)

    lowerStream.toString
  }

  "The readme example should give the expected output" in {
    import cats.syntax.flatMap._
    import laserdisc._
    import laserdisc.all._
    import laserdisc.auto._
    import log.effect.LogWriter
    import log.effect.fs2.SyncLogWriter

    def redisTest(implicit log: LogWriter[IO]): IO[Unit] =
      RedisClient.toNode[IO]("localhost", 6379).use { client =>
        client.send(
          set("a", 23),
          set("b", 55),
          get[PosInt]("b"),
          get[PosInt]("a")
        ) >>= {
          case (Right(OK), Right(OK), Right(Some(getOfb)), Right(Some(getOfa))) if getOfb.value == 55 && getOfa.value == 23 =>
            log info "yay!"
          case other =>
            log.error(s"something went terribly wrong $other") >>
              IO.raiseError(new RuntimeException("boom"))
        }
      }

    val logged = capturedConsoleOutOf {
      redisTest(SyncLogWriter.consoleLog[IO])
    }

    logged should include("Starting connection")
    logged should include("Server available for publishing: localhost:6379")
    logged should include("sending Arr(Bulk(SET),Bulk(a),Bulk(23))")
    logged should include("receiving Str(OK)")
    logged should include("sending Arr(Bulk(SET),Bulk(b),Bulk(55))")
    logged should include("receiving Str(OK)")
    logged should include("sending Arr(Bulk(GET),Bulk(b))")
    logged should include("receiving Bulk(55)")
    logged should include("sending Arr(Bulk(GET),Bulk(a))")
    logged should include("receiving Bulk(23)")
    logged should include("yay!")
    logged should include("Shutting down connection")
    logged should include("Shutdown complete")
    logged should include("Connection terminated: No issues")
  }
}
