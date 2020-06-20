import java.util.concurrent.ForkJoinPool

import cats.effect.{ContextShift, IO, Timer}
import munit.FunSuite

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor

final class ReadmeExampleSpec extends FunSuite with TestLogCapture {

  private[this] val ec: ExecutionContext = fromExecutor(new ForkJoinPool())

  private[this] implicit val timer: Timer[IO]               = IO.timer(ec)
  private[this] implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)

  test("The readme example gives the expected output and logs when a LogWriter is in scope") {
    import cats.syntax.flatMap._
    import laserdisc._
    import laserdisc.all._
    import laserdisc.auto._
    import laserdisc.fs2._
    import log.effect.LogWriter
    import log.effect.fs2.SyncLogWriter

    def redisTest(implicit log: LogWriter[IO]): IO[Unit] =
      RedisClient.to("localhost", 6379).use { client =>
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

    assert(logged contains "Starting connection")
    assert(logged contains "Connected to server localhost:6379")
    assert(logged contains "sending Arr(Bulk(SET),Bulk(a),Bulk(23))")
    assert(logged contains "receiving Str(OK)")
    assert(logged contains "sending Arr(Bulk(SET),Bulk(b),Bulk(55))")
    assert(logged contains "receiving Str(OK)")
    assert(logged contains "sending Arr(Bulk(GET),Bulk(b))")
    assert(logged contains "receiving Bulk(55)")
    assert(logged contains "sending Arr(Bulk(GET),Bulk(a))")
    assert(logged contains "receiving Bulk(23)")
    assert(logged contains "yay!")
    assert(logged contains "Shutting down connection")
    assert(logged contains "Shutdown complete")
    assert(logged contains "Connection terminated: No issues")
  }
}
