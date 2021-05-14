import java.util.concurrent.ForkJoinPool

import cats.effect.IO
import munit.FunSuite

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor
import cats.effect.Temporal

final class DefaultLoggerSpec extends FunSuite with TestLogCapture {

  private def assertNot(c: =>Boolean): Unit = assert(!c)

  private[this] val ec: ExecutionContext = fromExecutor(new ForkJoinPool())

  private[this] implicit val timer: Temporal[IO]               = IO.timer(ec)
  private[this] implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)

  test("The readme example doesn't log when no LogWriter is given") {
    import cats.syntax.flatMap._
    import laserdisc._
    import laserdisc.all._
    import laserdisc.auto._
    import laserdisc.fs2._
    import log.effect.fs2.SyncLogWriter.consoleLog

    val redisTest: IO[Unit] =
      RedisClient.to("localhost", 6379).use { client =>
        client.send(
          set("a", 23),
          set("b", 55),
          get[PosInt]("b"),
          get[PosInt]("a")
        ) >>= {
          case (Right(OK), Right(OK), Right(Some(getOfb)), Right(Some(getOfa))) if getOfb.value == 55 && getOfa.value == 23 =>
            consoleLog[IO].info("yay!")
          case other =>
            consoleLog[IO].error(s"something went terribly wrong $other") >>
              IO.raiseError(new RuntimeException("boom"))
        }
      }

    val logged = capturedConsoleOutOf(redisTest)

    assertNot(logged contains "Starting connection")
    assertNot(logged contains "Server available for publishing: localhost:6379")
    assertNot(logged contains "sending Arr(Bulk(SET),Bulk(a),Bulk(23))")
    assertNot(logged contains "receiving Str(OK)")
    assertNot(logged contains "sending Arr(Bulk(SET),Bulk(b),Bulk(55))")
    assertNot(logged contains "receiving Str(OK)")
    assertNot(logged contains "sending Arr(Bulk(GET),Bulk(b))")
    assertNot(logged contains "receiving Bulk(55)")
    assertNot(logged contains "sending Arr(Bulk(GET),Bulk(a))")
    assertNot(logged contains "receiving Bulk(23)")
    assertNot(logged contains "Shutting down connection")
    assertNot(logged contains "Shutdown complete")
    assertNot(logged contains "Connection terminated: No issues")
  }
}
