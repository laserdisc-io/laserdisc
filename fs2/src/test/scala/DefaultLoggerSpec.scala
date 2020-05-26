import java.util.concurrent.ForkJoinPool

import cats.effect.{ContextShift, IO, Timer}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor

final class DefaultLoggerSpec extends AnyWordSpecLike with Matchers with ConsoleUtil {

  private[this] val ec: ExecutionContext = fromExecutor(new ForkJoinPool())

  private[this] implicit val timer: Timer[IO]               = IO.timer(ec)
  private[this] implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)

  "The readme example should not log when a LogWriter is not given" in {
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

    logged shouldNot include("Starting connection")
    logged shouldNot include("Server available for publishing: localhost:6379")
    logged shouldNot include("sending Arr(Bulk(SET),Bulk(a),Bulk(23))")
    logged shouldNot include("receiving Str(OK)")
    logged shouldNot include("sending Arr(Bulk(SET),Bulk(b),Bulk(55))")
    logged shouldNot include("receiving Str(OK)")
    logged shouldNot include("sending Arr(Bulk(GET),Bulk(b))")
    logged shouldNot include("receiving Bulk(55)")
    logged shouldNot include("sending Arr(Bulk(GET),Bulk(a))")
    logged shouldNot include("receiving Bulk(23)")
    logged shouldNot include("Shutting down connection")
    logged shouldNot include("Shutdown complete")
    logged shouldNot include("Connection terminated: No issues")
  }
}
