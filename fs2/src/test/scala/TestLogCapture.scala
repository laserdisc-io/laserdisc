import cats.effect.IO
import cats.effect.unsafe.IORuntime.global
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}

import java.io.{ByteArrayOutputStream, PrintStream}
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext.fromExecutor

trait TestLogCapture {
  final def capturedConsoleOutOf(aWrite: IO[Unit]): String = {
    val runtime = IORuntime(
      compute = fromExecutor(Executors.newFixedThreadPool(2)),
      blocking = fromExecutor(Executors.newCachedThreadPool()),
      scheduler = global.scheduler,
      shutdown = global.shutdown,
      config = IORuntimeConfig()
    )
    val lowerStream = new ByteArrayOutputStream()
    val outStream   = new PrintStream(lowerStream)

    Console.withOut(outStream)(aWrite.unsafeRunSync()(runtime))

    lowerStream.toString
  }
}
