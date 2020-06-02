import java.io.{ByteArrayOutputStream, PrintStream}

import cats.effect.IO

trait TestLogCapture {

  final def capturedConsoleOutOf(aWrite: IO[Unit]): String = {
    val lowerStream = new ByteArrayOutputStream()
    val outStream   = new PrintStream(lowerStream)

    Console.withOut(outStream)(aWrite.unsafeRunSync)

    lowerStream.toString
  }
}
