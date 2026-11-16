/*
 * Copyright (c) 2018-2026 LaserDisc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package laserdisc
package fs2

import cats.effect.IO
import laserdisc.auto.*
import laserdisc.protocol.RESPRead
import munit.CatsEffectSuite

import java.util.Locale
import scala.concurrent.duration.*

// on the JVM, upper-casing follows the default locale, which turns a typed `multi` into `MULTİ` in Turkish
final class TurkishLocaleSpec extends CatsEffectSuite {
  private def typed(words: String*): Protocol[RESP] = Protocol(words.head, words.tail.toList).using(RESPRead.raw)

  test("a transaction typed in lower case on a machine whose locale is Turkish is still known, so what follows a lost one isn't sent") {
    // the first connection answers multi and closes, the others answer everything
    val server = FakeRedisServer((n, requests) => if (n == 1) requests.take(1).as("+OK\r\n") else requests.as("+OK\r\n"))
    IO(Locale.getDefault).flatMap { before =>
      (IO(Locale.setDefault(Locale.forLanguageTag("tr"))) >> server.use { port =>
        RedisClient[IO].to("127.0.0.1", port).use { client =>
          for {
            multi <- client.send(typed("multi"))
            set   <- IO.sleep(300.millis) >> client.send(typed("set", "a", "1"), 5.seconds)
          } yield {
            assertEquals(multi, Right(Str("OK")))
            assert(set.left.exists(_.isInstanceOf[TransactionLost]), set)
          }
        }
      }).guarantee(IO(Locale.setDefault(before)))
    }
  }
}
