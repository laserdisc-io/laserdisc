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
package protocol

import java.util.Locale
import scala.concurrent.duration.*

// on the JVM, upper-casing follows the default locale, which turns a typed `wait` into `WAİT` in Turkish
final class TurkishLocaleSpec extends munit.FunSuite {
  private def typed(words: String*): Protocol.Single[RESP] = Protocol(words.head, words.tail.toList).using(RESPRead.raw)

  test("a command with an i in its name, typed in lower case, gets its wait on a machine whose locale is Turkish") {
    val before = Locale.getDefault
    Locale.setDefault(Locale.forLanguageTag("tr"))
    try {
      assertEquals(Blocking.waitOf(typed("wait", "1", "500")), 500.millis)
      assertEquals(Blocking.waitOf(typed("waitaof", "0", "1", "500")), 500.millis)
    } finally Locale.setDefault(before)
  }
}
