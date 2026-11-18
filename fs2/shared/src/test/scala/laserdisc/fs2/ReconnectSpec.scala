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

import munit.FunSuite

import scala.concurrent.duration.*

final class ReconnectSpec extends FunSuite {
  test("the wait doubles each time in a row no server could be reached, up to max") {
    val reconnect = Reconnect(100.millis, 1.second)
    assertEquals((0 to 5).toList.map(reconnect.after), List(100.millis, 200.millis, 400.millis, 800.millis, 1.second, 1.second))
    assertEquals(reconnect.after(1000), 1.second)
  }

  test("the default starts at 100 milliseconds and goes up to 5 seconds") {
    assertEquals(Reconnect.default, Reconnect(100.millis, 5.seconds))
  }

  test("the first wait must be more than 0, and max at least the first") {
    intercept[IllegalArgumentException](Reconnect(Duration.Zero, 1.second))
    intercept[IllegalArgumentException](Reconnect(2.seconds, 1.second))
    assertEquals(Reconnect(1.second, 1.second).after(3), 1.second)
  }
}
