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
package usage

import munit.FunSuite

// what code outside of laserdisc.fs2 sees of it
final class Fs2InternalsSpec extends FunSuite {
  test("importing fs2 and laserdisc.fs2 together leaves fs2's and cats-effect's names as they are") {
    assert(
      compileErrors(
        """{
          import _root_.fs2.*
          import cats.effect.std.Queue
          import laserdisc.fs2.*
          val stream: Stream[cats.effect.IO, Int]       = Stream.emit(1)
          val pipe: Pipe[cats.effect.IO, Int, Int]      = _.map(_ + 1)
          val queue: cats.effect.IO[Queue[cats.effect.IO, Int]] = Queue.unbounded[cats.effect.IO, Int]
        }"""
      ).isEmpty
    )
  }

  test("requests and batches can't be built outside of laserdisc.fs2") {
    assert(
      compileErrors(
        "laserdisc.fs2.Request[cats.effect.IO, String](protocol.Protocol(\"PING\", Nil).using(protocol.RESPRead.raw), _ => cats.effect.IO.unit)"
      ).nonEmpty
    )
    assert(compileErrors("laserdisc.fs2.Batch[cats.effect.IO](Vector.empty, null, scala.concurrent.duration.Duration.Zero)").nonEmpty)
  }
}
