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

import scala.concurrent.duration.{Duration, FiniteDuration}

/** A command sent over and over, each time from what the last reply said, like SCAN with its cursor: each reply gives some values and says
  * whether to go on. It's a description, like a [[Protocol]], which `stream` on a client runs
  */
final class Loop[A] private (private[laserdisc] val first: Protocol[Loop.Step[A]], val waits: FiniteDuration)

object Loop {

  /** What a reply gave, and the command to send next, if any */
  final class Step[A] private[Loop] (val values: Seq[A], val next: Option[Protocol[Step[A]]])

  /** Sends `command(start)`, then, while `step` gives a new state, `command` of that state. `step` reads each reply into values and, unless
    * it was the last, the next state
    */
  def apply[S, R, A](start: S)(command: S => Protocol[R])(step: (S, R) => (Seq[A], Option[S])): Loop[A] =
    blocking(Duration.Zero)(start)(command)(step)

  /** A loop of blocking commands, which each wait up to `waits` on the server: it's added to the timeout of each */
  def blocking[S, R, A](waits: FiniteDuration)(start: S)(command: S => Protocol[R])(step: (S, R) => (Seq[A], Option[S])): Loop[A] = {
    def from(s: S): Protocol[Step[A]] = command(s).map { r =>
      val (values, next) = step(s, r)
      new Step(values, next.map(from))
    }
    new Loop(from(start), waits)
  }

  // SCAN and the like: from cursor 0 until the server replies with cursor 0 again
  private[laserdisc] def cursor[A, R](command: NonNegLong => Protocol[R])(read: R => (NonNegLong, Seq[A])): Loop[A] =
    Loop(NonNegLong.unsafeFrom(0L))(command) { (_, reply) =>
      val (cursor, values) = read(reply)
      values -> (if (cursor.value == 0L) None else Some(cursor))
    }
}
