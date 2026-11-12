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

/** A blocking command that waits on the server for as long as it takes. Only a dedicated client sends it: on a shared one it would hold the
  * connection, and every command sent after it, until it returns
  */
final class Forever[+A] private (private[laserdisc] val protocol: Protocol.Single[A]) {
  override def toString: String = s"Forever(${protocol.command})"
}

object Forever {
  // from the same command with a wait, which only stands in for waiting forever
  private[protocol] def apply[A](timed: Protocol.Single[A]): Forever[A] = new Forever(Blocking.forever(timed))

  // a command already sent with a wait of 0, as typed at the CLI, if it is one
  private[laserdisc] def from[A](protocol: Protocol.Single[A]): Option[Forever[A]] =
    if (Blocking.waitsForever(protocol)) Some(new Forever(protocol)) else None
}
