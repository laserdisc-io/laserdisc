/*
 * Copyright (c) 2018-2025 LaserDisc
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

sealed trait Request[F[_]] {
  type A
  def protocol: Protocol.Aux[A]
  def callback: Maybe[A] => F[Unit]
}

object Request {
  sealed abstract case class Req[F[_], A0](protocol: Protocol.Aux[A0], callback: Maybe[A0] => F[Unit]) extends Request[F] {
    override type A = A0
  }

  def apply[F[_], A](protocol: Protocol.Aux[A], callback: Maybe[A] => F[Unit]): Request[F] =
    new Req[F, A](protocol, callback) {}

  def unapply[F[_]](req: Request[F]): Option[(Protocol.Aux[req.A], Maybe[req.A] => F[Unit])] =
    Some(req.protocol -> req.callback)
}
