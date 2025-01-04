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
package protocol

trait ConnectionP {
  private[this] implicit final val pongRead: Str ==> PONG = Read.instance {
    case Str(PONG.`value`) => Right(PONG)
    case Str(other)        => Left(RESPDecErr(s"Wrong PONG encoding: was $other"))
  }

  final def auth(password: Key): Protocol.Aux[OK] = Protocol("AUTH", password).as[Str, OK]

  final def echo[A: Show: Bulk ==> *](message: A): Protocol.Aux[A] = Protocol("ECHO", message).as[Bulk, A]

  final val ping: Protocol.Aux[PONG]                               = Protocol("PING", Nil).as[Str, PONG]
  final def ping[A: Show: Bulk ==> *](message: A): Protocol.Aux[A] = Protocol("PING", message).as[Bulk, A]

  final val quit: Protocol.Aux[OK] = Protocol("QUIT", Nil).as[Str, OK]

  final def select(index: DbIndex): Protocol.Aux[OK] = Protocol("SELECT", index).as[Str, OK]

  final def swapdb(index1: DbIndex, index2: DbIndex): Protocol.Aux[OK] = Protocol("SWAPDB", index1 :: index2 :: Nil).as[Str, OK]
}
