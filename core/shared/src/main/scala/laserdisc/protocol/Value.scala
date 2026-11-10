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

/** A value to send, for commands where you also choose the type of the reply: anything with a [[Show]] converts to it, and a [[Bulk]] is
  * sent as its bytes are
  */
final class Value(val bulk: Bulk) extends AnyVal

object Value extends ValueWithoutShow {
  implicit final def fromShow[A](a: A)(implicit A: Show[A]): Value = new Value(a match {
    case bulk: Bulk => bulk
    case _          => Bulk(A.show(a))
  })

  // a value inside a list of parameters, sent as its bytes are when it's a Bulk
  private[protocol] final def bulk[A: Show](a: A): Bulk = fromShow(a).bulk
}
