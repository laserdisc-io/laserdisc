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

import Protocol.{Multi, Single}

trait TransactionBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "MULTI",
      "EXEC",
      "UNWATCH",
      "WATCH"
    )

  /** Runs `protocol` as a transaction (see [[Protocol.multi]]): the result is `None` when nothing ran because a watched key changed */
  final def multi[A](protocol: Single.Aux[A]): Multi.Aux[Option[Maybe[A]]] =
    Protocol.multi(protocol *: EmptyTuple).map(_.map(_.head))

  final val unwatch: Single.Aux[OK] = Protocol("UNWATCH", EmptyTuple).as[Str, OK]

  final def watch(keys: OneOrMoreKeys): Single.Aux[OK] = Protocol("WATCH", keys.value).as[Str, OK]
}

trait TransactionP extends TransactionBaseP with TransactionExtP
