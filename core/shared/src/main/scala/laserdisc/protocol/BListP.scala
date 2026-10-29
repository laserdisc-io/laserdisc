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

trait BListBaseP {
  final def blpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, seconds: NonNegInt): Protocol.Aux[Option[KV[A]]] =
    Protocol("BLPOP", keys.value *: seconds *: EmptyTuple).asOpt[Arr, KV[A]]

  final def brpop[A: Read[Bulk, _]](keys: OneOrMoreKeys, seconds: NonNegInt): Protocol.Aux[Option[KV[A]]] =
    Protocol("BRPOP", keys.value *: seconds *: EmptyTuple).asOpt[Arr, KV[A]]

  final def brpoplpush[A: Read[Bulk, _]](source: Key, destination: Key): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source *: destination *: 0 *: EmptyTuple).asOpt[Bulk, A]
  final def brpoplpush[A: Read[Bulk, _]](source: Key, destination: Key, timeout: PosInt): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source *: destination *: timeout *: EmptyTuple).asOpt[Bulk, A]
}

trait BListP extends BListBaseP with BListExtP
