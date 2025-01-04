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

trait BListBaseP {
  import shapeless._

  final def blpop[A: Bulk ==> *](keys: OneOrMoreKeys, seconds: NonNegInt): Protocol.Aux[Option[KV[A]]] =
    Protocol("BLPOP", keys.value :: seconds :: HNil).opt[GenArr].as[KV[A]]

  final def brpop[A: Bulk ==> *](keys: OneOrMoreKeys, seconds: NonNegInt): Protocol.Aux[Option[KV[A]]] =
    Protocol("BRPOP", keys.value :: seconds :: HNil).opt[GenArr].as[KV[A]]

  final def brpoplpush[A: Bulk ==> *](source: Key, destination: Key): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source :: destination :: 0 :: HNil).opt[GenBulk].as[A]
  final def brpoplpush[A: Bulk ==> *](source: Key, destination: Key, timeout: PosInt): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source :: destination :: timeout :: HNil).opt[GenBulk].as[A]
}

trait BListP extends BListBaseP with BListExtP
