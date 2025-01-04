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

trait HyperLogLogBaseP {
  final def pfadd(key: Key, elements: OneOrMoreKeys): Protocol.Aux[Boolean] = Protocol("PFADD", key :: elements.value).as[Num, Boolean]

  final def pfcount(keys: OneOrMoreKeys): Protocol.Aux[NonNegInt] = Protocol("PFCOUNT", keys.value).as[Num, NonNegInt]

  final def pfmerge(sourceKeys: TwoOrMoreKeys, destinationKey: Key): Protocol.Aux[OK] =
    Protocol("PFMERGE", destinationKey :: sourceKeys.value).as[Str, OK]
}

trait HyperLogLogP extends HyperLogLogBaseP with HyperLogLogExtP
