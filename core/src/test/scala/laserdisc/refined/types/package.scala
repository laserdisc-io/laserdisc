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
package refined

import org.scalacheck.Gen.listOf
import org.scalacheck.{Arbitrary, Gen, Prop}

package object types {
  private[types] val ints: Gen[Int]           = implicitly[Arbitrary[Int]].arbitrary
  private[types] val longs: Gen[Long]         = implicitly[Arbitrary[Long]].arbitrary
  private[types] val doubles: Gen[Double]     = implicitly[Arbitrary[Double]].arbitrary
  private[types] val strings: Gen[String]     = implicitly[Arbitrary[String]].arbitrary
  private[types] val intLists: Gen[List[Int]] = implicitly[Arbitrary[List[Int]]].arbitrary

  private[types] def keyLists(implicit k: Arbitrary[Key]): Gen[List[Key]] = listOf(k.arbitrary)

  private[types] def wightedKeyLists(implicit kv: Arbitrary[(Key, ValidDouble)]): Gen[List[(Key, ValidDouble)]] =
    listOf(kv.arbitrary)

  private[types] implicit val illegalArgumentException: IllegalArgumentException => Prop = _ => Prop.passed
}
