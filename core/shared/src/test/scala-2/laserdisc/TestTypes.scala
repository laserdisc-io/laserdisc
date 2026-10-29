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

import eu.timepit.refined.W
import eu.timepit.refined.api.{RefType, Refined}
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.scalacheck.{CollectionInstancesBinCompat1, NumericInstances, StringInstances}

object TestTypes {
  final type EmptyString = String Refined Equal[W.`""`.T]
  final val EmptyString: EmptyString = RefType.applyRefM[EmptyString]("")

  final type NonEmptyString = eu.timepit.refined.types.string.NonEmptyString
  final val NonEmptyString = eu.timepit.refined.types.string.NonEmptyString
}

private[laserdisc] trait BaseSpecPlatform extends CollectionInstancesBinCompat1 with NumericInstances with StringInstances {
  protected final val isScala3: Boolean = false
}
