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

import Protocol.Single

trait HyperLogLogExtP { this: HyperLogLogBaseP =>

  //format: off
  //PFADD
  final def pfadd(key: Key, element: Key, elements: Key*): Single[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element :: elements.toList))

  //PFCOUNT
  final def pfcount(key: Key, keys: Key*): Single[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key :: keys.toList))

  //PFMERGE
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, sourceKey17: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: sourceKey17 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, sourceKey17: Key, sourceKey18: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: sourceKey17 :: sourceKey18 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, sourceKey17: Key, sourceKey18: Key, sourceKey19: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: sourceKey17 :: sourceKey18 :: sourceKey19 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, sourceKey17: Key, sourceKey18: Key, sourceKey19: Key, sourceKey20: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: sourceKey17 :: sourceKey18 :: sourceKey19 :: sourceKey20 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, sourceKey17: Key, sourceKey18: Key, sourceKey19: Key, sourceKey20: Key, sourceKey21: Key, destinationKey: Key): Single[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: sourceKey17 :: sourceKey18 :: sourceKey19 :: sourceKey20 :: sourceKey21 :: Nil), destinationKey)
  //format: on
}
