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

trait GeoExtP { this: GeoBaseP =>
  import geotypes.*

  //format: off
  //GEOADD
  final def geoadd(key: Key, position1: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition, position11: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: position11 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition, position11: GeoPosition, position12: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: position11 :: position12 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition, position11: GeoPosition, position12: GeoPosition, position13: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: position11 :: position12 :: position13 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition, position11: GeoPosition, position12: GeoPosition, position13: GeoPosition, position14: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: position11 :: position12 :: position13 :: position14 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition, position11: GeoPosition, position12: GeoPosition, position13: GeoPosition, position14: GeoPosition, position15: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: position11 :: position12 :: position13 :: position14 :: position15 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition, position11: GeoPosition, position12: GeoPosition, position13: GeoPosition, position14: GeoPosition, position15: GeoPosition, position16: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: position11 :: position12 :: position13 :: position14 :: position15 :: position16 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition, position11: GeoPosition, position12: GeoPosition, position13: GeoPosition, position14: GeoPosition, position15: GeoPosition, position16: GeoPosition, position17: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: position11 :: position12 :: position13 :: position14 :: position15 :: position16 :: position17 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition, position11: GeoPosition, position12: GeoPosition, position13: GeoPosition, position14: GeoPosition, position15: GeoPosition, position16: GeoPosition, position17: GeoPosition, position18: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: position11 :: position12 :: position13 :: position14 :: position15 :: position16 :: position17 :: position18 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition, position11: GeoPosition, position12: GeoPosition, position13: GeoPosition, position14: GeoPosition, position15: GeoPosition, position16: GeoPosition, position17: GeoPosition, position18: GeoPosition, position19: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: position11 :: position12 :: position13 :: position14 :: position15 :: position16 :: position17 :: position18 :: position19 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition, position11: GeoPosition, position12: GeoPosition, position13: GeoPosition, position14: GeoPosition, position15: GeoPosition, position16: GeoPosition, position17: GeoPosition, position18: GeoPosition, position19: GeoPosition, position20: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: position11 :: position12 :: position13 :: position14 :: position15 :: position16 :: position17 :: position18 :: position19 :: position20 :: Nil))
  final def geoadd(key: Key, position1: GeoPosition, position2: GeoPosition, position3: GeoPosition, position4: GeoPosition, position5: GeoPosition, position6: GeoPosition, position7: GeoPosition, position8: GeoPosition, position9: GeoPosition, position10: GeoPosition, position11: GeoPosition, position12: GeoPosition, position13: GeoPosition, position14: GeoPosition, position15: GeoPosition, position16: GeoPosition, position17: GeoPosition, position18: GeoPosition, position19: GeoPosition, position20: GeoPosition, position21: GeoPosition): Protocol.Aux[NonNegInt] =
    geoadd(key, OneOrMore.unsafeFrom(position1 :: position2 :: position3 :: position4 :: position5 :: position6 :: position7 :: position8 :: position9 :: position10 :: position11 :: position12 :: position13 :: position14 :: position15 :: position16 :: position17 :: position18 :: position19 :: position20 :: position21 :: Nil))

  //GEOHASH
  final def geohash(key: Key, member1: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key, member17: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key, member17: Key, member18: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key, member17: Key, member18: Key, member19: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key, member17: Key, member18: Key, member19: Key, member20: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: member20 :: Nil))
  final def geohash(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key, member17: Key, member18: Key, member19: Key, member20: Key, member21: Key): Protocol.Aux[Seq[Option[GeoHash]]] =
    geohash(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: member20 :: member21 :: Nil))

  //GEOPOS
  final def geopos(key: Key, member1: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key, member17: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key, member17: Key, member18: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key, member17: Key, member18: Key, member19: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key, member17: Key, member18: Key, member19: Key, member20: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: member20 :: Nil))
  final def geopos(key: Key, member1: Key, member2: Key, member3: Key, member4: Key, member5: Key, member6: Key, member7: Key, member8: Key, member9: Key, member10: Key, member11: Key, member12: Key, member13: Key, member14: Key, member15: Key, member16: Key, member17: Key, member18: Key, member19: Key, member20: Key, member21: Key): Protocol.Aux[Seq[Option[GeoCoordinates]]] =
    geopos(key, OneOrMoreKeys.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: member20 :: member21 :: Nil))
  //format: on
}
