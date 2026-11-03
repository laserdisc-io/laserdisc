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

trait HashExtP { this: HashBaseP =>
  //format: off
  //HDEL
  final def hdel(key: Key, field1: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key, field17: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: field17 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key, field17: Key, field18: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: field17 :: field18 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key, field17: Key, field18: Key, field19: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: field17 :: field18 :: field19 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key, field17: Key, field18: Key, field19: Key, field20: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: field17 :: field18 :: field19 :: field20 :: Nil))
  final def hdel(key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key, field17: Key, field18: Key, field19: Key, field20: Key, field21: Key): Single.Aux[NonNegInt] =
    hdel(key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: field17 :: field18 :: field19 :: field20 :: field21 :: Nil))

  //HMGET
  final def hmget[A1](key: Key, field1: Key)(
      implicit ev: Read[Arr.Of[Option[Bulk]], (A1 *: EmptyTuple)]
  ): Single.Aux[A1] =
    hmget[A1 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: Nil)).map(_.head)
  final def hmget[A1, A2](key: Key, field1: Key, field2: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: EmptyTuple, (A1, A2)]
  ): Single.Aux[(A1, A2)] =
    hmget[A1 *: A2 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3](key: Key, field1: Key, field2: Key, field3: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: EmptyTuple, (A1, A2, A3)]
  ): Single.Aux[(A1, A2, A3)] =
    hmget[A1 *: A2 *: A3 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4](key: Key, field1: Key, field2: Key, field3: Key, field4: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: EmptyTuple, (A1, A2, A3, A4)]
  ): Single.Aux[(A1, A2, A3, A4)] =
    hmget[A1 *: A2 *: A3 *: A4 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: EmptyTuple, (A1, A2, A3, A4, A5)]
  ): Single.Aux[(A1, A2, A3, A4, A5)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: EmptyTuple, (A1, A2, A3, A4, A5, A6)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key, field17: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: field17 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key, field17: Key, field18: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: field17 :: field18 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key, field17: Key, field18: Key, field19: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: field17 :: field18 :: field19 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key, field17: Key, field18: Key, field19: Key, field20: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: field17 :: field18 :: field19 :: field20 :: Nil)).map(ev1(_))
  final def hmget[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](key: Key, field1: Key, field2: Key, field3: Key, field4: Key, field5: Key, field6: Key, field7: Key, field8: Key, field9: Key, field10: Key, field11: Key, field12: Key, field13: Key, field14: Key, field15: Key, field16: Key, field17: Key, field18: Key, field19: Key, field20: Key, field21: Key)(
      implicit ev0: Read[Arr.Of[Option[Bulk]], (A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: EmptyTuple)],
      ev1: Tupled[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: EmptyTuple, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)]
  ): Single.Aux[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] =
    hmget[A1 *: A2 *: A3 *: A4 *: A5 *: A6 *: A7 *: A8 *: A9 *: A10 *: A11 *: A12 *: A13 *: A14 *: A15 *: A16 *: A17 *: A18 *: A19 *: A20 *: A21 *: EmptyTuple](key, OneOrMoreKeys.unsafeFrom(field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: field9 :: field10 :: field11 :: field12 :: field13 :: field14 :: field15 :: field16 :: field17 :: field18 :: field19 :: field20 :: field21 :: Nil)).map(ev1(_))

  //HMSET
  final def hmset[A1: Show](key: Key, field1: Key, value1: A1): Single.Aux[OK] =
    hmset(key, (field1, value1) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show, A11: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10, field11: Key, value11: A11): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: (field11, value11) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show, A11: Show, A12: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10, field11: Key, value11: A11, field12: Key, value12: A12): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: (field11, value11) *: (field12, value12) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show, A11: Show, A12: Show, A13: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10, field11: Key, value11: A11, field12: Key, value12: A12, field13: Key, value13: A13): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: (field11, value11) *: (field12, value12) *: (field13, value13) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show, A11: Show, A12: Show, A13: Show, A14: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10, field11: Key, value11: A11, field12: Key, value12: A12, field13: Key, value13: A13, field14: Key, value14: A14): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: (field11, value11) *: (field12, value12) *: (field13, value13) *: (field14, value14) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show, A11: Show, A12: Show, A13: Show, A14: Show, A15: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10, field11: Key, value11: A11, field12: Key, value12: A12, field13: Key, value13: A13, field14: Key, value14: A14, field15: Key, value15: A15): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: (field11, value11) *: (field12, value12) *: (field13, value13) *: (field14, value14) *: (field15, value15) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show, A11: Show, A12: Show, A13: Show, A14: Show, A15: Show, A16: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10, field11: Key, value11: A11, field12: Key, value12: A12, field13: Key, value13: A13, field14: Key, value14: A14, field15: Key, value15: A15, field16: Key, value16: A16): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: (field11, value11) *: (field12, value12) *: (field13, value13) *: (field14, value14) *: (field15, value15) *: (field16, value16) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show, A11: Show, A12: Show, A13: Show, A14: Show, A15: Show, A16: Show, A17: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10, field11: Key, value11: A11, field12: Key, value12: A12, field13: Key, value13: A13, field14: Key, value14: A14, field15: Key, value15: A15, field16: Key, value16: A16, field17: Key, value17: A17): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: (field11, value11) *: (field12, value12) *: (field13, value13) *: (field14, value14) *: (field15, value15) *: (field16, value16) *: (field17, value17) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show, A11: Show, A12: Show, A13: Show, A14: Show, A15: Show, A16: Show, A17: Show, A18: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10, field11: Key, value11: A11, field12: Key, value12: A12, field13: Key, value13: A13, field14: Key, value14: A14, field15: Key, value15: A15, field16: Key, value16: A16, field17: Key, value17: A17, field18: Key, value18: A18): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: (field11, value11) *: (field12, value12) *: (field13, value13) *: (field14, value14) *: (field15, value15) *: (field16, value16) *: (field17, value17) *: (field18, value18) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show, A11: Show, A12: Show, A13: Show, A14: Show, A15: Show, A16: Show, A17: Show, A18: Show, A19: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10, field11: Key, value11: A11, field12: Key, value12: A12, field13: Key, value13: A13, field14: Key, value14: A14, field15: Key, value15: A15, field16: Key, value16: A16, field17: Key, value17: A17, field18: Key, value18: A18, field19: Key, value19: A19): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: (field11, value11) *: (field12, value12) *: (field13, value13) *: (field14, value14) *: (field15, value15) *: (field16, value16) *: (field17, value17) *: (field18, value18) *: (field19, value19) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show, A11: Show, A12: Show, A13: Show, A14: Show, A15: Show, A16: Show, A17: Show, A18: Show, A19: Show, A20: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10, field11: Key, value11: A11, field12: Key, value12: A12, field13: Key, value13: A13, field14: Key, value14: A14, field15: Key, value15: A15, field16: Key, value16: A16, field17: Key, value17: A17, field18: Key, value18: A18, field19: Key, value19: A19, field20: Key, value20: A20): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: (field11, value11) *: (field12, value12) *: (field13, value13) *: (field14, value14) *: (field15, value15) *: (field16, value16) *: (field17, value17) *: (field18, value18) *: (field19, value19) *: (field20, value20) *: EmptyTuple)
  final def hmset[A1: Show, A2: Show, A3: Show, A4: Show, A5: Show, A6: Show, A7: Show, A8: Show, A9: Show, A10: Show, A11: Show, A12: Show, A13: Show, A14: Show, A15: Show, A16: Show, A17: Show, A18: Show, A19: Show, A20: Show, A21: Show](key: Key, field1: Key, value1: A1, field2: Key, value2: A2, field3: Key, value3: A3, field4: Key, value4: A4, field5: Key, value5: A5, field6: Key, value6: A6, field7: Key, value7: A7, field8: Key, value8: A8, field9: Key, value9: A9, field10: Key, value10: A10, field11: Key, value11: A11, field12: Key, value12: A12, field13: Key, value13: A13, field14: Key, value14: A14, field15: Key, value15: A15, field16: Key, value16: A16, field17: Key, value17: A17, field18: Key, value18: A18, field19: Key, value19: A19, field20: Key, value20: A20, field21: Key, value21: A21): Single.Aux[OK] =
    hmset(key, (field1, value1) *: (field2, value2) *: (field3, value3) *: (field4, value4) *: (field5, value5) *: (field6, value6) *: (field7, value7) *: (field8, value8) *: (field9, value9) *: (field10, value10) *: (field11, value11) *: (field12, value12) *: (field13, value13) *: (field14, value14) *: (field15, value15) *: (field16, value16) *: (field17, value17) *: (field18, value18) *: (field19, value19) *: (field20, value20) *: (field21, value21) *: EmptyTuple)
  //format: on
}
