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

trait ListExtP { this: ListBaseP =>

  //format: off
  //LPUSH
  final def lpush[A: Show](key: Key, value1: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A, value17: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: value17 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A, value17: A, value18: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: value17 :: value18 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A, value17: A, value18: A, value19: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: value17 :: value18 :: value19 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A, value17: A, value18: A, value19: A, value20: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: value17 :: value18 :: value19 :: value20 :: Nil))
  final def lpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A, value17: A, value18: A, value19: A, value20: A, value21: A): Protocol.Aux[PosInt] =
    lpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: value17 :: value18 :: value19 :: value20 :: value21 :: Nil))

  //RPUSH
  final def rpush[A: Show](key: Key, value1: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A, value17: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: value17 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A, value17: A, value18: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: value17 :: value18 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A, value17: A, value18: A, value19: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: value17 :: value18 :: value19 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A, value17: A, value18: A, value19: A, value20: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: value17 :: value18 :: value19 :: value20 :: Nil))
  final def rpush[A: Show](key: Key, value1: A, value2: A, value3: A, value4: A, value5: A, value6: A, value7: A, value8: A, value9: A, value10: A, value11: A, value12: A, value13: A, value14: A, value15: A, value16: A, value17: A, value18: A, value19: A, value20: A, value21: A): Protocol.Aux[PosInt] =
    rpush(key, OneOrMore.unsafeFrom(value1 :: value2 :: value3 :: value4 :: value5 :: value6 :: value7 :: value8 :: value9 :: value10 :: value11 :: value12 :: value13 :: value14 :: value15 :: value16 :: value17 :: value18 :: value19 :: value20 :: value21 :: Nil))
  //format: on
}
