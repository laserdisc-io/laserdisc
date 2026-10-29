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

trait SetExtP { this: SetBaseP =>

  //format: off
  //SADD
  final def sadd[A: Show](key: Key, member1: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A, member17: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A, member17: A, member18: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A, member17: A, member18: A, member19: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A, member17: A, member18: A, member19: A, member20: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: member20 :: Nil))
  final def sadd[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A, member17: A, member18: A, member19: A, member20: A, member21: A): Protocol.Aux[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: member20 :: member21 :: Nil))

  //SDIFF
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil))
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, key22: Key): Protocol.Aux[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: key22 :: Nil))

  //SDIFFSTORE
  final def sdiffstore(key1: Key, key2: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil), destinationKey)

  //SINTER
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil))
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, key22: Key): Protocol.Aux[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: key22 :: Nil))

  //SINTERSTORE
  final def sinterstore(key1: Key, key2: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil), destinationKey)

  //SREM
  final def srem[A: Show](key: Key, member1: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A, member17: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A, member17: A, member18: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A, member17: A, member18: A, member19: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A, member17: A, member18: A, member19: A, member20: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: member20 :: Nil))
  final def srem[A: Show](key: Key, member1: A, member2: A, member3: A, member4: A, member5: A, member6: A, member7: A, member8: A, member9: A, member10: A, member11: A, member12: A, member13: A, member14: A, member15: A, member16: A, member17: A, member18: A, member19: A, member20: A, member21: A): Protocol.Aux[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member1 :: member2 :: member3 :: member4 :: member5 :: member6 :: member7 :: member8 :: member9 :: member10 :: member11 :: member12 :: member13 :: member14 :: member15 :: member16 :: member17 :: member18 :: member19 :: member20 :: member21 :: Nil))

  //SUNION
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil))
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, key22: Key): Protocol.Aux[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: key22 :: Nil))

  //SUNIONSTORE
  final def sunionstore(key1: Key, key2: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, destinationKey: Key): Protocol.Aux[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil), destinationKey)
  //format: on
}
