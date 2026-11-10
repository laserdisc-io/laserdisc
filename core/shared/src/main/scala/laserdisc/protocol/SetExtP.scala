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

trait SetExtP { this: SetBaseP =>

  //format: off
  //SADD
  final def sadd[A: Show](key: Key, member: A, members: A*): Single[NonNegInt] =
    sadd(key, OneOrMore.unsafeFrom(member :: members.toList))

  //SDIFF
  final def sdiff[A: Read[Bulk, _]](key1: Key, key2: Key, keys: Key*): Single[Seq[A]] =
    sdiff[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: keys.toList))

  //SDIFFSTORE
  final def sdiffstore(key1: Key, key2: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil), destinationKey)
  final def sdiffstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, destinationKey: Key): Single[NonNegInt] =
    sdiffstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil), destinationKey)

  //SINTER
  final def sinter[A: Read[Bulk, _]](key1: Key, key2: Key, keys: Key*): Single[Seq[A]] =
    sinter[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: keys.toList))

  //SINTERSTORE
  final def sinterstore(key1: Key, key2: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil), destinationKey)
  final def sinterstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, destinationKey: Key): Single[NonNegInt] =
    sinterstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil), destinationKey)

  //SREM
  final def srem[A: Show](key: Key, member: A, members: A*): Single[NonNegInt] =
    srem(key, OneOrMore.unsafeFrom(member :: members.toList))

  //SUNION
  final def sunion[A: Read[Bulk, _]](key1: Key, key2: Key, keys: Key*): Single[Seq[A]] =
    sunion[A](TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: keys.toList))

  //SUNIONSTORE
  final def sunionstore(key1: Key, key2: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil), destinationKey)
  final def sunionstore(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, destinationKey: Key): Single[NonNegInt] =
    sunionstore(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil), destinationKey)
  //format: on
}
