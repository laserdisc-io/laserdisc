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

trait BListExtP { this: BListBaseP =>

  //format: off
  //BLPOP with no timeout specified
  final def blpop[A: Read[Bulk, _]](key1: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil), NonNegInt(0))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, key22: Key): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: key22 :: Nil), NonNegInt(0))

  //BLPOP with timeout specified
  final def blpop[A: Read[Bulk, _]](key1: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def blpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    blpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil), NonNegInt.unsafeFrom(seconds.value))

  //BRPOP with no timeout specified
  final def brpop[A: Read[Bulk, _]](key1: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil), NonNegInt(0))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, key22: Key): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: key22 :: Nil), NonNegInt(0))

  //BRPOP with timeout specified
  final def brpop[A: Read[Bulk, _]](key1: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  final def brpop[A: Read[Bulk, _]](key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, seconds: PosInt): Protocol.Aux[Option[KV[A]]] =
    brpop[A](OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil), NonNegInt.unsafeFrom(seconds.value))
  //format: on
}
