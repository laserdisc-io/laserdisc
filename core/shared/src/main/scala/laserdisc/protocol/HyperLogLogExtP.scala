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

trait HyperLogLogExtP { this: HyperLogLogBaseP =>

  //format: off
  //PFADD
  final def pfadd(key: Key, element1: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key, element11: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: element11 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key, element11: Key, element12: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: element11 :: element12 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key, element11: Key, element12: Key, element13: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: element11 :: element12 :: element13 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key, element11: Key, element12: Key, element13: Key, element14: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: element11 :: element12 :: element13 :: element14 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key, element11: Key, element12: Key, element13: Key, element14: Key, element15: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: element11 :: element12 :: element13 :: element14 :: element15 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key, element11: Key, element12: Key, element13: Key, element14: Key, element15: Key, element16: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: element11 :: element12 :: element13 :: element14 :: element15 :: element16 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key, element11: Key, element12: Key, element13: Key, element14: Key, element15: Key, element16: Key, element17: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: element11 :: element12 :: element13 :: element14 :: element15 :: element16 :: element17 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key, element11: Key, element12: Key, element13: Key, element14: Key, element15: Key, element16: Key, element17: Key, element18: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: element11 :: element12 :: element13 :: element14 :: element15 :: element16 :: element17 :: element18 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key, element11: Key, element12: Key, element13: Key, element14: Key, element15: Key, element16: Key, element17: Key, element18: Key, element19: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: element11 :: element12 :: element13 :: element14 :: element15 :: element16 :: element17 :: element18 :: element19 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key, element11: Key, element12: Key, element13: Key, element14: Key, element15: Key, element16: Key, element17: Key, element18: Key, element19: Key, element20: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: element11 :: element12 :: element13 :: element14 :: element15 :: element16 :: element17 :: element18 :: element19 :: element20 :: Nil))
  final def pfadd(key: Key, element1: Key, element2: Key, element3: Key, element4: Key, element5: Key, element6: Key, element7: Key, element8: Key, element9: Key, element10: Key, element11: Key, element12: Key, element13: Key, element14: Key, element15: Key, element16: Key, element17: Key, element18: Key, element19: Key, element20: Key, element21: Key): Protocol.Aux[Boolean] =
    pfadd(key, OneOrMoreKeys.unsafeFrom(element1 :: element2 :: element3 :: element4 :: element5 :: element6 :: element7 :: element8 :: element9 :: element10 :: element11 :: element12 :: element13 :: element14 :: element15 :: element16 :: element17 :: element18 :: element19 :: element20 :: element21 :: Nil))

  //PFCOUNT
  final def pfcount(key1: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: Nil))
  final def pfcount(key1: Key, key2: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil))
  final def pfcount(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, key22: Key): Protocol.Aux[NonNegInt] =
    pfcount(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: key22 :: Nil))

  //PFMERGE
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, sourceKey17: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: sourceKey17 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, sourceKey17: Key, sourceKey18: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: sourceKey17 :: sourceKey18 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, sourceKey17: Key, sourceKey18: Key, sourceKey19: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: sourceKey17 :: sourceKey18 :: sourceKey19 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, sourceKey17: Key, sourceKey18: Key, sourceKey19: Key, sourceKey20: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: sourceKey17 :: sourceKey18 :: sourceKey19 :: sourceKey20 :: Nil), destinationKey)
  final def pfmerge(sourceKey1: Key, sourceKey2: Key, sourceKey3: Key, sourceKey4: Key, sourceKey5: Key, sourceKey6: Key, sourceKey7: Key, sourceKey8: Key, sourceKey9: Key, sourceKey10: Key, sourceKey11: Key, sourceKey12: Key, sourceKey13: Key, sourceKey14: Key, sourceKey15: Key, sourceKey16: Key, sourceKey17: Key, sourceKey18: Key, sourceKey19: Key, sourceKey20: Key, sourceKey21: Key, destinationKey: Key): Protocol.Aux[OK] =
    pfmerge(TwoOrMoreKeys.unsafeFrom(sourceKey1 :: sourceKey2 :: sourceKey3 :: sourceKey4 :: sourceKey5 :: sourceKey6 :: sourceKey7 :: sourceKey8 :: sourceKey9 :: sourceKey10 :: sourceKey11 :: sourceKey12 :: sourceKey13 :: sourceKey14 :: sourceKey15 :: sourceKey16 :: sourceKey17 :: sourceKey18 :: sourceKey19 :: sourceKey20 :: sourceKey21 :: Nil), destinationKey)
  //format: on
}
