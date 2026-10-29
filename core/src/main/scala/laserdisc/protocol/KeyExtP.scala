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

trait KeyExtP { this: KeyBaseP =>
  import keytypes.*

  //format: off
  //DEL
  final def del(key1: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: Nil))
  final def del(key1: Key, key2: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil))
  final def del(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, key22: Key): Protocol.Aux[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: key22 :: Nil))

  //EXISTS
  final def exists(key1: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: Nil))
  final def exists(key1: Key, key2: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil))
  final def exists(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, key22: Key): Protocol.Aux[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: key22 :: Nil))

  //MIGRATE
  final def migrate(key1: Key, key2: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyMigrateMode): Protocol.Aux[NOKEY | OK] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), host, port, dbIndex, timeout, mode)

  //TOUCH
  final def touch(key1: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: Nil))
  final def touch(key1: Key, key2: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil))
  final def touch(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, key22: Key): Protocol.Aux[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: key22 :: Nil))

  //UNLINK
  final def unlink(key1: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: Nil))
  final def unlink(key1: Key, key2: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: Nil))
  final def unlink(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, key19: Key, key20: Key, key21: Key, key22: Key): Protocol.Aux[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: key19 :: key20 :: key21 :: key22 :: Nil))
  //format: on
}
