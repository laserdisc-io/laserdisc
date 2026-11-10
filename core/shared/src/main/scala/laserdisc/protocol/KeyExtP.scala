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

trait KeyExtP { this: KeyBaseP =>
  //format: off
  //DEL
  final def del(key: Key, keys: Key*): Single[NonNegInt] =
    del(OneOrMoreKeys.unsafeFrom(key :: keys.toList))

  //EXISTS
  final def exists(key: Key, keys: Key*): Single[Option[PosInt]] =
    exists(OneOrMoreKeys.unsafeFrom(key :: keys.toList))

  //TOUCH
  final def touch(key: Key, keys: Key*): Single[NonNegInt] =
    touch(OneOrMoreKeys.unsafeFrom(key :: keys.toList))

  //UNLINK
  final def unlink(key: Key, keys: Key*): Single[NonNegInt] =
    unlink(OneOrMoreKeys.unsafeFrom(key :: keys.toList))
  //format: on
}

trait KeyMigrateExtP { this: KeyMigrateP =>
  //format: off
  //MIGRATE
  final def migrate(key1: Key, key2: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, key18: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: key18 :: Nil), host, port, dbIndex, timeout)
  final def migrate(key1: Key, key2: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: Nil), host, port, dbIndex, timeout, mode)
  final def migrate(key1: Key, key2: Key, key3: Key, key4: Key, key5: Key, key6: Key, key7: Key, key8: Key, key9: Key, key10: Key, key11: Key, key12: Key, key13: Key, key14: Key, key15: Key, key16: Key, key17: Key, host: Host, port: Port, dbIndex: DbIndex, timeout: NonNegInt, mode: KeyP.MigrateMode): Single[Either[NOKEY, OK]] =
    migrate(TwoOrMoreKeys.unsafeFrom(key1 :: key2 :: key3 :: key4 :: key5 :: key6 :: key7 :: key8 :: key9 :: key10 :: key11 :: key12 :: key13 :: key14 :: key15 :: key16 :: key17 :: Nil), host, port, dbIndex, timeout, mode)
  //format: on
}
