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

import org.scalacheck.Prop.forAll

final class KeyExtPSpec extends KeyPSpec {
  import keytypes.*

  property("The Key extended protocol using del roundtrips successfully given key1") {
    forAll { (k1: Key, nni: NonNegInt) =>
      val protocol = del(k1)
      assertEquals(protocol.encode, Arr(Bulk("DEL"), Bulk(k1)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using del roundtrips successfully given key1, key2") {
    forAll { (k1: Key, k2: Key, nni: NonNegInt) =>
      val protocol = del(k1, k2)
      assertEquals(protocol.encode, Arr(Bulk("DEL"), Bulk(k1), Bulk(k2)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using del roundtrips successfully given key1, key2, key3") {
    forAll { (k1: Key, k2: Key, k3: Key, nni: NonNegInt) =>
      val protocol = del(k1, k2, k3)
      assertEquals(protocol.encode, Arr(Bulk("DEL"), Bulk(k1), Bulk(k2), Bulk(k3)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using del roundtrips successfully given key1, key2, key3, key4") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, nni: NonNegInt) =>
      val protocol = del(k1, k2, k3, k4)
      assertEquals(protocol.encode, Arr(Bulk("DEL"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using del roundtrips successfully given key1, key2, key3, key4, key5") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, nni: NonNegInt) =>
      val protocol = del(k1, k2, k3, k4, k5)
      assertEquals(protocol.encode, Arr(Bulk("DEL"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using exists roundtrips successfully given key1") {
    forAll { (k1: Key, opi: Option[PosInt]) =>
      val protocol = exists(k1)
      assertEquals(protocol.encode, Arr(Bulk("EXISTS"), Bulk(k1)))
      assertEquals(protocol.decode(Num(opi.fold(0L)(_.value.toLong))), opi)
    }
  }
  property("The Key extended protocol using exists roundtrips successfully given key1, key2") {
    forAll { (k1: Key, k2: Key, opi: Option[PosInt]) =>
      val protocol = exists(k1, k2)
      assertEquals(protocol.encode, Arr(Bulk("EXISTS"), Bulk(k1), Bulk(k2)))
      assertEquals(protocol.decode(Num(opi.fold(0L)(_.value.toLong))), opi)
    }
  }
  property("The Key extended protocol using exists roundtrips successfully given key1, key2, key3") {
    forAll { (k1: Key, k2: Key, k3: Key, opi: Option[PosInt]) =>
      val protocol = exists(k1, k2, k3)
      assertEquals(protocol.encode, Arr(Bulk("EXISTS"), Bulk(k1), Bulk(k2), Bulk(k3)))
      assertEquals(protocol.decode(Num(opi.fold(0L)(_.value.toLong))), opi)
    }
  }
  property("The Key extended protocol using exists roundtrips successfully given key1, key2, key3, key4") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, opi: Option[PosInt]) =>
      val protocol = exists(k1, k2, k3, k4)
      assertEquals(protocol.encode, Arr(Bulk("EXISTS"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4)))
      assertEquals(protocol.decode(Num(opi.fold(0L)(_.value.toLong))), opi)
    }
  }
  property("The Key extended protocol using exists roundtrips successfully given key1, key2, key3, key4, key5") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, opi: Option[PosInt]) =>
      val protocol = exists(k1, k2, k3, k4, k5)
      assertEquals(protocol.encode, Arr(Bulk("EXISTS"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5)))
      assertEquals(protocol.decode(Num(opi.fold(0L)(_.value.toLong))), opi)
    }
  }
  property("The Key extended protocol using migrate roundtrips successfully given key1, key2, host, port, db index and timeout") {
    forAll { (keys: (Key, Key), input: (Host, Port, DbIndex, NonNegInt, Either[NOKEY, OK])) =>
      val (k1, k2)                 = keys
      val (h, p, dbi, nni, nkOrOk) = input
      val protocol                 = migrate(k1, k2, h, p, dbi, nni)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: Bulk("KEYS") :: Bulk(k1) :: Bulk(k2) :: Nil
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }
  property("The Key extended protocol using migrate roundtrips successfully given key1, key2, key3, host, port, db index and timeout") {
    forAll { (keys: (Key, Key, Key), input: (Host, Port, DbIndex, NonNegInt, Either[NOKEY, OK])) =>
      val (k1, k2, k3)             = keys
      val (h, p, dbi, nni, nkOrOk) = input
      val protocol                 = migrate(k1, k2, k3, h, p, dbi, nni)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: Bulk("KEYS") :: Bulk(k1) :: Bulk(k2) :: Bulk(
            k3
          ) :: Nil
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }
  property(
    "The Key extended protocol using migrate roundtrips successfully given key1, key2, key3, key4, host, port, db index and timeout"
  ) {
    forAll { (keys: (Key, Key, Key, Key), input: (Host, Port, DbIndex, NonNegInt, Either[NOKEY, OK])) =>
      val (k1, k2, k3, k4)         = keys
      val (h, p, dbi, nni, nkOrOk) = input
      val protocol                 = migrate(k1, k2, k3, k4, h, p, dbi, nni)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: Bulk("KEYS") :: Bulk(k1) :: Bulk(k2) :: Bulk(
            k3
          ) :: Bulk(k4) :: Nil
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }
  property(
    "The Key extended protocol using migrate roundtrips successfully given key1, key2, key3, key4, key5, host, port, db index and timeout"
  ) {
    forAll { (keys: (Key, Key, Key, Key, Key), input: (Host, Port, DbIndex, NonNegInt, Either[NOKEY, OK])) =>
      val (k1, k2, k3, k4, k5)     = keys
      val (h, p, dbi, nni, nkOrOk) = input
      val protocol                 = migrate(k1, k2, k3, k4, k5, h, p, dbi, nni)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: Bulk("KEYS") :: Bulk(k1) :: Bulk(k2) :: Bulk(
            k3
          ) :: Bulk(k4) :: Bulk(k5) :: Nil
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }
  property(
    "The Key extended protocol using migrate roundtrips successfully given key1, key2, key3, key4, key5, key6, host, port, db index and timeout"
  ) {
    forAll { (keys: (Key, Key, Key, Key, Key, Key), input: (Host, Port, DbIndex, NonNegInt, Either[NOKEY, OK])) =>
      val (k1, k2, k3, k4, k5, k6) = keys
      val (h, p, dbi, nni, nkOrOk) = input
      val protocol                 = migrate(k1, k2, k3, k4, k5, k6, h, p, dbi, nni)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: Bulk("KEYS") :: Bulk(k1) :: Bulk(k2) :: Bulk(
            k3
          ) :: Bulk(k4) :: Bulk(k5) :: Bulk(k6) :: Nil
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }
  property(
    "The Key extended protocol using migrate roundtrips successfully given key1, key2, host, port, db index, timeout and migrate mode"
  ) {
    forAll { (keys: (Key, Key), input: (Host, Port, DbIndex, NonNegInt, KeyMigrateMode, Either[NOKEY, OK])) =>
      val (k1, k2)                     = keys
      val (h, p, dbi, nni, mm, nkOrOk) = input
      val protocol                     = migrate(k1, k2, h, p, dbi, nni, mm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: mm.params
            .map(Bulk(_)) ::: (Bulk("KEYS") :: Bulk(k1) :: Bulk(k2) :: Nil)
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }
  property(
    "The Key extended protocol using migrate roundtrips successfully given key1, key2, key3, host, port, db index, timeout and migrate mode"
  ) {
    forAll { (keys: (Key, Key, Key), input: (Host, Port, DbIndex, NonNegInt, KeyMigrateMode, Either[NOKEY, OK])) =>
      val (k1, k2, k3)                 = keys
      val (h, p, dbi, nni, mm, nkOrOk) = input
      val protocol                     = migrate(k1, k2, k3, h, p, dbi, nni, mm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: mm.params
            .map(Bulk(_)) ::: (Bulk("KEYS") :: Bulk(k1) :: Bulk(k2) :: Bulk(k3) :: Nil)
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }
  property(
    "The Key extended protocol using migrate roundtrips successfully given key1, key2, key3, key4, host, port, db index, timeout and migrate mode"
  ) {
    forAll { (keys: (Key, Key, Key, Key), input: (Host, Port, DbIndex, NonNegInt, KeyMigrateMode, Either[NOKEY, OK])) =>
      val (k1, k2, k3, k4)             = keys
      val (h, p, dbi, nni, mm, nkOrOk) = input
      val protocol                     = migrate(k1, k2, k3, k4, h, p, dbi, nni, mm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: mm.params
            .map(Bulk(_)) ::: (Bulk("KEYS") :: Bulk(k1) :: Bulk(k2) :: Bulk(k3) :: Bulk(k4) :: Nil)
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }
  property(
    "The Key extended protocol using migrate roundtrips successfully given key1, key2, key3, key4, key5, host, port, db index, timeout and migrate mode"
  ) {
    forAll { (keys: (Key, Key, Key, Key, Key), input: (Host, Port, DbIndex, NonNegInt, KeyMigrateMode, Either[NOKEY, OK])) =>
      val (k1, k2, k3, k4, k5)         = keys
      val (h, p, dbi, nni, mm, nkOrOk) = input
      val protocol                     = migrate(k1, k2, k3, k4, k5, h, p, dbi, nni, mm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: mm.params
            .map(Bulk(_)) ::: (Bulk("KEYS") :: Bulk(k1) :: Bulk(k2) :: Bulk(k3) :: Bulk(k4) :: Bulk(k5) :: Nil)
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }
  property(
    "The Key extended protocol using migrate roundtrips successfully given key1, key2, key3, key4, key5, key6, host, port, db index, timeout and migrate mode"
  ) {
    forAll { (keys: (Key, Key, Key, Key, Key, Key), input: (Host, Port, DbIndex, NonNegInt, KeyMigrateMode, Either[NOKEY, OK])) =>
      val (k1, k2, k3, k4, k5, k6)     = keys
      val (h, p, dbi, nni, mm, nkOrOk) = input
      val protocol                     = migrate(k1, k2, k3, k4, k5, k6, h, p, dbi, nni, mm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: mm.params
            .map(Bulk(_)) ::: (Bulk("KEYS") :: Bulk(k1) :: Bulk(k2) :: Bulk(k3) :: Bulk(k4) :: Bulk(k5) :: Bulk(k6) :: Nil)
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }
  property("The Key extended protocol using touch roundtrips successfully given key1") {
    forAll { (k1: Key, nni: NonNegInt) =>
      val protocol = touch(k1)
      assertEquals(protocol.encode, Arr(Bulk("TOUCH"), Bulk(k1)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using touch roundtrips successfully given key1, key2") {
    forAll { (k1: Key, k2: Key, nni: NonNegInt) =>
      val protocol = touch(k1, k2)
      assertEquals(protocol.encode, Arr(Bulk("TOUCH"), Bulk(k1), Bulk(k2)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using touch roundtrips successfully given key1, key2, key3") {
    forAll { (k1: Key, k2: Key, k3: Key, nni: NonNegInt) =>
      val protocol = touch(k1, k2, k3)
      assertEquals(protocol.encode, Arr(Bulk("TOUCH"), Bulk(k1), Bulk(k2), Bulk(k3)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using touch roundtrips successfully given key1, key2, key3, key4") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, nni: NonNegInt) =>
      val protocol = touch(k1, k2, k3, k4)
      assertEquals(protocol.encode, Arr(Bulk("TOUCH"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using touch roundtrips successfully given key1, key2, key3, key4, key5") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, nni: NonNegInt) =>
      val protocol = touch(k1, k2, k3, k4, k5)
      assertEquals(protocol.encode, Arr(Bulk("TOUCH"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using unlink roundtrips successfully given key1") {
    forAll { (k1: Key, nni: NonNegInt) =>
      val protocol = unlink(k1)
      assertEquals(protocol.encode, Arr(Bulk("UNLINK"), Bulk(k1)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using unlink roundtrips successfully given key1, key2") {
    forAll { (k1: Key, k2: Key, nni: NonNegInt) =>
      val protocol = unlink(k1, k2)
      assertEquals(protocol.encode, Arr(Bulk("UNLINK"), Bulk(k1), Bulk(k2)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using unlink roundtrips successfully given key1, key2, key3") {
    forAll { (k1: Key, k2: Key, k3: Key, nni: NonNegInt) =>
      val protocol = unlink(k1, k2, k3)
      assertEquals(protocol.encode, Arr(Bulk("UNLINK"), Bulk(k1), Bulk(k2), Bulk(k3)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using unlink roundtrips successfully given key1, key2, key3, key4") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, nni: NonNegInt) =>
      val protocol = unlink(k1, k2, k3, k4)
      assertEquals(protocol.encode, Arr(Bulk("UNLINK"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
  property("The Key extended protocol using unlink roundtrips successfully given key1, key2, key3, key4, key5") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, nni: NonNegInt) =>
      val protocol = unlink(k1, k2, k3, k4, k5)
      assertEquals(protocol.encode, Arr(Bulk("UNLINK"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }
}
