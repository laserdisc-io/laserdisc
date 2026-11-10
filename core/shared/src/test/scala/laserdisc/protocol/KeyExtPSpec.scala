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

final class KeyExtPSpec extends KeyPSpec with ExtPArguments {
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

  //format: off
  private val (host, port, db, timeout) = (Host("localhost"), Port(6379), DbIndex(3), NonNegInt(5))
  private val mode: KeyP.MigrateMode    = KeyP.MigrateMode.both

  test("The Key extended protocol using del sends a list of keys passed as arguments") {
    val keys     = (2 to 30).map(i => Key.unsafeFrom(s"k$i"))
    val expected = List("DEL", "k1") ++ keys.map(_.value)
    assertEquals(del(k1, keys*).encode, frame(expected*))
  }
  test("The Key extended protocol using del sends in order any number of keys") {
    assertEquals(del(k1).encode, frame("DEL", "k1"))
    assertEquals(del(k1, k2).encode, frame("DEL", "k1", "k2"))
    assertEquals(del(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).encode, frame("DEL", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22"))
  }
  test("The Key extended protocol using exists sends in order any number of keys") {
    assertEquals(exists(k1).encode, frame("EXISTS", "k1"))
    assertEquals(exists(k1, k2).encode, frame("EXISTS", "k1", "k2"))
    assertEquals(exists(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).encode, frame("EXISTS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22"))
  }
  test("The Key extended protocol using touch sends in order any number of keys") {
    assertEquals(touch(k1).encode, frame("TOUCH", "k1"))
    assertEquals(touch(k1, k2).encode, frame("TOUCH", "k1", "k2"))
    assertEquals(touch(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).encode, frame("TOUCH", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22"))
  }
  test("The Key extended protocol using unlink sends in order any number of keys") {
    assertEquals(unlink(k1).encode, frame("UNLINK", "k1"))
    assertEquals(unlink(k1, k2).encode, frame("UNLINK", "k1", "k2"))
    assertEquals(unlink(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).encode, frame("UNLINK", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22"))
  }
  test("The Key extended protocol using migrate sends in order from 2 to 18 keys") {
    assertEquals(migrate(k1, k2, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2"))
    assertEquals(migrate(k1, k2, k3, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3"))
    assertEquals(migrate(k1, k2, k3, k4, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4"))
    assertEquals(migrate(k1, k2, k3, k4, k5, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, host, port, db, timeout).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
  }
  test("The Key extended protocol using migrate sends in order from 2 to 17 keys, with a mode") {
    assertEquals(migrate(k1, k2, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2"))
    assertEquals(migrate(k1, k2, k3, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3"))
    assertEquals(migrate(k1, k2, k3, k4, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4"))
    assertEquals(migrate(k1, k2, k3, k4, k5, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(migrate(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, host, port, db, timeout, mode).encode, frame("MIGRATE", "localhost", "6379", "", "3", "5", "COPY", "REPLACE", "KEYS", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
  }
}
