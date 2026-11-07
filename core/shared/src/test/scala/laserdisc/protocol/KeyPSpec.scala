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

abstract class KeyPSpec extends BaseSpec with KeyP {
  import keytypes.*
  import org.scalacheck.{Arbitrary, Gen}
  import org.scalacheck.Gen.const

  private implicit final val encodingShow: Show[KeyEncoding] = Show.unsafeFromToString

  private implicit final val encodingArb: Arbitrary[KeyEncoding] = Arbitrary {
    Gen.oneOf(
      KeyEncoding.raw,
      KeyEncoding.int,
      KeyEncoding.ziplist,
      KeyEncoding.intset,
      KeyEncoding.hashtable,
      KeyEncoding.skiplist
    )
  }
  private implicit final val restoreEvictionArb: Arbitrary[KeyRestoreEviction] = Arbitrary {
    nonNegIntArb.arbitrary.flatMap(nni => Gen.oneOf(KeyIdleTimeEviction(nni), KeyFrequencyEviction(nni)))
  }
  private implicit final val restoreModeArb: Arbitrary[KeyRestoreMode] = Arbitrary {
    Gen.oneOf(KeyRestoreMode.replace, KeyRestoreMode.absolutettl, KeyRestoreMode.both)
  }
  private implicit final val ttlResponseArb: Arbitrary[KeyTTLResponse] = Arbitrary {
    Gen.oneOf(
      const(KeyNoKeyTTLResponse),
      const(KeyNoExpireTTLResponse),
      nonNegLongArb.arbitrary.map(KeyExpireAfterTTLResponse(_))
    )
  }
  private implicit final val typeArb: Arbitrary[KeyType] = Arbitrary {
    Gen.oneOf(
      KeyType.array,
      KeyType.other(Key("vectorset")),
      KeyType.string,
      KeyType.list,
      KeyType.set,
      KeyType.zset,
      KeyType.hash,
      KeyType.stream
    )
  }

  private final val ttlResponseToNum: KeyTTLResponse => Num = {
    case KeyNoKeyTTLResponse            => Num(-2L)
    case KeyNoExpireTTLResponse         => Num(-1L)
    case KeyExpireAfterTTLResponse(ttl) => Num(ttl.value)
  }
  private final val scanKeyToArr: Scan[Key] => Arr = scanKey =>
    Arr(
      Bulk(scanKey.cursor.value),
      scanKey.values.fold(NilArr: GenArr)(ks => Arr(ks.map(k => Bulk(k.value)).toList))
    )

  protected final val noKeyOrOkToStr: Either[NOKEY, OK] => Str = {
    case Left(_)  => Str(NOKEY.value)
    case Right(_) => Str(OK.value)
  }

  protected implicit final val keyMigrateModeArb: Arbitrary[KeyMigrateMode] = Arbitrary {
    Gen.oneOf(KeyMigrateMode.both, KeyMigrateMode.copy, KeyMigrateMode.replace)
  }
  protected implicit final val nokeyOrOkArb: Arbitrary[Either[NOKEY, OK]] = Arbitrary(
    Gen.oneOf(NOKEY, OK).map {
      case NOKEY => Left(NOKEY)
      case OK    => Right(OK)
      case _     => ???
    }
  )

  property("The Key protocol using del roundtrips successfully given keys") {
    forAll { (ks: OneOrMoreKeys, nni: NonNegInt) =>
      val protocol = del(ks)
      assertEquals(protocol.encode, Arr(Bulk("DEL") :: ks.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Key protocol using dump roundtrips successfully given a key") {
    forAll { (k: Key, os: Option[String]) =>
      val protocol = dump(k)
      assertEquals(protocol.encode, Arr(Bulk("DUMP"), Bulk(k)))
      assertEquals(protocol.decode(os.fold(NullBulk: GenBulk)(Bulk(_))), os.map(Bulk(_)))
    }
  }

  property("The Key protocol using exists roundtrips successfully given keys") {
    forAll { (ks: OneOrMoreKeys, opi: Option[PosInt]) =>
      val protocol = exists(ks)
      assertEquals(protocol.encode, Arr(Bulk("EXISTS") :: ks.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(opi.fold(0L)(_.value.toLong))), opi)
    }
  }

  property("The Key protocol using expire roundtrips successfully given key and expiration") {
    forAll { (k: Key, nni: NonNegInt, b: Boolean) =>
      val protocol = expire(k, nni)
      assertEquals(protocol.encode, Arr(Bulk("EXPIRE"), Bulk(k), Bulk(nni)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  property("The Key protocol using expireat roundtrips successfully given key and expire timestamp") {
    forAll { (k: Key, nni: NonNegInt, b: Boolean) =>
      val protocol = expireat(k, nni)
      assertEquals(protocol.encode, Arr(Bulk("EXPIREAT"), Bulk(k), Bulk(nni)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  property("The Key protocol using keys roundtrips successfully given glob pattern") {
    forAll { (g: GlobPattern, ks: List[Key]) =>
      val protocol = keys(g)
      assertEquals(protocol.encode, Arr(Bulk("KEYS"), Bulk(g)))
      assertEquals(protocol.decode(Arr(ks.map(Bulk(_)))), ks)
    }
  }

  property("The Key protocol using migrate roundtrips successfully given key, host, port, db index and timeout") {
    forAll { (k: Key, h: Host, p: Port, dbi: DbIndex, nni: NonNegInt, nkOrOk: Either[NOKEY, OK]) =>
      val protocol = migrate(k, h, p, dbi, nni)
      assertEquals(protocol.encode, Arr(Bulk("MIGRATE"), Bulk(h), Bulk(p), Bulk(k), Bulk(dbi), Bulk(nni)))
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }

  property("The Key protocol using migrate roundtrips successfully given keys, host, port, db index and timeout") {
    forAll { (ks: TwoOrMoreKeys, h: Host, p: Port, dbi: DbIndex, nni: NonNegInt, nkOrOk: Either[NOKEY, OK]) =>
      val protocol = migrate(ks, h, p, dbi, nni)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") ::
            Bulk(h) ::
            Bulk(p) ::
            Bulk("") ::
            Bulk(dbi) ::
            Bulk(nni) ::
            Bulk("KEYS") ::
            ks.value.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }

  property("The Key protocol using migrate roundtrips successfully given key, host, port, db index, timeout and migrate mode") {
    forAll { (input: (Key, Host, Port, DbIndex, NonNegInt, Either[NOKEY, OK]), mm: KeyMigrateMode) =>
      val (k, h, p, dbi, nni, nkOrOk) = input
      val protocol                    = migrate(k, h, p, dbi, nni, mm)
      assertEquals(
        protocol.encode,
        Arr(Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk(k) :: Bulk(dbi) :: Bulk(nni) :: mm.params.map(Bulk(_)))
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }

  property("The Key protocol using migrate roundtrips successfully given keys, host, port, db index, timeout and migrate mode") {
    forAll { (input: (TwoOrMoreKeys, Host, Port, DbIndex, NonNegInt, Either[NOKEY, OK]), mm: KeyMigrateMode) =>
      val (ks, h, p, dbi, nni, nkOrOk) = input
      val protocol                     = migrate(ks, h, p, dbi, nni, mm)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: mm.params
            .map(Bulk(_)) ::: (Bulk("KEYS") :: ks.value.map(Bulk(_)))
        )
      )
      assertEquals(protocol.decode(noKeyOrOkToStr(nkOrOk)), nkOrOk)
    }
  }

  property("The Key protocol using move roundtrips successfully given key and db") {
    forAll { (k: Key, db: DbIndex, b: Boolean) =>
      val protocol = move(k, db)
      assertEquals(protocol.encode, Arr(Bulk("MOVE"), Bulk(k), Bulk(db)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  property("The Key protocol using obj.encoding roundtrips successfully given key") {
    forAll { (k: Key, oe: Option[KeyEncoding]) =>
      val protocol = obj.encoding(k)
      assertEquals(protocol.encode, Arr(Bulk("OBJECT"), Bulk("ENCODING"), Bulk(k)))
      assertEquals(protocol.decode(oe.fold(NullBulk: GenBulk)(Bulk(_))), oe)
    }
  }

  property("The Key protocol using obj.freq roundtrips successfully given key") {
    forAll { (k: Key, nni: NonNegInt) =>
      val protocol = obj.freq(k)
      assertEquals(protocol.encode, Arr(Bulk("OBJECT"), Bulk("FREQ"), Bulk(k)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Key protocol using obj.idletime roundtrips successfully given key") {
    forAll { (k: Key, nni: NonNegInt) =>
      val protocol = obj.idletime(k)
      assertEquals(protocol.encode, Arr(Bulk("OBJECT"), Bulk("IDLETIME"), Bulk(k)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Key protocol using obj.refcount roundtrips successfully given key") {
    forAll { (k: Key, nni: NonNegInt) =>
      val protocol = obj.refcount(k)
      assertEquals(protocol.encode, Arr(Bulk("OBJECT"), Bulk("REFCOUNT"), Bulk(k)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Key protocol using persist roundtrips successfully given key") {
    forAll { (k: Key, b: Boolean) =>
      val protocol = persist(k)
      assertEquals(protocol.encode, Arr(Bulk("PERSIST"), Bulk(k)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  property("The Key protocol using pexpire roundtrips successfully given key") {
    forAll { (k: Key, nnl: NonNegLong, b: Boolean) =>
      val protocol = pexpire(k, nnl)
      assertEquals(protocol.encode, Arr(Bulk("PEXPIRE"), Bulk(k), Bulk(nnl)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  property("The Key protocol using pexpireat roundtrips successfully given key") {
    forAll { (k: Key, nnl: NonNegLong, b: Boolean) =>
      val protocol = pexpireat(k, nnl)
      assertEquals(protocol.encode, Arr(Bulk("PEXPIREAT"), Bulk(k), Bulk(nnl)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  property("The Key protocol using pttl roundtrips successfully given key") {
    forAll { (k: Key, ttl: KeyTTLResponse) =>
      val protocol = pttl(k)
      assertEquals(protocol.encode, Arr(Bulk("PTTL"), Bulk(k)))
      assertEquals(protocol.decode(ttlResponseToNum(ttl)), ttl)
    }
  }

  property("The Key protocol using randomkey roundtrips successfully using val") {
    forAll { (ok: Option[Key]) =>
      val protocol = randomkey
      assertEquals(protocol.encode, Arr(Bulk("RANDOMKEY")))
      assertEquals(protocol.decode(ok.fold(NullBulk: GenBulk)(Bulk(_))), ok)
    }
  }

  property("The Key protocol using rename roundtrips successfully given old key and new key") {
    forAll { (ok: Key, nk: Key) =>
      val protocol = rename(ok, nk)
      assertEquals(protocol.encode, Arr(Bulk("RENAME"), Bulk(ok), Bulk(nk)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Key protocol using renamenx roundtrips successfully given old key and new key") {
    forAll { (ok: Key, nk: Key, b: Boolean) =>
      val protocol = renamenx(ok, nk)
      assertEquals(protocol.encode, Arr(Bulk("RENAMENX"), Bulk(ok), Bulk(nk)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  property("The Key protocol using restore roundtrips successfully given key, ttl and serialized value") {
    forAll { (k: Key, nnl: NonNegLong, s: String) =>
      val protocol = restore(k, nnl, Bulk(s))
      assertEquals(protocol.encode, Arr(Bulk("RESTORE"), Bulk(k), Bulk(nnl), Bulk(s)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Key protocol using restore roundtrips successfully given key, ttl, serialized value and mode") {
    forAll { (k: Key, nnl: NonNegLong, s: String, m: KeyRestoreMode) =>
      val protocol = restore(k, nnl, Bulk(s), m)
      assertEquals(protocol.encode, Arr(Bulk("RESTORE") :: Bulk(k) :: Bulk(nnl) :: Bulk(s) :: m.params.map(Bulk(_))))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Key protocol using restore roundtrips successfully given key, ttl, serialized value and eviction") {
    forAll { (k: Key, nnl: NonNegLong, s: String, e: KeyRestoreEviction) =>
      val protocol = restore(k, nnl, Bulk(s), e)
      assertEquals(protocol.encode, Arr(Bulk("RESTORE"), Bulk(k), Bulk(nnl), Bulk(s), Bulk(e.param), Bulk(e.seconds)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Key protocol using restore sends IDLETIME and FREQ for the eviction") {
    forAll { (k: Key, nnl: NonNegLong, nni: NonNegInt) =>
      assertEquals(
        restore(k, nnl, Bulk("v"), KeyIdleTimeEviction(nni)).encode,
        Arr(Bulk("RESTORE"), Bulk(k), Bulk(nnl), Bulk("v"), Bulk("IDLETIME"), Bulk(nni))
      )
      assertEquals(
        restore(k, nnl, Bulk("v"), KeyFrequencyEviction(nni)).encode,
        Arr(Bulk("RESTORE"), Bulk(k), Bulk(nnl), Bulk("v"), Bulk("FREQ"), Bulk(nni))
      )
    }
  }

  property("The Key protocol using restore roundtrips successfully given key, ttl, serialized value, mode and eviction") {
    forAll { (k: Key, nnl: NonNegLong, s: String, m: KeyRestoreMode, e: KeyRestoreEviction) =>
      val protocol = restore(k, nnl, Bulk(s), m, e)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("RESTORE") :: Bulk(k) :: Bulk(nnl) :: Bulk(s) :: m.params.map(Bulk(_)) ::: (Bulk(e.param) :: Bulk(e.seconds) :: Nil)
        )
      )
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Key protocol using scan roundtrips successfully given cursor") {
    forAll { (nnl: NonNegLong, sk: Scan[Key]) =>
      val protocol = scan(nnl)
      assertEquals(protocol.encode, Arr(Bulk("SCAN"), Bulk(nnl)))
      assertEquals(protocol.decode(scanKeyToArr(sk)), sk)
    }
  }

  property("The Key protocol using scan roundtrips successfully given cursor and glob pattern") {
    forAll { (nnl: NonNegLong, g: GlobPattern, sk: Scan[Key]) =>
      val protocol = scan(nnl, g)
      assertEquals(protocol.encode, Arr(Bulk("SCAN"), Bulk(nnl), Bulk("MATCH"), Bulk(g)))
      assertEquals(protocol.decode(scanKeyToArr(sk)), sk)
    }
  }

  property("The Key protocol using scan roundtrips successfully given cursor and count") {
    forAll { (nnl: NonNegLong, pi: PosInt, sk: Scan[Key]) =>
      val protocol = scan(nnl, pi)
      assertEquals(protocol.encode, Arr(Bulk("SCAN"), Bulk(nnl), Bulk("COUNT"), Bulk(pi)))
      assertEquals(protocol.decode(scanKeyToArr(sk)), sk)
    }
  }

  property("The Key protocol using scan roundtrips successfully given cursor, glob pattern and count") {
    forAll { (nnl: NonNegLong, g: GlobPattern, pi: PosInt, sk: Scan[Key]) =>
      val protocol = scan(nnl, g, pi)
      assertEquals(protocol.encode, Arr(Bulk("SCAN"), Bulk(nnl), Bulk("MATCH"), Bulk(g), Bulk("COUNT"), Bulk(pi)))
      assertEquals(protocol.decode(scanKeyToArr(sk)), sk)
    }
  }

  property("The Key protocol using touch roundtrips successfully given keys") {
    forAll { (ks: OneOrMoreKeys, nni: NonNegInt) =>
      val protocol = touch(ks)
      assertEquals(protocol.encode, Arr(Bulk("TOUCH") :: ks.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Key protocol using ttl roundtrips successfully given key") {
    forAll { (k: Key, ttl0: KeyTTLResponse) =>
      val protocol = ttl(k)
      assertEquals(protocol.encode, Arr(Bulk("TTL"), Bulk(k)))
      assertEquals(protocol.decode(ttlResponseToNum(ttl0)), ttl0)
    }
  }

  property("The Key protocol using typeof roundtrips successfully given key") {
    forAll { (k: Key, ot: Option[KeyType]) =>
      val protocol = typeof(k)
      assertEquals(protocol.encode, Arr(Bulk("TYPE"), Bulk(k)))
      assertEquals(protocol.decode(ot.fold(Str("none"))(Str(_))), ot)
    }
  }

  property("The Key protocol using unlink roundtrips successfully given keys") {
    forAll { (ks: OneOrMoreKeys, nni: NonNegInt) =>
      val protocol = unlink(ks)
      assertEquals(protocol.encode, Arr(Bulk("UNLINK") :: ks.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Key protocol using wait roundtrips successfully given replicas and timeout") {
    forAll { (pi1: PosInt, pl: PosLong, pi2: PosInt) =>
      val protocol = wait(pi1, pl)
      assertEquals(protocol.encode, Arr(Bulk("WAIT"), Bulk(pi1), Bulk(pl)))
      assertEquals(protocol.decode(Num(pi2.value.toLong)), pi2)
    }
  }

  property("The Key protocol using copy roundtrips successfully given source, destination, db and REPLACE") {
    forAll { (s: Key, d: Key, db: DbIndex, b: Boolean) =>
      val protocol = copy(s, d, db, KeyReplace)
      assertEquals(protocol.encode, Arr(Bulk("COPY"), Bulk(s), Bulk(d), Bulk("DB"), Bulk(db), Bulk("REPLACE")))
      assertEquals(protocol.decode(Num(if (b) 1L else 0L)), b)
    }
  }

  property("The Key protocol using migrate sends AUTH2 after the mode and before KEYS") {
    forAll { (ks: TwoOrMoreKeys, h: Host, p: Port, db: DbIndex, t: NonNegInt, u: String, pw: String) =>
      val protocol = migrate(ks, h, p, db, t, KeyMigrateMode.both, KeyMigrateAuth.userAndPassword(u, pw))
      assertEquals(
        protocol.encode,
        Arr(
          List(
            Bulk("MIGRATE"),
            Bulk(h),
            Bulk(p),
            Bulk(""),
            Bulk(db),
            Bulk(t),
            Bulk("COPY"),
            Bulk("REPLACE"),
            Bulk("AUTH2"),
            Bulk(u),
            Bulk(pw),
            Bulk("KEYS")
          ) :::
            ks.value.map(Bulk(_))
        )
      )
      assertEquals(protocol.decode(Str("NOKEY")), Left(NOKEY))
    }
  }

  property("The Key protocol using scan roundtrips successfully given cursor, glob pattern and type") {
    forAll { (nnl: NonNegLong, g: GlobPattern, t: KeyType, sk: Scan[Key]) =>
      val protocol = scan(nnl, g, t)
      assertEquals(protocol.encode, Arr(Bulk("SCAN"), Bulk(nnl), Bulk("MATCH"), Bulk(g), Bulk("TYPE"), Bulk(t)))
      assertEquals(protocol.decode(scanKeyToArr(sk)), sk)
    }
  }

  property("The Key protocol using scan roundtrips successfully given cursor, glob pattern, count and type") {
    forAll { (nnl: NonNegLong, g: GlobPattern, pi: PosInt, t: KeyType, sk: Scan[Key]) =>
      val protocol = scan(nnl, g, pi, t)
      assertEquals(protocol.encode, Arr(Bulk("SCAN"), Bulk(nnl), Bulk("MATCH"), Bulk(g), Bulk("COUNT"), Bulk(pi), Bulk("TYPE"), Bulk(t)))
      assertEquals(protocol.decode(scanKeyToArr(sk)), sk)
    }
  }

  property("The Key protocol using typeof reads a stream, an array, a type of a module, and none as no key") {
    forAll { (k: Key) =>
      assertEquals(typeof(k).decode(Str("stream")), Some(KeyType.stream))
      assertEquals(typeof(k).decode(Str("array")), Some(KeyType.array))
      assertEquals(typeof(k).decode(Str("ReJSON-RL")), Some(KeyType.other(Key("ReJSON-RL"))))
      assertEquals(typeof(k).decode(Str("none")), None)
    }
  }

  property("The Key protocol using sort with GET reads a value or None for each pattern") {
    forAll { (k: Key, by: Key, d: Key, o: NonNegLong, c: PosLong, ovs: List[Option[String]]) =>
      val options  = KeySort.by(by).limit(o, c).desc.alpha
      val protocol = sort[String](k, options, KeySortGet(Key("#"), Key("h_*->f")))
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("SORT"),
          Bulk(k),
          Bulk("BY"),
          Bulk(by),
          Bulk("LIMIT"),
          Bulk(o),
          Bulk(c),
          Bulk("DESC"),
          Bulk("ALPHA"),
          Bulk("GET"),
          Bulk("#"),
          Bulk("GET"),
          Bulk("h_*->f")
        )
      )
      assertEquals(protocol.decode(Arr(ovs.map(_.fold(NullBulk: GenBulk)(Bulk(_))))), ovs)
      assertEquals(sort(k, KeySort.noSort, d).encode, Arr(Bulk("SORT"), Bulk(k), Bulk("BY"), Bulk("nosort"), Bulk("STORE"), Bulk(d)))
    }
  }

  property("The Key protocol using expire with a condition sends it after the seconds") {
    forAll { (k: Key, s: NonNegInt, b: Boolean) =>
      val protocol = expire(k, s, KeyExpireCondition.xxGt)
      assertEquals(protocol.encode, Arr(Bulk("EXPIRE"), Bulk(k), Bulk(s), Bulk("XX"), Bulk("GT")))
      assertEquals(protocol.decode(Num(if (b) 1L else 0L)), b)
    }
  }

  property("The Key protocol using pexpiretime reads no key, no expiry or the timestamp") {
    forAll { (k: Key, ts: NonNegLong) =>
      val protocol = pexpiretime(k)
      assertEquals(protocol.encode, Arr(Bulk("PEXPIRETIME"), Bulk(k)))
      assertEquals(protocol.decode(Num(-2L)), KeyNoKeyTTLResponse)
      assertEquals(protocol.decode(Num(-1L)), KeyNoExpireTTLResponse)
      assertEquals(protocol.decode(Num(ts.value)), KeyExpireAt(ts))
    }
  }

  property("The Key protocol using waitaof reads how many it reached locally and on replicas") {
    forAll { (l: NonNegInt, r: NonNegInt, t: NonNegLong) =>
      val protocol = waitaof(l, r, t)
      assertEquals(protocol.encode, Arr(Bulk("WAITAOF"), Bulk(l), Bulk(r), Bulk(t)))
      assertEquals(protocol.decode(Arr(Num(l.value.toLong), Num(r.value.toLong))), l -> r)
    }
  }
}
