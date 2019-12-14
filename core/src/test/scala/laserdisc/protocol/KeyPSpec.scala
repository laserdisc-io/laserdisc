package laserdisc
package protocol

final class KeyPSpec extends KeyExtPSpec {
  import keytypes._
  import org.scalacheck.{Arbitrary, Gen}
  import org.scalacheck.Gen.const

  private[this] implicit final val encodingShow: Show[KeyEncoding] = Show.unsafeFromToString
  private[this] implicit final val typeShow: Show[KeyType]         = Show.unsafeFromToString

  private[this] implicit final val encodingArb: Arbitrary[KeyEncoding] = Arbitrary {
    Gen.oneOf(
      KeyEncoding.raw,
      KeyEncoding.int,
      KeyEncoding.ziplist,
      KeyEncoding.intset,
      KeyEncoding.hashtable,
      KeyEncoding.skiplist
    )
  }
  private[this] implicit final val restoreEvictionArb: Arbitrary[KeyRestoreEviction] = Arbitrary {
    nonNegIntArb.arbitrary.flatMap(nni => Gen.oneOf(KeyIdleTimeEviction(nni), KeyFrequencyEviction(nni)))
  }
  private[this] implicit final val restoreModeArb: Arbitrary[KeyRestoreMode] = Arbitrary {
    Gen.oneOf(KeyRestoreMode.replace, KeyRestoreMode.absolutettl, KeyRestoreMode.both)
  }
  private[this] implicit final val ttlResponseArb: Arbitrary[KeyTTLResponse] = Arbitrary {
    Gen.oneOf(
      const(KeyNoKeyTTLResponse),
      const(KeyNoExpireTTLResponse),
      nonNegLongArb.arbitrary.map(KeyExpireAfterTTLResponse(_))
    )
  }
  private[this] implicit final val typeArb: Arbitrary[KeyType] = Arbitrary {
    Gen.oneOf(KeyType.string, KeyType.list, KeyType.set, KeyType.zset, KeyType.hash)
  }

  private[this] final val ttlResponseToNum: KeyTTLResponse => Num = {
    case KeyNoKeyTTLResponse            => Num(-2L)
    case KeyNoExpireTTLResponse         => Num(-1L)
    case KeyExpireAfterTTLResponse(ttl) => Num(ttl.value)
  }
  private[this] final val scanKeyToArr: Scan[Key] => Arr = scanKey =>
    Arr(
      Bulk(scanKey.cursor.value),
      scanKey.values.fold(NilArr: GenArr)(ks => Arr(ks.map(k => Bulk(k.value)).toList))
    )

  "The Key protocol" when {
    "using del" should {
      "roundtrip successfully" when {
        "given keys" in forAll("keys", "deleted") { (ks: OneOrMoreKeys, nni: NonNegInt) =>
          val protocol = del(ks)

          protocol.encode shouldBe Arr(Bulk("DEL") :: ks.value.map(Bulk(_)))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using dump" should {
      "roundtrip successfully" when {
        "given a key" in forAll("key", "dump") { (k: Key, os: Option[String]) =>
          val protocol = dump(k)

          protocol.encode shouldBe Arr(Bulk("DUMP"), Bulk(k))
          protocol.decode(os.fold(NullBulk: GenBulk)(Bulk(_))) onRight (_ shouldBe os.map(Bulk(_)))
        }
      }
    }

    "using exists" should {
      "roundtrip successfully" when {
        "given keys" in forAll("keys", "exists") { (ks: OneOrMoreKeys, opi: Option[PosInt]) =>
          val protocol = exists(ks)

          protocol.encode shouldBe Arr(Bulk("EXISTS") :: ks.value.map(Bulk(_)))
          protocol.decode(Num(opi.fold(0L)(_.value.toLong))) onRight (_ shouldBe opi)
        }
      }
    }

    "using expire" should {
      "roundtrip successfully" when {
        "given key and expiration" in forAll("key", "expiration", "expired") { (k: Key, nni: NonNegInt, b: Boolean) =>
          val protocol = expire(k, nni)

          protocol.encode shouldBe Arr(Bulk("EXPIRE"), Bulk(k), Bulk(nni))
          protocol.decode(boolToNum(b)) onRight (_ shouldBe b)
        }
      }
    }

    "using expireat" should {
      "roundtrip successfully" when {
        "given key and expire timestamp" in forAll("key", "timestamp", "expired") { (k: Key, nni: NonNegInt, b: Boolean) =>
          val protocol = expireat(k, nni)

          protocol.encode shouldBe Arr(Bulk("EXPIREAT"), Bulk(k), Bulk(nni))
          protocol.decode(boolToNum(b)) onRight (_ shouldBe b)
        }
      }
    }

    "using keys" should {
      "roundtrip successfully" when {
        "given glob pattern" in forAll("glob pattern", "keys") { (g: GlobPattern, ks: List[Key]) =>
          val protocol = keys(g)

          protocol.encode shouldBe Arr(Bulk("KEYS"), Bulk(g))
          protocol.decode(Arr(ks.map(Bulk(_)))) onRight (_ shouldBe ks)
        }
      }
    }

    "using migrate" should {
      "roundtrip successfully" when {
        "given key, host, port, db index and timeout" in forAll("key", "host", "port", "db index", "timeout", "response") {
          (k: Key, h: Host, p: Port, dbi: DbIndex, nni: NonNegInt, nkOrOk: NOKEY | OK) =>
            val protocol = migrate(k, h, p, dbi, nni)

            protocol.encode shouldBe Arr(Bulk("MIGRATE"), Bulk(h), Bulk(p), Bulk(k), Bulk(dbi), Bulk(nni))
            protocol.decode(noKeyOrOkToStr(nkOrOk)) onRight (_ shouldBe nkOrOk)
        }
        "given keys, host, port, db index and timeout" in forAll("keys", "host", "port", "db index", "timeout", "response") {
          (ks: TwoOrMoreKeys, h: Host, p: Port, dbi: DbIndex, nni: NonNegInt, nkOrOk: NOKEY | OK) =>
            val protocol = migrate(ks, h, p, dbi, nni)

            protocol.encode shouldBe Arr(
              Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: Bulk("KEYS") :: ks.value.map(Bulk(_))
            )
            protocol.decode(noKeyOrOkToStr(nkOrOk)) onRight (_ shouldBe nkOrOk)
        }
        "given key, host, port, db index, timeout and migrate mode" in {
          forAll("key, host, port, db index, timeout, response", "migrate mode") {
            (input: (Key, Host, Port, DbIndex, NonNegInt, NOKEY | OK), mm: KeyMigrateMode) =>
              val (k, h, p, dbi, nni, nkOrOk) = input
              val protocol                    = migrate(k, h, p, dbi, nni, mm)

              protocol.encode shouldBe Arr(
                Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk(k) :: Bulk(dbi) :: Bulk(nni) :: mm.params.map(Bulk(_))
              )
              protocol.decode(noKeyOrOkToStr(nkOrOk)) onRight (_ shouldBe nkOrOk)
          }
        }
        "given keys, host, port, db index, timeout and migrate mode" in {
          forAll("keys, host, port, db index, timeout, response", "migrate mode") {
            (input: (TwoOrMoreKeys, Host, Port, DbIndex, NonNegInt, NOKEY | OK), mm: KeyMigrateMode) =>
              val (ks, h, p, dbi, nni, nkOrOk) = input
              val protocol                     = migrate(ks, h, p, dbi, nni, mm)

              protocol.encode shouldBe Arr(
                Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) ::
                  mm.params.map(Bulk(_)) ::: (Bulk("KEYS") :: ks.value.map(Bulk(_)))
              )
              protocol.decode(noKeyOrOkToStr(nkOrOk)) onRight (_ shouldBe nkOrOk)
          }
        }
      }
    }

    "using move" should {
      "roundtrip successfully" when {
        "given key and db" in forAll("key", "db", "moved") { (k: Key, db: DbIndex, b: Boolean) =>
          val protocol = move(k, db)

          protocol.encode shouldBe Arr(Bulk("MOVE"), Bulk(k), Bulk(db))
          protocol.decode(boolToNum(b)) onRight (_ shouldBe b)
        }
      }
    }

    "using obj.encoding" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "encoding") { (k: Key, oe: Option[KeyEncoding]) =>
          val protocol = obj.encoding(k)

          protocol.encode shouldBe Arr(Bulk("OBJECT"), Bulk("ENCODING"), Bulk(k))
          protocol.decode(oe.fold(NullBulk: GenBulk)(Bulk(_))) onRight (_ shouldBe oe)
        }
      }
    }

    "using obj.freq" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "frequency") { (k: Key, nni: NonNegInt) =>
          val protocol = obj.freq(k)

          protocol.encode shouldBe Arr(Bulk("OBJECT"), Bulk("FREQ"), Bulk(k))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using obj.idletime" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "idle time") { (k: Key, nni: NonNegInt) =>
          val protocol = obj.idletime(k)

          protocol.encode shouldBe Arr(Bulk("OBJECT"), Bulk("IDLETIME"), Bulk(k))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using obj.refcount" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "ref count") { (k: Key, nni: NonNegInt) =>
          val protocol = obj.refcount(k)

          protocol.encode shouldBe Arr(Bulk("OBJECT"), Bulk("REFCOUNT"), Bulk(k))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using persist" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "persisted") { (k: Key, b: Boolean) =>
          val protocol = persist(k)

          protocol.encode shouldBe Arr(Bulk("PERSIST"), Bulk(k))
          protocol.decode(boolToNum(b)) onRight (_ shouldBe b)
        }
      }
    }

    "using pexpire" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "timeout", "persisted") { (k: Key, nnl: NonNegLong, b: Boolean) =>
          val protocol = pexpire(k, nnl)

          protocol.encode shouldBe Arr(Bulk("PEXPIRE"), Bulk(k), Bulk(nnl))
          protocol.decode(boolToNum(b)) onRight (_ shouldBe b)
        }
      }
    }

    "using pexpireat" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "timestamp", "persisted") { (k: Key, nnl: NonNegLong, b: Boolean) =>
          val protocol = pexpireat(k, nnl)

          protocol.encode shouldBe Arr(Bulk("PEXPIREAT"), Bulk(k), Bulk(nnl))
          protocol.decode(boolToNum(b)) onRight (_ shouldBe b)
        }
      }
    }

    "using pttl" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "ttl") { (k: Key, ttl: KeyTTLResponse) =>
          val protocol = pttl(k)

          protocol.encode shouldBe Arr(Bulk("PTTL"), Bulk(k))
          protocol.decode(ttlResponseToNum(ttl)) onRight (_ shouldBe ttl)
        }
      }
    }

    "using randomkey" should {
      "roundtrip successfully" when {
        "using val" in { (ok: Option[Key]) =>
          val protocol = randomkey

          protocol.encode shouldBe Arr(Bulk("RANDOMKEY"))
          protocol.decode(ok.fold(NullBulk: GenBulk)(Bulk(_))) onRight (_ shouldBe ok)
        }
      }
    }

    "using rename" should {
      "roundtrip successfully" when {
        "given old key and new key" in forAll("old key", "new key") { (ok: Key, nk: Key) =>
          val protocol = rename(ok, nk)

          protocol.encode shouldBe Arr(Bulk("RENAME"), Bulk(ok), Bulk(nk))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using renamenx" should {
      "roundtrip successfully" when {
        "given old key and new key" in forAll("old key", "new key", "renamed") { (ok: Key, nk: Key, b: Boolean) =>
          val protocol = renamenx(ok, nk)

          protocol.encode shouldBe Arr(Bulk("RENAMENX"), Bulk(ok), Bulk(nk))
          protocol.decode(boolToNum(b)) onRight (_ shouldBe b)
        }
      }
    }

    "using restore" should {
      "roundtrip successfully" when {
        "given key, ttl and serialized value" in forAll("key", "ttl", "serialized value") { (k: Key, nnl: NonNegLong, s: String) =>
          val protocol = restore(k, nnl, Bulk(s))

          protocol.encode shouldBe Arr(Bulk("RESTORE"), Bulk(k), Bulk(nnl), Bulk(s))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
        "given key, ttl, serialized value and mode" in {
          forAll("key", "ttl", "serialized value", "mode") { (k: Key, nnl: NonNegLong, s: String, m: KeyRestoreMode) =>
            val protocol = restore(k, nnl, Bulk(s), m)

            protocol.encode shouldBe Arr(Bulk("RESTORE") :: Bulk(k) :: Bulk(nnl) :: Bulk(s) :: m.params.map(Bulk(_)))
            protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
          }
        }
        "given key, ttl, serialized value and eviction" in {
          forAll("key", "ttl", "serialized value", "eviction") { (k: Key, nnl: NonNegLong, s: String, e: KeyRestoreEviction) =>
            val protocol = restore(k, nnl, Bulk(s), e)

            protocol.encode shouldBe Arr(Bulk("RESTORE"), Bulk(k), Bulk(nnl), Bulk(s), Bulk(e.param), Bulk(e.seconds))
            protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
          }
        }
        "given key, ttl, serialized value, mode and eviction" in forAll("key", "ttl", "serialized value", "mode", "eviction") {
          (k: Key, nnl: NonNegLong, s: String, m: KeyRestoreMode, e: KeyRestoreEviction) =>
            val protocol = restore(k, nnl, Bulk(s), m, e)

            protocol.encode shouldBe Arr(
              Bulk("RESTORE") :: Bulk(k) :: Bulk(nnl) :: Bulk(s) :: m.params.map(Bulk(_)) ::: (Bulk(e.param) :: Bulk(e.seconds) :: Nil)
            )
            protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using scan" should {
      "roundtrip successfully" when {
        "given cursor" in forAll("cursor", "scan result") { (nnl: NonNegLong, sk: Scan[Key]) =>
          val protocol = scan(nnl)

          protocol.encode shouldBe Arr(Bulk("SCAN"), Bulk(nnl))
          protocol.decode(scanKeyToArr(sk)) onRight (_ shouldBe sk)
        }
        "given cursor and glob pattern" in {
          forAll("cursor", "glob pattern", "scan result") { (nnl: NonNegLong, g: GlobPattern, sk: Scan[Key]) =>
            val protocol = scan(nnl, g)

            protocol.encode shouldBe Arr(Bulk("SCAN"), Bulk(nnl), Bulk("MATCH"), Bulk(g))
            protocol.decode(scanKeyToArr(sk)) onRight (_ shouldBe sk)
          }
        }
        "given cursor and count" in {
          forAll("cursor", "count", "scan result") { (nnl: NonNegLong, pi: PosInt, sk: Scan[Key]) =>
            val protocol = scan(nnl, pi)

            protocol.encode shouldBe Arr(Bulk("SCAN"), Bulk(nnl), Bulk("COUNT"), Bulk(pi))
            protocol.decode(scanKeyToArr(sk)) onRight (_ shouldBe sk)
          }
        }
        "given cursor, glob pattern and count" in forAll("cursor", "glob pattern", "count", "scan result") {
          (nnl: NonNegLong, g: GlobPattern, pi: PosInt, sk: Scan[Key]) =>
            val protocol = scan(nnl, g, pi)

            protocol.encode shouldBe Arr(Bulk("SCAN"), Bulk(nnl), Bulk("MATCH"), Bulk(g), Bulk("COUNT"), Bulk(pi))
            protocol.decode(scanKeyToArr(sk)) onRight (_ shouldBe sk)
        }
      }
    }

    //FIXME add SORT

    "using touch" should {
      "roundtrip successfully" when {
        "given keys" in forAll("keys", "touched") { (ks: OneOrMoreKeys, nni: NonNegInt) =>
          val protocol = touch(ks)

          protocol.encode shouldBe Arr(Bulk("TOUCH") :: ks.value.map(Bulk(_)))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using ttl" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "ttl") { (k: Key, ttl0: KeyTTLResponse) =>
          val protocol = ttl(k)

          protocol.encode shouldBe Arr(Bulk("TTL"), Bulk(k))
          protocol.decode(ttlResponseToNum(ttl0)) onRight (_ shouldBe ttl0)
        }
      }
    }

    "using typeof" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "type") { (k: Key, ot: Option[KeyType]) =>
          val protocol = typeof(k)

          protocol.encode shouldBe Arr(Bulk("TYPE"), Bulk(k))
          protocol.decode(ot.fold(Str("none"))(Str(_))) onRight (_ shouldBe ot)
        }
      }
    }

    "using unlink" should {
      "roundtrip successfully" when {
        "given keys" in forAll("keys", "unlinked") { (ks: OneOrMoreKeys, nni: NonNegInt) =>
          val protocol = unlink(ks)

          protocol.encode shouldBe Arr(Bulk("UNLINK") :: ks.value.map(Bulk(_)))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using wait" should {
      "roundtrip successfully" when {
        "given replicas" in forAll("replicas", "acknowledgements") { (pi1: PosInt, pi2: PosInt) =>
          val protocol = wait(pi1)

          protocol.encode shouldBe Arr(Bulk("WAIT"), Bulk(pi1), Bulk(0))
          protocol.decode(Num(pi2.value.toLong)) onRight (_ shouldBe pi2)
        }
        "given replicas and timeout" in forAll("replicas", "timeout", "acknowledgements") { (pi1: PosInt, pl: PosLong, pi2: PosInt) =>
          val protocol = wait(pi1, pl)

          protocol.encode shouldBe Arr(Bulk("WAIT"), Bulk(pi1), Bulk(pl))
          protocol.decode(Num(pi2.value.toLong)) onRight (_ shouldBe pi2)
        }
      }
    }
  }
}
