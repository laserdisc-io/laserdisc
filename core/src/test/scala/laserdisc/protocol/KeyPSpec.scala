package laserdisc
package protocol

final class KeyPSpec extends KeyExtPSpec {
  import keytypes._
  import org.scalacheck.{Arbitrary, Gen}

  private[this] implicit final val encodingShow: Show[KeyEncoding] = Show.unsafeFromToString

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

  "The Key protocol" when {

    "using del" should {

      "roundtrip successfully" when {
        "given keys" in forAll("keys", "deleted") { (ks: OneOrMoreKeys, nni: NonNegInt) =>
          val protocol = del(ks)

          protocol.encode shouldBe Arr(Bulk("DEL") :: ks.value.map(Bulk(_)))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
      }
    }

    "using dump" should {

      "roundtrip successfully" when {
        "given a key" in forAll("key", "dump") { (k: Key, os: Option[String]) =>
          val protocol = dump(k)

          protocol.encode shouldBe Arr(Bulk("DUMP"), Bulk(k))
          protocol.decode(os.fold(NullBulk: GenBulk)(Bulk(_))).right.value shouldBe os.map(Bulk(_))
        }
      }
    }

    "using exists" should {

      "roundtrip successfully" when {
        "given keys" in forAll("keys", "exists") { (ks: OneOrMoreKeys, opi: Option[PosInt]) =>
          val protocol = exists(ks)

          protocol.encode shouldBe Arr(Bulk("EXISTS") :: ks.value.map(Bulk(_)))
          protocol.decode(Num(opi.fold(0L)(_.value.toLong))).right.value shouldBe opi
        }
      }
    }

    "using expire" should {

      "roundtrip successfully" when {
        "given key and expiration" in forAll("key", "expiration", "expired") { (k: Key, nni: NonNegInt, b: Boolean) =>
          val protocol = expire(k, nni)

          protocol.encode shouldBe Arr(Bulk("EXPIRE"), Bulk(k), Bulk(nni))
          protocol.decode(boolToNum(b)).right.value shouldBe b
        }
      }
    }

    "using expireat" should {

      "roundtrip successfully" when {
        "given key and expire timestamp" in forAll("key", "timestamp", "expired") { (k: Key, nni: NonNegInt, b: Boolean) =>
          val protocol = expireat(k, nni)

          protocol.encode shouldBe Arr(Bulk("EXPIREAT"), Bulk(k), Bulk(nni))
          protocol.decode(boolToNum(b)).right.value shouldBe b
        }
      }
    }

    "using keys" should {

      "roundtrip successfully" when {
        "given glob pattern" in forAll("glob pattern", "keys") { (g: GlobPattern, ks: List[Key]) =>
          val protocol = keys(g)

          protocol.encode shouldBe Arr(Bulk("KEYS"), Bulk(g))
          protocol.decode(Arr(ks.map(Bulk(_)))).right.value shouldBe ks
        }
      }
    }

    "using migrate" should {

      "roundtrip successfully" when {
        "given key, host, port, db index and timeout" in forAll("key", "host", "port", "db index", "timeout", "response") {
          (k: Key, h: Host, p: Port, dbi: DbIndex, nni: NonNegInt, nkOrOk: NOKEY | OK) =>
            val protocol = migrate(k, h, p, dbi, nni)

            protocol.encode shouldBe Arr(Bulk("MIGRATE"), Bulk(h), Bulk(p), Bulk(k), Bulk(dbi), Bulk(nni))
            protocol.decode(noKeyOrOkToStr(nkOrOk)).right.value shouldBe nkOrOk
        }
        "given keys, host, port, db index and timeout" in forAll("keys", "host", "port", "db index", "timeout", "response") {
          (ks: TwoOrMoreKeys, h: Host, p: Port, dbi: DbIndex, nni: NonNegInt, nkOrOk: NOKEY | OK) =>
            val protocol = migrate(ks, h, p, dbi, nni)

            protocol.encode shouldBe Arr(
              Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) :: Bulk("KEYS") :: ks.value.map(Bulk(_))
            )
            protocol.decode(noKeyOrOkToStr(nkOrOk)).right.value shouldBe nkOrOk
        }
        "given key, host, port, db index, timeout and migrate mode" in {
          forAll("key", "host", "port", "db index", "timeout", "response") {
            (k: Key, h: Host, p: Port, dbi: DbIndex, nni: NonNegInt, nkOrOk: NOKEY | OK) =>
              forAll("migrate mode") { mm: KeyMigrateMode =>
                val protocol = migrate(k, h, p, dbi, nni, mm)

                protocol.encode shouldBe Arr(
                  Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk(k) :: Bulk(dbi) :: Bulk(nni) :: mm.params.map(Bulk(_))
                )
                protocol.decode(noKeyOrOkToStr(nkOrOk)).right.value shouldBe nkOrOk
              }
          }
        }
        "given keys, host, port, db index, timeout and migrate mode" in {
          forAll("keys", "host", "port", "db index", "timeout", "response") {
            (ks: TwoOrMoreKeys, h: Host, p: Port, dbi: DbIndex, nni: NonNegInt, nkOrOk: NOKEY | OK) =>
              forAll("migrate mode") { mm: KeyMigrateMode =>
                val protocol = migrate(ks, h, p, dbi, nni, mm)

                protocol.encode shouldBe Arr(
                  Bulk("MIGRATE") :: Bulk(h) :: Bulk(p) :: Bulk("") :: Bulk(dbi) :: Bulk(nni) ::
                  mm.params.map(Bulk(_)) ::: (Bulk("KEYS") :: ks.value.map(Bulk(_)))
                )
                protocol.decode(noKeyOrOkToStr(nkOrOk)).right.value shouldBe nkOrOk
              }
          }
        }
      }
    }

    "using move" should {

      "roundtrip successfully" when {
        "given key and db" in forAll("key", "db", "moved") { (k: Key, db: DbIndex, b: Boolean) =>
          val protocol = move(k, db)

          protocol.encode shouldBe Arr(Bulk("MOVE"), Bulk(k), Bulk(db))
          protocol.decode(boolToNum(b)).right.value shouldBe b
        }
      }
    }

    "using obj.encoding" should {

      "roundtrip successfully" when {
        "given key" in forAll("key", "encoding") { (k: Key, oe: Option[KeyEncoding]) =>
          val protocol = obj.encoding(k)

          protocol.encode shouldBe Arr(Bulk("OBJECT"), Bulk("ENCODING"), Bulk(k))
          protocol.decode(oe.fold(NullBulk: GenBulk)(Bulk(_))).right.value shouldBe oe
        }
      }
    }

    "using obj.freq" should {

      "roundtrip successfully" when {
        "given key" in forAll("key", "frequency") { (k: Key, nnl: NonNegLong) =>
          val protocol = obj.freq(k)

          protocol.encode shouldBe Arr(Bulk("OBJECT"), Bulk("FREQ"), Bulk(k))
          protocol.decode(Num(nnl.value)).right.value shouldBe nnl
        }
      }
    }

    "using obj.idletime" should {

      "roundtrip successfully" when {
        "given key" in forAll("key", "idle time") { (k: Key, onni: Option[NonNegInt]) =>
          val protocol = obj.idletime(k)

          protocol.encode shouldBe Arr(Bulk("OBJECT"), Bulk("IDLETIME"), Bulk(k))
          protocol.decode(onni.fold(NullBulk: GenBulk)(Bulk(_))).right.value shouldBe onni
        }
      }
    }

    "using obj.refcount" should {

      "roundtrip successfully" when {
        "given key" in forAll("key", "ref count") { (k: Key, nni: NonNegInt) =>
          val protocol = obj.refcount(k)

          protocol.encode shouldBe Arr(Bulk("OBJECT"), Bulk("REFCOUNT"), Bulk(k))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
      }
    }
  }
}
