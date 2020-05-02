package laserdisc
package protocol

final class ListPSpec extends ListExtPSpec {
  import listtypes._
  import org.scalacheck.{Arbitrary, Gen}

  private[this] implicit final val positionArb: Arbitrary[ListPosition] = Arbitrary {
    Gen.oneOf(ListPosition.after, ListPosition.before)
  }

  "The List protocol" when {
    "using lindex" should {
      "roundtrip successfully" when {
        "given key and index" in forAll("key", "index", "value") { (k: Key, i: Index, oi: Option[Int]) =>
          val protocol = lindex[Int](k, i)

          protocol.encode shouldBe Arr(Bulk("LINDEX"), Bulk(k), Bulk(i))
          protocol.decode(oi.fold(NullBulk: GenBulk)(Bulk(_))) onRight (_ shouldBe oi)
        }
      }
    }

    "using linsert" should {
      "roundtrip successfully" when {
        "given key, position, pivot and value" in {
          forAll("key", "position", "pivot", "value", "inserted") { (k: Key, p: ListPosition, pi: String, v: String, opi: Option[PosInt]) =>
            val protocol = linsert(k, p, pi, v)

            protocol.encode shouldBe Arr(Bulk("LINSERT"), Bulk(k), Bulk(p), Bulk(pi), Bulk(v))
            protocol.decode(Num(opi.fold(-1L)(_.value.toLong))) onRight (_ shouldBe opi)
          }
        }
      }
    }

    "using llen" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "length") { (k: Key, nni: NonNegInt) =>
          val protocol = llen(k)

          protocol.encode shouldBe Arr(Bulk("LLEN"), Bulk(k))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using lpop" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "popped value") { (k: Key, os: Option[String]) =>
          val protocol = lpop[String](k)

          protocol.encode shouldBe Arr(Bulk("LPOP"), Bulk(k))
          protocol.decode(os.fold(NullBulk: GenBulk)(Bulk(_))) onRight (_ shouldBe os)
        }
      }
    }

    "using lpush" should {
      "roundtrip successfully" when {
        "given key and values" in forAll("key", "values", "pushed") { (k: Key, is: OneOrMore[Int], pi: PosInt) =>
          val protocol = lpush(k, is)

          protocol.encode shouldBe Arr(Bulk("LPUSH") :: Bulk(k) :: is.value.map(Bulk(_)))
          protocol.decode(Num(pi.value.toLong)) onRight (_ shouldBe pi)
        }
      }
    }

    "using lpushx" should {
      "roundtrip successfully" when {
        "given key and value" in forAll("key", "value", "pushed") { (k: Key, i: Int, opi: Option[PosInt]) =>
          val protocol = lpushx(k, i)

          protocol.encode shouldBe Arr(Bulk("LPUSHX"), Bulk(k), Bulk(i))
          protocol.decode(Num(opi.fold(0L)(_.value.toLong))) onRight (_ shouldBe opi)
        }
      }
    }

    "using lrange" should {
      "roundtrip successfully" when {
        "given key, start index and end index" in {
          forAll("key", "start index", "end index", "values") { (k: Key, si: Index, ei: Index, is: List[Int]) =>
            val protocol = lrange[Int](k, si, ei)

            protocol.encode shouldBe Arr(Bulk("LRANGE"), Bulk(k), Bulk(si), Bulk(ei))
            protocol.decode(Arr(is.map(Bulk(_)))) onRight (_ shouldBe is)
          }
        }
      }
    }

    "using lrem" should {
      "roundtrip successfully" when {
        "given key, count and value" in forAll("key", "count", "value", "removed") { (k: Key, i: Index, s: String, nni: NonNegInt) =>
          val protocol = lrem(k, i, s)

          protocol.encode shouldBe Arr(Bulk("LREM"), Bulk(k), Bulk(i), Bulk(s))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using lset" should {
      "roundtrip successfully" when {
        "given key, count and value" in forAll("key", "count", "value") { (k: Key, i: Index, s: String) =>
          val protocol = lset(k, i, s)

          protocol.encode shouldBe Arr(Bulk("LSET"), Bulk(k), Bulk(i), Bulk(s))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using ltrim" should {
      "roundtrip successfully" when {
        "given key, start index and end index" in forAll("key", "start index", "end index") { (k: Key, si: Index, ei: Index) =>
          val protocol = ltrim(k, si, ei)

          protocol.encode shouldBe Arr(Bulk("LTRIM"), Bulk(k), Bulk(si), Bulk(ei))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using rpop" should {
      "roundtrip successfully" when {
        "given key" in forAll("key", "popped value") { (k: Key, os: Option[String]) =>
          val protocol = rpop[String](k)

          protocol.encode shouldBe Arr(Bulk("RPOP"), Bulk(k))
          protocol.decode(os.fold(NullBulk: GenBulk)(Bulk(_))) onRight (_ shouldBe os)
        }
      }
    }

    "using rpoplpush" should {
      "roundtrip successfully" when {
        "given source key and destination key" in {
          forAll("source key", "destination", "popped value") { (sk: Key, dk: Key, os: Option[String]) =>
            val protocol = rpoplpush[String](sk, dk)

            protocol.encode shouldBe Arr(Bulk("RPOPLPUSH"), Bulk(sk), Bulk(dk))
            protocol.decode(os.fold(NullBulk: GenBulk)(Bulk(_))) onRight (_ shouldBe os)
          }
        }
      }
    }

    "using rpush" should {
      "roundtrip successfully" when {
        "given key and values" in forAll("key", "values", "pushed") { (k: Key, is: OneOrMore[Int], pi: PosInt) =>
          val protocol = rpush(k, is)

          protocol.encode shouldBe Arr(Bulk("RPUSH") :: Bulk(k) :: is.value.map(Bulk(_)))
          protocol.decode(Num(pi.value.toLong)) onRight (_ shouldBe pi)
        }
      }
    }

    "using rpushx" should {
      "roundtrip successfully" when {
        "given key and value" in forAll("key", "value", "pushed") { (k: Key, i: Int, opi: Option[PosInt]) =>
          val protocol = rpushx(k, i)

          protocol.encode shouldBe Arr(Bulk("RPUSHX"), Bulk(k), Bulk(i))
          protocol.decode(Num(opi.fold(0L)(_.value.toLong))) onRight (_ shouldBe opi)
        }
      }
    }
  }
}
