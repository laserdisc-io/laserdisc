package laserdisc
package protocol

import org.scalacheck.Prop.forAll

abstract class ListPSpec extends BaseSpec with ListP {
  import listtypes._
  import org.scalacheck.{Arbitrary, Gen}

  private[this] implicit final val positionArb: Arbitrary[ListPosition] = Arbitrary {
    Gen.oneOf(ListPosition.after, ListPosition.before)
  }

  property("The List protocol using lindex roundtrips successfully given key and index") {
    forAll { (k: Key, i: Index, oi: Option[Int]) =>
      val protocol = lindex[Int](k, i)
      assertEquals(protocol.encode, Arr(Bulk("LINDEX"), Bulk(k), Bulk(i)))
      assertEquals(protocol.decode(oi.fold(NullBulk: GenBulk)(Bulk(_))), oi)
    }
  }

  property("The List protocol using linsert roundtrips successfully given key, position, pivot and value") {
    forAll { (k: Key, p: ListPosition, pi: String, v: String, opi: Option[PosInt]) =>
      val protocol = linsert(k, p, pi, v)
      assertEquals(protocol.encode, Arr(Bulk("LINSERT"), Bulk(k), Bulk(p), Bulk(pi), Bulk(v)))
      assertEquals(protocol.decode(Num(opi.fold(-1L)(_.value.toLong))), opi)
    }
  }

  property("The List protocol using llen roundtrips successfully given key") {
    forAll { (k: Key, nni: NonNegInt) =>
      val protocol = llen(k)
      assertEquals(protocol.encode, Arr(Bulk("LLEN"), Bulk(k)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The List protocol using lpop roundtrips successfully given key") {
    forAll { (k: Key, os: Option[String]) =>
      val protocol = lpop[String](k)
      assertEquals(protocol.encode, Arr(Bulk("LPOP"), Bulk(k)))
      assertEquals(protocol.decode(os.fold(NullBulk: GenBulk)(Bulk(_))), os)
    }
  }

  property("The List protocol using lpush roundtrips successfully given key and values") {
    forAll { (k: Key, is: OneOrMore[Int], pi: PosInt) =>
      val protocol = lpush(k, is)
      assertEquals(protocol.encode, Arr(Bulk("LPUSH") :: Bulk(k) :: is.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }

  property("The List protocol using lpushx roundtrips successfully given key and value") {
    forAll { (k: Key, i: Int, opi: Option[PosInt]) =>
      val protocol = lpushx(k, i)
      assertEquals(protocol.encode, Arr(Bulk("LPUSHX"), Bulk(k), Bulk(i)))
      assertEquals(protocol.decode(Num(opi.fold(0L)(_.value.toLong))), opi)
    }
  }

  property("The List protocol using lrange roundtrips successfully given key, start index and end index") {
    forAll { (k: Key, si: Index, ei: Index, is: List[Int]) =>
      val protocol = lrange[Int](k, si, ei)
      assertEquals(protocol.encode, Arr(Bulk("LRANGE"), Bulk(k), Bulk(si), Bulk(ei)))
      assertEquals(protocol.decode(Arr(is.map(Bulk(_)))), is)
    }
  }

  property("The List protocol using lrem roundtrips successfully given key, count and value") {
    forAll { (k: Key, i: Index, s: String, nni: NonNegInt) =>
      val protocol = lrem(k, i, s)
      assertEquals(protocol.encode, Arr(Bulk("LREM"), Bulk(k), Bulk(i), Bulk(s)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The List protocol using lset roundtrips successfully given key, count and value") {
    forAll { (k: Key, i: Index, s: String) =>
      val protocol = lset(k, i, s)
      assertEquals(protocol.encode, Arr(Bulk("LSET"), Bulk(k), Bulk(i), Bulk(s)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The List protocol using ltrim roundtrips successfully given key, start index and end index") {
    forAll { (k: Key, si: Index, ei: Index) =>
      val protocol = ltrim(k, si, ei)
      assertEquals(protocol.encode, Arr(Bulk("LTRIM"), Bulk(k), Bulk(si), Bulk(ei)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The List protocol using rpop roundtrips successfully given key") {
    forAll { (k: Key, os: Option[String]) =>
      val protocol = rpop[String](k)
      assertEquals(protocol.encode, Arr(Bulk("RPOP"), Bulk(k)))
      assertEquals(protocol.decode(os.fold(NullBulk: GenBulk)(Bulk(_))), os)
    }
  }

  property("The List protocol using rpoplpush roundtrips successfully given source key and destination key") {
    forAll { (sk: Key, dk: Key, os: Option[String]) =>
      val protocol = rpoplpush[String](sk, dk)
      assertEquals(protocol.encode, Arr(Bulk("RPOPLPUSH"), Bulk(sk), Bulk(dk)))
      assertEquals(protocol.decode(os.fold(NullBulk: GenBulk)(Bulk(_))), os)
    }
  }

  property("The List protocol using rpush roundtrips successfully given key and values") {
    forAll { (k: Key, is: OneOrMore[Int], pi: PosInt) =>
      val protocol = rpush(k, is)
      assertEquals(protocol.encode, Arr(Bulk("RPUSH") :: Bulk(k) :: is.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(pi.value.toLong)), pi)
    }
  }

  property("The List protocol using rpushx roundtrips successfully given key and value") {
    forAll { (k: Key, i: Int, opi: Option[PosInt]) =>
      val protocol = rpushx(k, i)
      assertEquals(protocol.encode, Arr(Bulk("RPUSHX"), Bulk(k), Bulk(i)))
      assertEquals(protocol.decode(Num(opi.fold(0L)(_.value.toLong))), opi)
    }
  }
}
