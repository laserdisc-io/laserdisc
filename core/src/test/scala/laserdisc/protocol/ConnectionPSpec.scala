package laserdisc
package protocol

import org.scalacheck.Prop.forAll

final class ConnectionPSpec extends BaseSpec with ConnectionP {
  property("The Connection protocol using auth roundtrips successfully given non empty password") {
    forAll { key: Key =>
      val protocol = auth(key)
      assertEquals(protocol.encode, Arr(Bulk("AUTH"), Bulk(key.value)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Connection protocol using echo roundtrips successfully given any String message") {
    forAll { s: String =>
      val protocol = echo(s)
      assertEquals(protocol.encode, Arr(Bulk("ECHO"), Bulk(s)))
      assertEquals(protocol.decode(Bulk(s)), s)
    }
  }

  property("The Connection protocol using echo roundtrips successfully given any Int message") {
    forAll { i: Int =>
      val protocol = echo(i)
      assertEquals(protocol.encode, Arr(Bulk("ECHO"), Bulk(i)))
      assertEquals(protocol.decode(Bulk(i)), i)
    }
  }

  property("The Connection protocol using ping roundtrips successfully given any String message") {
    forAll { s: String =>
      val protocol = ping(s)
      assertEquals(protocol.encode, Arr(Bulk("PING"), Bulk(s)))
      assertEquals(protocol.decode(Bulk(s)), s)
    }
  }

  property("The Connection protocol using ping roundtrips successfully given any Int message") {
    forAll { i: Int =>
      val protocol = ping(i)
      assertEquals(protocol.encode, Arr(Bulk("PING"), Bulk(i)))
      assertEquals(protocol.decode(Bulk(i)), i)
    }
  }

  property("The Connection protocol using ping roundtrips successfully using val to get back PONG message") {
    val protocol = ping
    assertEquals(protocol.encode, Arr(Bulk("PING")))
    assertEquals(protocol.decode(Str(PONG.value)), PONG)
  }

  property("The Connection protocol using quit roundtrips successfully") {
    val protocol = quit
    assertEquals(protocol.encode, Arr(Bulk("QUIT")))
    assertEquals(protocol.decode(Str(OK.value)), OK)
  }

  property("The Connection protocol using select roundtrips successfully given valid DbIndexes") {
    forAll { dbi: DbIndex =>
      val protocol = select(dbi)
      assertEquals(protocol.encode, Arr(Bulk("SELECT"), Bulk(dbi)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Connection protocol using swapdb roundtrips successfully given valid DbIndexes") {
    forAll { (dbi1: DbIndex, dbi2: DbIndex) =>
      val protocol = swapdb(dbi1, dbi2)
      assertEquals(protocol.encode, Arr(Bulk("SWAPDB"), Bulk(dbi1), Bulk(dbi2)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }
}
