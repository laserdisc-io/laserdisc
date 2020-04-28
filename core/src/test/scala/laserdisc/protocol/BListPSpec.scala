package laserdisc
package protocol

import org.scalacheck.Prop.forAll

abstract class BListPSpec extends BaseSpec with BListP {
  test("The Blocking List extended protocol using blpop fails to compile given key but missing read instance") {
    assertNoDiff(
      compileErrors("""blpop[Bar](Key("a"))"""),
      """|error:
         |Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar].
         |
         |Try writing your own, for example:
         |
         |implicit final val myRead: Read[laserdisc.protocol.Bulk, laserdisc.Bar] = new Read[laserdisc.protocol.Bulk, laserdisc.Bar] {
         |  override final def read(a: laserdisc.protocol.Bulk): Option[laserdisc.Bar] = ???
         |}
         |
         |Note 1: you can use the factory method Read.instance instead of creating it manually as shown above
         |Note 2: make sure to inspect the combinators as you may be able to leverage some other Read instance
         |
         |blpop[Bar](Key("a"))
         |          ^
         |""".stripMargin
    )
  }

  test("The Blocking List extended protocol using blpop fails to compile given key and timeout but missing read instance") {
    assertNoDiff(
      compileErrors("""blpop[Bar](Key("a"), PosInt(1))"""),
      """|error:
         |Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar].
         |
         |Try writing your own, for example:
         |
         |implicit final val myRead: Read[laserdisc.protocol.Bulk, laserdisc.Bar] = new Read[laserdisc.protocol.Bulk, laserdisc.Bar] {
         |  override final def read(a: laserdisc.protocol.Bulk): Option[laserdisc.Bar] = ???
         |}
         |
         |Note 1: you can use the factory method Read.instance instead of creating it manually as shown above
         |Note 2: make sure to inspect the combinators as you may be able to leverage some other Read instance
         |
         |blpop[Bar](Key("a"), PosInt(1))
         |          ^
         |""".stripMargin
    )
  }

  test("The Blocking List extended protocol using blpop fails to compile given one key and timeout but missing read instance") {
    assertNoDiff(
      compileErrors("""blpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))"""),
      """|error:
         |Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar].
         |
         |Try writing your own, for example:
         |
         |implicit final val myRead: Read[laserdisc.protocol.Bulk, laserdisc.Bar] = new Read[laserdisc.protocol.Bulk, laserdisc.Bar] {
         |  override final def read(a: laserdisc.protocol.Bulk): Option[laserdisc.Bar] = ???
         |}
         |
         |Note 1: you can use the factory method Read.instance instead of creating it manually as shown above
         |Note 2: make sure to inspect the combinators as you may be able to leverage some other Read instance
         |
         |blpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))
         |          ^
         |""".stripMargin
    )
  }

  test("The Blocking List extended protocol using brpop fails to compile given key but missing read instance") {
    assertNoDiff(
      compileErrors("""brpop[Bar](Key("a"))"""),
      """|error:
         |Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar].
         |
         |Try writing your own, for example:
         |
         |implicit final val myRead: Read[laserdisc.protocol.Bulk, laserdisc.Bar] = new Read[laserdisc.protocol.Bulk, laserdisc.Bar] {
         |  override final def read(a: laserdisc.protocol.Bulk): Option[laserdisc.Bar] = ???
         |}
         |
         |Note 1: you can use the factory method Read.instance instead of creating it manually as shown above
         |Note 2: make sure to inspect the combinators as you may be able to leverage some other Read instance
         |
         |brpop[Bar](Key("a"))
         |          ^
         |""".stripMargin
    )
  }

  test("The Blocking List extended protocol using brpop fails to compile given key and timeout but missing read instance") {
    assertNoDiff(
      compileErrors("""brpop[Bar](Key("a"), PosInt(1))"""),
      """|error:
         |Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar].
         |
         |Try writing your own, for example:
         |
         |implicit final val myRead: Read[laserdisc.protocol.Bulk, laserdisc.Bar] = new Read[laserdisc.protocol.Bulk, laserdisc.Bar] {
         |  override final def read(a: laserdisc.protocol.Bulk): Option[laserdisc.Bar] = ???
         |}
         |
         |Note 1: you can use the factory method Read.instance instead of creating it manually as shown above
         |Note 2: make sure to inspect the combinators as you may be able to leverage some other Read instance
         |
         |brpop[Bar](Key("a"), PosInt(1))
         |          ^
         |""".stripMargin
    )
  }

  test("The Blocking List extended protocol using brpop fails to compile given one key and timeout but missing read instance") {
    assertNoDiff(
      compileErrors("""brpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))"""),
      """|error:
         |Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar].
         |
         |Try writing your own, for example:
         |
         |implicit final val myRead: Read[laserdisc.protocol.Bulk, laserdisc.Bar] = new Read[laserdisc.protocol.Bulk, laserdisc.Bar] {
         |  override final def read(a: laserdisc.protocol.Bulk): Option[laserdisc.Bar] = ???
         |}
         |
         |Note 1: you can use the factory method Read.instance instead of creating it manually as shown above
         |Note 2: make sure to inspect the combinators as you may be able to leverage some other Read instance
         |
         |brpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))
         |          ^
         |""".stripMargin
    )
  }

  property("The Blocking List protocol using blpop roundtrips successfully given one or more keys and timeout") {
    forAll { (ks: OneOrMoreKeys, t: NonNegInt, i: Int) =>
      val protocol = blpop[Int](ks, t)
      assertEquals(protocol.encode, Arr((Bulk("BLPOP") :: ks.value.map(Bulk(_))) :+ Bulk(t)))
      protocol.decode(Arr(Bulk(ks.value.head), Bulk(i))) onRight (_ contains KV(ks.value.head, i))
    }
  }

  property("The Blocking List protocol using blpop roundtrips successfully given one or more keys, timeout and specific read instance") {
    forAll { (ks: OneOrMoreKeys, t: NonNegInt, i: Int) =>
      val protocol = blpop[Foo](ks, t)
      assertEquals(protocol.encode, Arr((Bulk("BLPOP") :: ks.value.map(Bulk(_))) :+ Bulk(t)))
      protocol.decode(Arr(Bulk(ks.value.head), Bulk(i))) onRight (_ contains KV(ks.value.head, Foo(i)))
    }
  }

  property("The Blocking List protocol using brpop roundtrips successfully given one or more keys and timeout") {
    forAll { (ks: OneOrMoreKeys, t: NonNegInt, i: Int) =>
      val protocol = brpop[Int](ks, t)
      assertEquals(protocol.encode, Arr((Bulk("BRPOP") :: ks.value.map(Bulk(_))) :+ Bulk(t)))
      protocol.decode(Arr(Bulk(ks.value.head), Bulk(i))) onRight (_ contains KV(ks.value.head, i))
    }
  }

  property("The Blocking List protocol using brpop roundtrips successfully given one or more keys, timeout and specific read instance") {
    forAll { (ks: OneOrMoreKeys, t: NonNegInt, i: Int) =>
      val protocol = brpop[Foo](ks, t)
      assertEquals(protocol.encode, Arr((Bulk("BRPOP") :: ks.value.map(Bulk(_))) :+ Bulk(t)))
      protocol.decode(Arr(Bulk(ks.value.head), Bulk(i))) onRight (_ contains KV(ks.value.head, Foo(i)))
    }
  }

  property("The Blocking List protocol using brpoplpush roundtrips successfully given source key and destination key") {
    forAll { (s: Key, d: Key, i: Int) =>
      val protocol = brpoplpush[Int](s, d)
      assertEquals(protocol.encode, Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(0)))
      protocol.decode(Bulk(i)) onRight (_ contains i)
    }
  }

  property(
    "The Blocking List protocol using brpoplpush roundtrips successfully given source key, destination key and specific read instance"
  ) {
    forAll { (s: Key, d: Key, i: Int) =>
      val protocol = brpoplpush[Foo](s, d)
      assertEquals(protocol.encode, Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(0)))
      protocol.decode(Bulk(i)) onRight (_ contains Foo(i))
    }
  }

  property("The Blocking List protocol using brpoplpush roundtrips successfully given source key, destination key and timeout") {
    forAll { (s: Key, d: Key, pi: PosInt, i: Int) =>
      val protocol = brpoplpush[Int](s, d, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(pi)))
      protocol.decode(Bulk(i)) onRight (_ contains i)
    }
  }

  property(
    "The Blocking List protocol using brpoplpush roundtrips successfully given source key, destination key, timeout and specific read instance"
  ) {
    forAll { (s: Key, d: Key, pi: PosInt, i: Int) =>
      val protocol = brpoplpush[Foo](s, d, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(pi)))
      protocol.decode(Bulk(i)) onRight (_ contains Foo(i))
    }
  }
}
