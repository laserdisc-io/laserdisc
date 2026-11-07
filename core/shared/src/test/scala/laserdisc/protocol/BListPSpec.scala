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

abstract class BListPSpec extends BaseSpec with BListP {

  test("The Blocking List protocol using blpop fails to compile given one key and timeout but missing read instance") {
    assertCompileErrors(
      compileErrors("""blpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), BlockingSeconds(0.0d))"""),
      """|error:
         |Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar].
         |
         |Try writing your own, for example:
         |
         |implicit final val myRead: Read[laserdisc.protocol.Bulk, laserdisc.Bar] = Read.instance { (a: laserdisc.protocol.Bulk) =>
         |  ??? // Right(b) when a can be read as laserdisc.Bar, Left(RESPDecErr(...)) when it can't
         |}
         |
         |You may also be able to build it from an existing Read with map, contramap or flatMap.
         |
         |blpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), BlockingSeconds(0.0d))
         |          ^
         |""".stripMargin,
      "Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar]."
    )
  }

  test("The Blocking List protocol using brpop fails to compile given one key and timeout but missing read instance") {
    assertCompileErrors(
      compileErrors("""brpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), BlockingSeconds(0.0d))"""),
      """|error:
         |Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar].
         |
         |Try writing your own, for example:
         |
         |implicit final val myRead: Read[laserdisc.protocol.Bulk, laserdisc.Bar] = Read.instance { (a: laserdisc.protocol.Bulk) =>
         |  ??? // Right(b) when a can be read as laserdisc.Bar, Left(RESPDecErr(...)) when it can't
         |}
         |
         |You may also be able to build it from an existing Read with map, contramap or flatMap.
         |
         |brpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), BlockingSeconds(0.0d))
         |          ^
         |""".stripMargin,
      "Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar]."
    )
  }

  property("The Blocking List protocol using blpop roundtrips successfully given one or more keys and timeout") {
    forAll { (ks: OneOrMoreKeys, t: BlockingSeconds, i: Int) =>
      val protocol = blpop[Int](ks, t)
      assertEquals(protocol.encode, Arr((Bulk("BLPOP") :: ks.value.map(Bulk(_))) :+ Bulk(t)))
      protocol.decode(Arr(Bulk(ks.value.head), Bulk(i))).onRight(_ contains KV(ks.value.head, i))
    }
  }

  property("The Blocking List protocol using blpop roundtrips successfully given one or more keys, timeout and specific read instance") {
    forAll { (ks: OneOrMoreKeys, t: BlockingSeconds, i: Int) =>
      val protocol = blpop[Foo](ks, t)
      assertEquals(protocol.encode, Arr((Bulk("BLPOP") :: ks.value.map(Bulk(_))) :+ Bulk(t)))
      protocol.decode(Arr(Bulk(ks.value.head), Bulk(i))).onRight(_ contains KV(ks.value.head, Foo(i)))
    }
  }

  property("The Blocking List protocol using brpop roundtrips successfully given one or more keys and timeout") {
    forAll { (ks: OneOrMoreKeys, t: BlockingSeconds, i: Int) =>
      val protocol = brpop[Int](ks, t)
      assertEquals(protocol.encode, Arr((Bulk("BRPOP") :: ks.value.map(Bulk(_))) :+ Bulk(t)))
      protocol.decode(Arr(Bulk(ks.value.head), Bulk(i))).onRight(_ contains KV(ks.value.head, i))
    }
  }

  property("The Blocking List protocol using brpop roundtrips successfully given one or more keys, timeout and specific read instance") {
    forAll { (ks: OneOrMoreKeys, t: BlockingSeconds, i: Int) =>
      val protocol = brpop[Foo](ks, t)
      assertEquals(protocol.encode, Arr((Bulk("BRPOP") :: ks.value.map(Bulk(_))) :+ Bulk(t)))
      protocol.decode(Arr(Bulk(ks.value.head), Bulk(i))).onRight(_ contains KV(ks.value.head, Foo(i)))
    }
  }

  property("The Blocking List protocol using brpoplpush roundtrips successfully given source key and destination key") {
    forAll { (s: Key, d: Key, i: Int) =>
      val protocol = brpoplpush[Int](s, d)
      assertEquals(protocol.encode, Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(0)))
      protocol.decode(Bulk(i)).onRight(_ contains i)
    }
  }

  property(
    "The Blocking List protocol using brpoplpush roundtrips successfully given source key, destination key and specific read instance"
  ) {
    forAll { (s: Key, d: Key, i: Int) =>
      val protocol = brpoplpush[Foo](s, d)
      assertEquals(protocol.encode, Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(0)))
      protocol.decode(Bulk(i)).onRight(_ contains Foo(i))
    }
  }

  property("The Blocking List protocol using brpoplpush roundtrips successfully given source key, destination key and timeout") {
    forAll { (s: Key, d: Key, pi: BlockingSeconds, i: Int) =>
      val protocol = brpoplpush[Int](s, d, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(pi)))
      protocol.decode(Bulk(i)).onRight(_ contains i)
    }
  }

  property(
    "The Blocking List protocol using brpoplpush roundtrips successfully given source key, destination key, timeout and specific read instance"
  ) {
    forAll { (s: Key, d: Key, pi: BlockingSeconds, i: Int) =>
      val protocol = brpoplpush[Foo](s, d, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(pi)))
      protocol.decode(Bulk(i)).onRight(_ contains Foo(i))
    }
  }

  test("The Blocking List protocol sends whole seconds without a fraction, and others with one") {
    val a = OneOrMoreKeys.unsafeFrom(List(Key("a")))
    assertEquals(blpop[String](a, BlockingSeconds(5.0d)).encode, Arr(Bulk("BLPOP"), Bulk("a"), Bulk("5")))
    assertEquals(blpop[String](a, BlockingSeconds(0.0d)).encode, Arr(Bulk("BLPOP"), Bulk("a"), Bulk("0")))
    assertEquals(blpop[String](a, BlockingSeconds(0.25d)).encode, Arr(Bulk("BLPOP"), Bulk("a"), Bulk("0.25")))
  }

  property("The Blocking List protocol using blmove sends the timeout last") {
    forAll { (s: Key, d: Key, t: BlockingSeconds, os: Option[String]) =>
      val protocol = blmove[String](s, d, ListP.Side.right, ListP.Side.left, t)
      assertEquals(protocol.encode, Arr(Bulk("BLMOVE"), Bulk(s), Bulk(d), Bulk("RIGHT"), Bulk("LEFT"), Bulk(t)))
      assertEquals(protocol.decode(os.fold(NullBulk: GenBulk)(Bulk(_))), os)
    }
  }

  property("The Blocking List protocol using blmpop sends the timeout first") {
    forAll { (ks: OneOrMoreKeys, t: BlockingSeconds, okv: Option[KV[List[String]]]) =>
      val protocol = blmpop[String](ks, ListP.Side.left, t)
      assertEquals(protocol.encode, Arr(Bulk("BLMPOP") :: Bulk(t) :: Bulk(ks.value.size) :: ks.value.map(Bulk(_)) ::: List(Bulk("LEFT"))))
      val reply = okv.fold(NilArr: GenArr) { case KV(k, vs) => Arr(Bulk(k), Arr(vs.map(Bulk(_)))) }
      assertEquals(protocol.decode(reply), okv)
    }
  }

  property("The Blocking List protocol using blmovem without an amount and order sends the timeout last") {
    forAll { (s: Key, d: Key, t: BlockingSeconds) =>
      val protocol = blmovem[String](s, d, ListP.Side.left, ListP.Side.left, t)
      assertEquals(protocol.encode, Arr(Bulk("BLMOVEM"), Bulk(s), Bulk(d), Bulk("LEFT"), Bulk("LEFT"), Bulk(t)))
    }
  }

  property("The Blocking List protocol using blmovem sends the timeout before the amount") {
    forAll { (s: Key, d: Key, c: PosInt, t: BlockingSeconds) =>
      val protocol = blmovem[String](s, d, ListP.Side.left, ListP.Side.left, ListP.Amount.upTo(c), ListP.Order.oneByOne, t)
      assertEquals(
        protocol.encode,
        Arr(Bulk("BLMOVEM"), Bulk(s), Bulk(d), Bulk("LEFT"), Bulk("LEFT"), Bulk(t), Bulk("COUNT"), Bulk(c), Bulk("OBO"))
      )
    }
  }
}
