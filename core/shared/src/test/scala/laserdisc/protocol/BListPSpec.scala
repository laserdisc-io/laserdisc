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
      compileErrors("""blpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))"""),
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
         |blpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))
         |          ^
         |""".stripMargin,
      "Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar]."
    )
  }

  test("The Blocking List protocol using brpop fails to compile given one key and timeout but missing read instance") {
    assertCompileErrors(
      compileErrors("""brpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))"""),
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
         |brpop[Bar](OneOrMoreKeys.unsafeFrom(List(Key("a"))), NonNegInt(0))
         |          ^
         |""".stripMargin,
      "Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar]."
    )
  }

  property("The Blocking List protocol using blpop roundtrips successfully given one or more keys and timeout") {
    forAll { (ks: OneOrMoreKeys, t: NonNegInt, i: Int) =>
      val protocol = blpop[Int](ks, t)
      assertEquals(protocol.encode, Arr((Bulk("BLPOP") :: ks.value.map(Bulk(_))) :+ Bulk(t)))
      protocol.decode(Arr(Bulk(ks.value.head), Bulk(i))).onRight(_ contains KV(ks.value.head, i))
    }
  }

  property("The Blocking List protocol using blpop roundtrips successfully given one or more keys, timeout and specific read instance") {
    forAll { (ks: OneOrMoreKeys, t: NonNegInt, i: Int) =>
      val protocol = blpop[Foo](ks, t)
      assertEquals(protocol.encode, Arr((Bulk("BLPOP") :: ks.value.map(Bulk(_))) :+ Bulk(t)))
      protocol.decode(Arr(Bulk(ks.value.head), Bulk(i))).onRight(_ contains KV(ks.value.head, Foo(i)))
    }
  }

  property("The Blocking List protocol using brpop roundtrips successfully given one or more keys and timeout") {
    forAll { (ks: OneOrMoreKeys, t: NonNegInt, i: Int) =>
      val protocol = brpop[Int](ks, t)
      assertEquals(protocol.encode, Arr((Bulk("BRPOP") :: ks.value.map(Bulk(_))) :+ Bulk(t)))
      protocol.decode(Arr(Bulk(ks.value.head), Bulk(i))).onRight(_ contains KV(ks.value.head, i))
    }
  }

  property("The Blocking List protocol using brpop roundtrips successfully given one or more keys, timeout and specific read instance") {
    forAll { (ks: OneOrMoreKeys, t: NonNegInt, i: Int) =>
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
    forAll { (s: Key, d: Key, pi: PosInt, i: Int) =>
      val protocol = brpoplpush[Int](s, d, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(pi)))
      protocol.decode(Bulk(i)).onRight(_ contains i)
    }
  }

  property(
    "The Blocking List protocol using brpoplpush roundtrips successfully given source key, destination key, timeout and specific read instance"
  ) {
    forAll { (s: Key, d: Key, pi: PosInt, i: Int) =>
      val protocol = brpoplpush[Foo](s, d, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOPLPUSH"), Bulk(s), Bulk(d), Bulk(pi)))
      protocol.decode(Bulk(i)).onRight(_ contains Foo(i))
    }
  }
}
