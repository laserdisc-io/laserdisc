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

final class BListExtPSpec extends BListPSpec with ExtPArguments {
  test("The Blocking List extended protocol using blpop fails to compile given key but missing read instance") {
    assertCompileErrors(
      compileErrors("""blpop[Bar](Key("a"))"""),
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
         |blpop[Bar](Key("a"))
         |          ^
         |""".stripMargin,
      "Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar]."
    )
  }
  test("The Blocking List extended protocol using blpop fails to compile given key and timeout but missing read instance") {
    assertCompileErrors(
      compileErrors("""blpop[Bar](Key("a"), PosInt(1))"""),
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
         |blpop[Bar](Key("a"), PosInt(1))
         |          ^
         |""".stripMargin,
      "Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar]."
    )
  }
  // BLPOP with no timeout specified
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1") {
    forAll { (k1: Key, i: Int) =>
      val protocol = blpop[Int](k1)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1, key2") {
    forAll { (k1: Key, k2: Key, i: Int) =>
      val protocol = blpop[Int](k1, k2)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3") {
    forAll { (k1: Key, k2: Key, k3: Key, i: Int) =>
      val protocol = blpop[Int](k1, k2, k3)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3, key4") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, i: Int) =>
      val protocol = blpop[Int](k1, k2, k3, k4)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3, key4, key5") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, i: Int) =>
      val protocol = blpop[Int](k1, k2, k3, k4, k5)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  // BLPOP with no timeout specified and specific read instance
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1 and specific read instance") {
    forAll { (k1: Key, i: Int) =>
      val protocol = blpop[Foo](k1)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1, key2 and specific read instance") {
    forAll { (k1: Key, k2: Key, i: Int) =>
      val protocol = blpop[Foo](k1, k2)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3 and specific read instance") {
    forAll { (k1: Key, k2: Key, k3: Key, i: Int) =>
      val protocol = blpop[Foo](k1, k2, k3)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3, key4 and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, i: Int) =>
      val protocol = blpop[Foo](k1, k2, k3, k4)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3, key4, key5 and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, i: Int) =>
      val protocol = blpop[Foo](k1, k2, k3, k4, k5)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  // BLPOP with timeout specified
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1 and positive timeout") {
    forAll { (k1: Key, pi: PosInt, i: Int) =>
      val protocol = blpop[Int](k1, pi)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1, key2 and positive timeout") {
    forAll { (k1: Key, k2: Key, pi: PosInt, i: Int) =>
      val protocol = blpop[Int](k1, k2, pi)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3 and positive timeout") {
    forAll { (k1: Key, k2: Key, k3: Key, pi: PosInt, i: Int) =>
      val protocol = blpop[Int](k1, k2, k3, pi)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3, key4 and positive timeout") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, pi: PosInt, i: Int) =>
      val protocol = blpop[Int](k1, k2, k3, k4, pi)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property(
    "The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3, key4, key5 and positive timeout"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, pi: PosInt, i: Int) =>
      val protocol = blpop[Int](k1, k2, k3, k4, k5, pi)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  // BLPOP with timeout specified and specific read instance
  property(
    "The Blocking List extended protocol using blpop roundtrips successfully given key1, positive timeout and specific read instance"
  ) {
    forAll { (k1: Key, pi: PosInt, i: Int) =>
      val protocol = blpop[Foo](k1, pi)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, positive timeout and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, pi: PosInt, i: Int) =>
      val protocol = blpop[Foo](k1, k2, pi)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3, positive timeout and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, pi: PosInt, i: Int) =>
      val protocol = blpop[Foo](k1, k2, k3, pi)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3, key4, positive timeout and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, pi: PosInt, i: Int) =>
      val protocol = blpop[Foo](k1, k2, k3, k4, pi)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using blpop roundtrips successfully given key1, key2, key3, key4, key5, positive timeout and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, pi: PosInt, i: Int) =>
      val protocol = blpop[Foo](k1, k2, k3, k4, k5, pi)
      assertEquals(protocol.encode, Arr(Bulk("BLPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  test("The Blocking List extended protocol using brpop fails to compile given key but missing read instance") {
    assertCompileErrors(
      compileErrors("""brpop[Bar](Key("a"))"""),
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
         |brpop[Bar](Key("a"))
         |          ^
         |""".stripMargin,
      "Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar]."
    )
  }
  test("The Blocking List extended protocol using brpop fails to compile given key and timeout but missing read instance") {
    assertCompileErrors(
      compileErrors("""brpop[Bar](Key("a"), PosInt(1))"""),
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
         |brpop[Bar](Key("a"), PosInt(1))
         |          ^
         |""".stripMargin,
      "Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar]."
    )
  }
  // BRPOP with no timeout specified
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1") {
    forAll { (k1: Key, i: Int) =>
      val protocol = brpop[Int](k1)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1, key2") {
    forAll { (k1: Key, k2: Key, i: Int) =>
      val protocol = brpop[Int](k1, k2)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3") {
    forAll { (k1: Key, k2: Key, k3: Key, i: Int) =>
      val protocol = brpop[Int](k1, k2, k3)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3, key4") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, i: Int) =>
      val protocol = brpop[Int](k1, k2, k3, k4)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3, key4, key5") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, i: Int) =>
      val protocol = brpop[Int](k1, k2, k3, k4, k5)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  // BRPOP with no timeout specified and specific read instance
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1 and specific read instance") {
    forAll { (k1: Key, i: Int) =>
      val protocol = brpop[Foo](k1)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1, key2 and specific read instance") {
    forAll { (k1: Key, k2: Key, i: Int) =>
      val protocol = brpop[Foo](k1, k2)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3 and specific read instance") {
    forAll { (k1: Key, k2: Key, k3: Key, i: Int) =>
      val protocol = brpop[Foo](k1, k2, k3)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3, key4 and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, i: Int) =>
      val protocol = brpop[Foo](k1, k2, k3, k4)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3, key4, key5 and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, i: Int) =>
      val protocol = brpop[Foo](k1, k2, k3, k4, k5)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5), Bulk(0)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  // BRPOP with timeout specified
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1 and positive timeout") {
    forAll { (k1: Key, pi: PosInt, i: Int) =>
      val protocol = brpop[Int](k1, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1, key2 and positive timeout") {
    forAll { (k1: Key, k2: Key, pi: PosInt, i: Int) =>
      val protocol = brpop[Int](k1, k2, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3 and positive timeout") {
    forAll { (k1: Key, k2: Key, k3: Key, pi: PosInt, i: Int) =>
      val protocol = brpop[Int](k1, k2, k3, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property("The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3, key4 and positive timeout") {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, pi: PosInt, i: Int) =>
      val protocol = brpop[Int](k1, k2, k3, k4, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  property(
    "The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3, key4, key5 and positive timeout"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, pi: PosInt, i: Int) =>
      val protocol = brpop[Int](k1, k2, k3, k4, k5, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, i))
    }
  }
  // BRPOP with timeout specified and specific read instance
  property(
    "The Blocking List extended protocol using brpop roundtrips successfully given key1, positive timeout and specific read instance"
  ) {
    forAll { (k1: Key, pi: PosInt, i: Int) =>
      val protocol = brpop[Foo](k1, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, positive timeout and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, pi: PosInt, i: Int) =>
      val protocol = brpop[Foo](k1, k2, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3, positive timeout and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, pi: PosInt, i: Int) =>
      val protocol = brpop[Foo](k1, k2, k3, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3, key4, positive timeout and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, pi: PosInt, i: Int) =>
      val protocol = brpop[Foo](k1, k2, k3, k4, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }
  property(
    "The Blocking List extended protocol using brpop roundtrips successfully given key1, key2, key3, key4, key5, positive timeout and specific read instance"
  ) {
    forAll { (k1: Key, k2: Key, k3: Key, k4: Key, k5: Key, pi: PosInt, i: Int) =>
      val protocol = brpop[Foo](k1, k2, k3, k4, k5, pi)
      assertEquals(protocol.encode, Arr(Bulk("BRPOP"), Bulk(k1), Bulk(k2), Bulk(k3), Bulk(k4), Bulk(k5), Bulk(pi)))
      protocol.decode(Arr(Bulk(k1), Bulk(i))).onRight(_ contains KV(k1, Foo(i)))
    }
  }

  //format: off
  private val seconds = PosInt(7)

  test("The Blocking List extended protocol using blpop sends in order from 1 to 22 keys") {
    assertEquals(blpop[String](k1).encode, frame("BLPOP", "k1", "0"))
    assertEquals(blpop[String](k1, k2).encode, frame("BLPOP", "k1", "k2", "0"))
    assertEquals(blpop[String](k1, k2, k3).encode, frame("BLPOP", "k1", "k2", "k3", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22", "0"))
  }
  test("The Blocking List extended protocol using blpop sends in order from 1 to 21 keys, with a timeout") {
    assertEquals(blpop[String](k1, seconds).encode, frame("BLPOP", "k1", "7"))
    assertEquals(blpop[String](k1, k2, seconds).encode, frame("BLPOP", "k1", "k2", "7"))
    assertEquals(blpop[String](k1, k2, k3, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "7"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, seconds).encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "7"))
  }
  test("The Blocking List extended protocol using brpop sends in order from 1 to 22 keys") {
    assertEquals(brpop[String](k1).encode, frame("BRPOP", "k1", "0"))
    assertEquals(brpop[String](k1, k2).encode, frame("BRPOP", "k1", "k2", "0"))
    assertEquals(brpop[String](k1, k2, k3).encode, frame("BRPOP", "k1", "k2", "k3", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22", "0"))
  }
  test("The Blocking List extended protocol using brpop sends in order from 1 to 21 keys, with a timeout") {
    assertEquals(brpop[String](k1, seconds).encode, frame("BRPOP", "k1", "7"))
    assertEquals(brpop[String](k1, k2, seconds).encode, frame("BRPOP", "k1", "k2", "7"))
    assertEquals(brpop[String](k1, k2, k3, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "7"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, seconds).encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "7"))
  }
}
