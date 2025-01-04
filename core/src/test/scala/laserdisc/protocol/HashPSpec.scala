/*
 * Copyright (c) 2018-2025 LaserDisc
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

abstract class HashPSpec extends BaseSpec with HashP {
  import shapeless._

  private[this] final val scanKVToArr: ScanKV => Arr = scanKV =>
    Arr(
      Bulk(scanKV.cursor.value),
      scanKV.maybeValues.fold(NilArr: GenArr)(kvs => Arr(kvs.flatMap { case KV(k, v) => List(Bulk(k.value), Bulk(v)) }.toList))
    )

  property("The Hash protocol using hdel roundtrips successfully given key and fields") {
    forAll { (k: Key, fs: OneOrMoreKeys, nni: NonNegInt) =>
      val protocol = hdel(k, fs)
      assertEquals(protocol.encode, Arr(Bulk("HDEL") :: Bulk(k) :: fs.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Hash protocol using hexists roundtrips successfully given key and field") {
    forAll { (k: Key, f: Key, b: Boolean) =>
      val protocol = hexists(k, f)
      assertEquals(protocol.encode, Arr(Bulk("HEXISTS"), Bulk(k), Bulk(f)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  test("The Hash protocol using hget fails to compile given key and field but missing read instance") {
    assertNoDiff(
      compileErrors("""hget[Bar](Key("a"), Key("f"))"""),
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
         |hget[Bar](Key("a"), Key("f"))
         |         ^
         |""".stripMargin
    )
  }

  property("The Hash protocol using hget roundtrips successfully given key and field") {
    forAll { (k: Key, f: Key, s: String) =>
      val protocol = hget(k, f)
      assertEquals(protocol.encode, Arr(Bulk("HGET"), Bulk(k), Bulk(f)))
      assertEquals(protocol.decode(Bulk(s)), Some(Bulk(s)))
    }
  }

  property("The Hash protocol using hget roundtrips successfully given key, field and specific read instance") {
    forAll { (k: Key, f: Key, i: Int) =>
      val protocol = hget[Foo](k, f)
      assertEquals(protocol.encode, Arr(Bulk("HGET"), Bulk(k), Bulk(f)))
      assertEquals(protocol.decode(Bulk(i)), Some(Foo(i)))
    }
  }

  test("The Hash protocol using hgetall fails to compile given key but missing read instance") {
    assertNoDiff(
      compileErrors("""hgetall[Map[String, Int]](Key("a"))"""),
      """|error:
         |Implicit not found Read[laserdisc.protocol.Arr, Map[String,Int]].
         |
         |Try writing your own, for example:
         |
         |implicit final val myRead: Read[laserdisc.protocol.Arr, Map[String,Int]] = new Read[laserdisc.protocol.Arr, Map[String,Int]] {
         |  override final def read(a: laserdisc.protocol.Arr): Option[Map[String,Int]] = ???
         |}
         |
         |Note 1: you can use the factory method Read.instance instead of creating it manually as shown above
         |Note 2: make sure to inspect the combinators as you may be able to leverage some other Read instance
         |
         |hgetall[Map[String, Int]](Key("a"))
         |                         ^
         |""".stripMargin
    )
  }

  property("The Hash protocol using hgetall roundtrips successfully given key") {
    forAll { (k: Key, f: Key, v: String) =>
      val protocol = hgetall(k)
      assertEquals(protocol.encode, Arr(Bulk("HGETALL"), Bulk(k)))
      assertEquals(protocol.decode(Arr(Bulk(f), Bulk(v))), Arr(Bulk(f), Bulk(v)))
    }
  }

  property("The Hash protocol using hgetall roundtrips successfully given key and specific read instance (Map[Key, String])") {
    forAll { (k: Key, f: Key, v: String) =>
      val protocol = hgetall[Map[Key, String]](k)
      assertEquals(protocol.encode, Arr(Bulk("HGETALL"), Bulk(k)))
      assertEquals(protocol.decode(Arr(Bulk(f), Bulk(v))), Map(f -> v))
    }
  }

  property("The Hash protocol using hgetall roundtrips successfully given key and specific read instance (Key :: String :: HNil)") {
    forAll { (k: Key, f: Key, v: String) =>
      val protocol = hgetall[Key :: String :: HNil](k)
      assertEquals(protocol.encode, Arr(Bulk("HGETALL"), Bulk(k)))
      assertEquals(protocol.decode(Arr(Bulk(f), Bulk(v))), f :: v :: HNil)
    }
  }

  property("The Hash protocol using hincrby roundtrips successfully given key, field and long increment") {
    forAll { (k: Key, f: Key, nzl: NonZeroLong, l: Long) =>
      val protocol = hincrby(k, f, nzl)
      assertEquals(protocol.encode, Arr(Bulk("HINCRBY"), Bulk(k), Bulk(f), Bulk(nzl)))
      assertEquals(protocol.decode(Num(l)), l)
    }
  }

  property("The Hash protocol using hincrby roundtrips successfully given key, field and double increment") {
    forAll { (k: Key, f: Key, nzd: NonZeroDouble, d: Double) =>
      val protocol = hincrby(k, f, nzd)
      assertEquals(protocol.encode, Arr(Bulk("HINCRBYFLOAT"), Bulk(k), Bulk(f), Bulk(nzd)))
      assertEquals(protocol.decode(Bulk(d)), d)
    }
  }

  property("The Hash protocol using hkeys roundtrips successfully given key") {
    forAll { (k: Key, ks: List[Key]) =>
      val protocol = hkeys(k)
      assertEquals(protocol.encode, Arr(Bulk("HKEYS"), Bulk(k)))
      assertEquals(protocol.decode(Arr(ks.map(Bulk(_)))), ks)
    }
  }

  property("The Hash protocol using hlen roundtrips successfully given key") {
    forAll { (k: Key, nni: NonNegInt) =>
      val protocol = hlen(k)
      assertEquals(protocol.encode, Arr(Bulk("HLEN"), Bulk(k)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Hash protocol using hmget roundtrips successfully given key and one field") {
    forAll { (k: Key, f: Key, i: Int) =>
      val protocol = hmget[Int](k, f)
      assertEquals(protocol.encode, Arr(Bulk("HMGET"), Bulk(k), Bulk(f)))
      assertEquals(protocol.decode(Arr(Bulk(i))), i)
    }
  }

  property("The Hash protocol using hmget roundtrips successfully given key and two fields") {
    forAll { (k: Key, f1: Key, f2: Key, i: Int, s: String) =>
      val protocol = hmget[Int, String](k, f1, f2)
      assertEquals(protocol.encode, Arr(Bulk("HMGET"), Bulk(k), Bulk(f1), Bulk(f2)))
      assertEquals(protocol.decode(Arr(Bulk(i), Bulk(s))), i -> s)
    }
  }

  test("The Hash protocol using hmset fails to compile given key and HNil") {
    assertNoDiff(
      compileErrors("""hmset(Key("a"), HNil)"""),
      """|error:
         |Implicit not found RESPParamWrite[shapeless.HNil.type].
         |
         |Normally you would not need to define one manually, as one will be derived for you automatically iff:
         |- an instance of Show[shapeless.HNil.type] is in scope
         |- shapeless.HNil.type is a List whose LUB has a RESPParamWrite instance defined
         |- shapeless.HNil.type is an HList whose elements all have a RESPParamWrite instance defined
         |
         |hmset(Key("a"), HNil)
         |     ^
         |""".stripMargin
    )
  }

  property("The Hash protocol using hmset roundtrips successfully given key and HList of (Key, A) pairs") {
    forAll { (k: Key, f1: Key, i: Int, f2: Key, s: String) =>
      val protocol = hmset(k, (f1 -> i) :: (f2 -> s) :: HNil)
      assertEquals(protocol.encode, Arr(Bulk("HMSET"), Bulk(k), Bulk(f1), Bulk(i), Bulk(f2), Bulk(s)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Hash protocol using hmset roundtrips successfully given key and a Product (Baz)") {
    forAll { (k: Key, baz: Baz) =>
      val protocol = hmset(k, baz)
      assertEquals(protocol.encode, Arr(Bulk("HMSET"), Bulk(k), Bulk("f1"), Bulk(baz.f1), Bulk("f2"), Bulk(baz.f2)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Hash protocol using hscan roundtrips successfully given key and cursor") {
    forAll { (k: Key, nnl: NonNegLong, skv: ScanKV) =>
      val protocol = hscan(k, nnl)
      assertEquals(protocol.encode, Arr(Bulk("HSCAN"), Bulk(k), Bulk(nnl)))
      assertEquals(protocol.decode(scanKVToArr(skv)), skv)
    }
  }

  property("The Hash protocol using hscan roundtrips successfully given key, cursor and glob pattern") {
    forAll { (k: Key, nnl: NonNegLong, g: GlobPattern, skv: ScanKV) =>
      val protocol = hscan(k, nnl, g)
      assertEquals(protocol.encode, Arr(Bulk("HSCAN"), Bulk(k), Bulk(nnl), Bulk("MATCH"), Bulk(g)))
      assertEquals(protocol.decode(scanKVToArr(skv)), skv)
    }
  }

  property("The Hash protocol using hscan roundtrips successfully given key, cursor and count") {
    forAll { (k: Key, nnl: NonNegLong, pi: PosInt, skv: ScanKV) =>
      val protocol = hscan(k, nnl, pi)
      assertEquals(protocol.encode, Arr(Bulk("HSCAN"), Bulk(k), Bulk(nnl), Bulk("COUNT"), Bulk(pi)))
      assertEquals(protocol.decode(scanKVToArr(skv)), skv)
    }
  }

  property("The Hash protocol using hscan roundtrips successfully given key, cursor, glob pattern and count") {
    forAll { (k: Key, nnl: NonNegLong, g: GlobPattern, pi: PosInt, skv: ScanKV) =>
      val protocol = hscan(k, nnl, g, pi)
      assertEquals(protocol.encode, Arr(Bulk("HSCAN"), Bulk(k), Bulk(nnl), Bulk("MATCH"), Bulk(g), Bulk("COUNT"), Bulk(pi)))
      assertEquals(protocol.decode(scanKVToArr(skv)), skv)
    }
  }

  property("The Hash protocol using hset roundtrips successfully given key, field and value") {
    forAll { (k: Key, f: Key, v: Int, b: Boolean) =>
      val protocol = hset(k, f, v)
      assertEquals(protocol.encode, Arr(Bulk("HSET"), Bulk(k), Bulk(f), Bulk(v)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  property("The Hash protocol using hsetnx roundtrips successfully given key, field and value") {
    forAll { (k: Key, f: Key, v: Int, b: Boolean) =>
      val protocol = hsetnx(k, f, v)
      assertEquals(protocol.encode, Arr(Bulk("HSETNX"), Bulk(k), Bulk(f), Bulk(v)))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  property("The Hash protocol using hstrlen roundtrips successfully given key and field") {
    forAll { (k: Key, f: Key, nni: NonNegInt) =>
      val protocol = hstrlen(k, f)
      assertEquals(protocol.encode, Arr(Bulk("HSTRLEN"), Bulk(k), Bulk(f)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Hash protocol using hvals roundtrips successfully given key (expecting one field)") {
    forAll { (k: Key, i: Int) =>
      val protocol = hvals[Int :: HNil](k)
      assertEquals(protocol.encode, Arr(Bulk("HVALS"), Bulk(k)))
      assertEquals(protocol.decode(Arr(Bulk(i))), i :: HNil)
    }
  }

  property("The Hash protocol using hvals roundtrips successfully given key (expecting two fields)") {
    forAll { (k: Key, i: Int, s: String) =>
      val protocol = hvals[Int :: String :: HNil](k)
      assertEquals(protocol.encode, Arr(Bulk("HVALS"), Bulk(k)))
      assertEquals(protocol.decode(Arr(Bulk(i), Bulk(s))), i :: s :: HNil)
    }
  }
}
