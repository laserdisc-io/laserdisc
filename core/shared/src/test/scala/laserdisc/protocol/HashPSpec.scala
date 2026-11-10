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

final case class HashPSpecUser(name: String, age: Int, email: Option[String])

abstract class HashPSpec extends BaseSpec with HashP {
  private def bulks(ss: String*): Arr          = Arr(ss.map(Bulk(_)).toList)
  private final val scanKVToArr: ScanKV => Arr = scanKV =>
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
    assertCompileErrors(
      compileErrors("""hget[Bar](Key("a"), Key("f"))"""),
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
         |hget[Bar](Key("a"), Key("f"))
         |         ^
         |""".stripMargin,
      "Implicit not found Read[laserdisc.protocol.Bulk, laserdisc.Bar]."
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
    assertCompileErrorsContain(
      compileErrors("""hgetall[Seq[Long]](Key("a"))"""),
      "Implicit not found Read[laserdisc.protocol.Arr.OfPairs[laserdisc.protocol.Bulk,laserdisc.protocol.Bulk], Seq[Long]].",
      "Implicit not found Read[laserdisc.protocol.Arr.OfPairs[laserdisc.protocol.Bulk, laserdisc.protocol.Bulk], Seq[Long]]."
    )
  }

  test("The Hash protocol using hgetall fails to compile given a tuple, which has no field names") {
    assert(compileErrors("""hgetall[(String, Int)](Key("a"))""").nonEmpty)
  }

  property("The Hash protocol using hgetall roundtrips successfully given key and specific read instance (Map[Key, String])") {
    forAll { (k: Key, f: Key, v: String) =>
      val protocol = hgetall[Map[Key, String]](k)
      assertEquals(protocol.encode, Arr(Bulk("HGETALL"), Bulk(k)))
      assertEquals(protocol.decode(Arr(Bulk(f), Bulk(v))), Map(f -> v))
    }
  }

  property("The Hash protocol using hgetall roundtrips successfully given key and specific read instance (Seq[(Key, String)])") {
    forAll { (k: Key, f: Key, v: String) =>
      val protocol = hgetall[Seq[(Key, String)]](k)
      assertEquals(protocol.encode, Arr(Bulk("HGETALL"), Bulk(k)))
      assertEquals(protocol.decode(Arr(Bulk(f), Bulk(v))), Seq(f -> v))
    }
  }

  test("The Hash protocol using hgetall reads a case class by field name, in any order, leaving out other fields") {
    val protocol = hgetall[HashPSpecUser](Key("u"))
    assertEquals(
      protocol.decode(Arr(Bulk("age"), Bulk("36"), Bulk("extra"), Bulk("x"), Bulk("name"), Bulk("ada"))),
      HashPSpecUser("ada", 36, None)
    )
    assertEquals(
      protocol.decode(Arr(Bulk("name"), Bulk("ada"), Bulk("email"), Bulk("ada@example.com"), Bulk("age"), Bulk("36"))),
      HashPSpecUser("ada", 36, Some("ada@example.com"))
    )
  }

  test("The Hash protocol using hgetall names the case class field that's missing or doesn't read") {
    val protocol = hgetall[HashPSpecUser](Key("u"))
    assertLeftEquals(protocol.decode(Arr(Bulk("name"), Bulk("ada"))).leftMap(_.getMessage.endsWith("missing field age")), true)
    assertLeftEquals(
      protocol.decode(Arr(Bulk("name"), Bulk("ada"), Bulk("age"), Bulk("x"))).leftMap(_.getMessage),
      "RESP type(s) of Arr(Bulk(name),Bulk(ada),Bulk(age),Bulk(x)) matched but failed to deserialize correctly with error field age: Read Error: expected Bulk(Int) but was Bulk(x)"
    )
  }

  test("The Hash protocol using hset writes a case class field by field, leaving out the fields that are None") {
    assertEquals(
      hset(Key("u"), HashPSpecUser("ada", 36, None)).encode,
      Arr(Bulk("HSET"), Bulk("u"), Bulk("name"), Bulk("ada"), Bulk("age"), Bulk("36"))
    )
    assertEquals(
      hset(Key("u"), HashPSpecUser("ada", 36, Some("ada@example.com"))).encode,
      Arr(Bulk("HSET"), Bulk("u"), Bulk("name"), Bulk("ada"), Bulk("age"), Bulk("36"), Bulk("email"), Bulk("ada@example.com"))
    )
    assertEquals(hset(Key("u"), HashPSpecUser("ada", 36, None)).decode(Num(2L)).map(_.value), 2)
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

  test("The Hash protocol using hmset fails to compile given key and EmptyTuple") {
    assertCompileErrorsContain(
      compileErrors("""hmset(Key("a"), EmptyTuple)"""),
      "match expected type shapeless.HNil <:!< laserdisc.EmptyTuple",
      "error: EmptyTuple must not be a subtype of EmptyTuple.type"
    )
  }

  property("The Hash protocol using hmset roundtrips successfully given key and Tuple of (Key, A) pairs") {
    forAll { (k: Key, f1: Key, i: Int, f2: Key, s: String) =>
      val protocol = hmset(k, (f1 -> i) *: (f2 -> s) *: EmptyTuple)
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
      val protocol = hvals[Int *: EmptyTuple](k)
      assertEquals(protocol.encode, Arr(Bulk("HVALS"), Bulk(k)))
      assertEquals(protocol.decode(Arr(Bulk(i))), i *: EmptyTuple)
    }
  }

  property("The Hash protocol using hvals roundtrips successfully given key (expecting two fields)") {
    forAll { (k: Key, i: Int, s: String) =>
      val protocol = hvals[Int *: String *: EmptyTuple](k)
      assertEquals(protocol.encode, Arr(Bulk("HVALS"), Bulk(k)))
      assertEquals(protocol.decode(Arr(Bulk(i), Bulk(s))), i *: s *: EmptyTuple)
    }
  }

  private def fields(fs: OneOrMoreKeys): List[Bulk] = Bulk("FIELDS") :: Bulk(fs.value.size) :: fs.value.map(Bulk(_))

  property("The Hash protocol using hset with several fields replies how many were added") {
    forAll { (k: Key, fvs: OneOrMore[(Key, Int)], nni: NonNegInt) =>
      val protocol = hset(k, fvs)
      assertEquals(protocol.encode, Arr(Bulk("HSET") :: Bulk(k) :: fvs.value.flatMap { case (f, v) => List(Bulk(f), Bulk(v)) }))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Hash protocol using hrandfieldswithvalues reads fields and values") {
    forAll { (k: Key, c: NonZeroInt, fvs: List[(Key, Int)]) =>
      val protocol = hrandfieldswithvalues[Int](k, c)
      assertEquals(protocol.encode, Arr(Bulk("HRANDFIELD"), Bulk(k), Bulk(c), Bulk("WITHVALUES")))
      assertEquals(protocol.decode(Arr(fvs.flatMap { case (f, v) => List(Bulk(f), Bulk(v)) })), fvs)
      assertEquals(hrandfield(k).decode(NullBulk), None)
    }
  }

  property("The Hash protocol using hexpire with a condition reads what it did to each field") {
    import hashtypes.*
    forAll { (k: Key, s: NonNegLong, fs: OneOrMoreKeys) =>
      val protocol = hexpire(k, s, HashExpireCondition.gt, fs)
      assertEquals(protocol.encode, Arr(Bulk("HEXPIRE") :: Bulk(k) :: Bulk(s) :: Bulk("GT") :: fields(fs)))
      assertEquals(
        protocol.decode(Arr(Num(-2L), Num(0L), Num(1L), Num(2L))),
        Seq(HashNoField, HashFieldNotSet, HashFieldSet, HashFieldDeleted)
      )
    }
  }

  property("The Hash protocol using httl, hpexpiretime and hpersist read each field's reply") {
    import hashtypes.*
    forAll { (k: Key, fs: OneOrMoreKeys, n: NonNegLong) =>
      assertEquals(httl(k, fs).encode, Arr(Bulk("HTTL") :: Bulk(k) :: fields(fs)))
      assertEquals(httl(k, fs).decode(Arr(Num(-2L), Num(-1L), Num(n.value))), Seq(HashNoField, HashNoExpire, HashFieldExpireAfter(n)))
      assertEquals(hpexpiretime(k, fs).decode(Arr(Num(-1L), Num(n.value))), Seq(HashNoExpire, HashFieldExpireAt(n)))
      assertEquals(hpersist(k, fs).decode(Arr(Num(-2L), Num(-1L), Num(1L))), Seq(HashNoField, HashNoExpire, HashFieldPersisted))
    }
  }

  property("The Hash protocol using hgetex and hgetdel read a value or None for each field") {
    forAll { (k: Key, fs: OneOrMoreKeys, s: PosLong, ovs: List[Option[Int]]) =>
      val protocol = hgetex[Int](k, StringP.Expiry.seconds(s), fs)
      assertEquals(protocol.encode, Arr(Bulk("HGETEX") :: Bulk(k) :: Bulk("EX") :: Bulk(s) :: fields(fs)))
      assertEquals(protocol.decode(Arr(ovs.map(_.fold(NullBulk: GenBulk)(Bulk(_))))), ovs)
      assertEquals(hgetdel[Int](k, fs).encode, Arr(Bulk("HGETDEL") :: Bulk(k) :: fields(fs)))
    }
  }

  property("The Hash protocol using hsetex sends the key condition, fields condition and expiry before the fields") {
    import hashtypes.*
    forAll { (k: Key, fvs: OneOrMore[(Key, Int)], b: Boolean) =>
      val protocol = hsetex(k, HashKeyCondition.xx, HashFieldsCondition.fnx, StringP.Expiry.keepTtl, fvs)
      assertEquals(
        protocol.encode,
        Arr(
          List(Bulk("HSETEX"), Bulk(k), Bulk("XX"), Bulk("FNX"), Bulk("KEEPTTL"), Bulk("FIELDS"), Bulk(fvs.value.size)) :::
            fvs.value.flatMap { case (f, v) => List(Bulk(f), Bulk(v)) }
        )
      )
      assertEquals(protocol.decode(Num(if (b) 1L else 0L)), b)
    }
  }

  test("The Hash protocol using hsetex sends a Bulk value as its bytes") {
    val binary = Bulk(scodec.bits.ByteVector(0xff, 0x80))
    assertEquals(
      hsetex(Key("h"), OneOrMore.unsafeFrom(List(Key("f") -> binary))).encode,
      Arr(Bulk("HSETEX"), Bulk("h"), Bulk("FIELDS"), Bulk("1"), Bulk("f"), binary)
    )
  }

  property("The Hash protocol using hscannovalues reads only fields") {
    forAll { (k: Key, c: NonNegLong, fs: List[Key]) =>
      val protocol = hscannovalues(k, c)
      assertEquals(protocol.encode, Arr(Bulk("HSCAN"), Bulk(k), Bulk(c), Bulk("NOVALUES")))
      assertEquals(protocol.decode(Arr(Bulk(0L), Arr(fs.map(Bulk(_))))), Scan(NonNegLong(0L), Some(fs)))
    }
  }

  property("The Hash protocol using hexpireat and hpexpire send the time and any condition before the fields") {
    import hashtypes.*
    forAll { (k: Key, t: NonNegLong, fs: OneOrMoreKeys) =>
      val reply    = Arr(Num(1L))
      val expected = Seq(HashFieldSet)
      assertEquals(hexpireat(k, t, fs).encode, Arr(Bulk("HEXPIREAT") :: Bulk(k) :: Bulk(t) :: fields(fs)))
      assertEquals(hexpireat(k, t, fs).decode(reply), expected)
      assertEquals(
        hexpireat(k, t, HashExpireCondition.nx, fs).encode,
        Arr(Bulk("HEXPIREAT") :: Bulk(k) :: Bulk(t) :: Bulk("NX") :: fields(fs))
      )
      assertEquals(hexpireat(k, t, HashExpireCondition.nx, fs).decode(reply), expected)
      assertEquals(hpexpire(k, t, fs).encode, Arr(Bulk("HPEXPIRE") :: Bulk(k) :: Bulk(t) :: fields(fs)))
      assertEquals(hpexpire(k, t, fs).decode(reply), expected)
      assertEquals(
        hpexpire(k, t, HashExpireCondition.xx, fs).encode,
        Arr(Bulk("HPEXPIRE") :: Bulk(k) :: Bulk(t) :: Bulk("XX") :: fields(fs))
      )
      assertEquals(hpexpire(k, t, HashExpireCondition.xx, fs).decode(reply), expected)
    }
  }

  test("The Hash protocol using hscannovalues sends each of its options") {
    val (k, c, g, n) = (Key("k"), NonNegLong(5L), GlobPattern("a*"), PosInt(2))
    val reply        = Arr(Bulk("0"), Arr(Bulk("a")))
    val scan         = Scan(NonNegLong(0L), Some(Seq(Key("a"))))

    assertEquals(hscannovalues(k, c, g).encode, bulks("HSCAN", "k", "5", "MATCH", "a*", "NOVALUES"))
    assertEquals(hscannovalues(k, c, n).encode, bulks("HSCAN", "k", "5", "COUNT", "2", "NOVALUES"))
    assertEquals(hscannovalues(k, c, g, n).encode, bulks("HSCAN", "k", "5", "MATCH", "a*", "COUNT", "2", "NOVALUES"))
    assertEquals(hscannovalues(k, c, g).decode(reply), scan)
    assertEquals(hscannovalues(k, c, g, n).decode(reply), scan)
  }
}
