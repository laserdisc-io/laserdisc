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

final class StringPSpec extends BaseSpec with StringP {
  import laserdisc.auto.*

  test("The String protocol sends a Bulk value as its bytes, even ones that aren't UTF-8, through Show and through Value") {
    val binary                                             = Bulk(scodec.bits.ByteVector(0xff, 0x80))
    def value(sent: RESP, at: Int): scodec.bits.ByteVector = sent match {
      case Arr(elements) => elements(at) match { case bulk: Bulk => bulk.bytes; case other => fail(s"not a Bulk: $other") }
      case other         => fail(s"not an Arr: $other")
    }
    assertEquals(value(redis_8_10.set(Key("k"), binary).encode, 2), binary.bytes)
    assertEquals(value(redis_8_10.setget[String](Key("k"), binary).encode, 2), binary.bytes)
    assertEquals(value(redis_8_10.hset(Key("h"), Key("f"), binary).encode, 3), binary.bytes)
    assertEquals(
      value(redis_8_10.evalargs[Bulk, String]("return ARGV[1]", OneOrMore.unsafeFrom(List[Value](binary))).encode, 3),
      binary.bytes
    )
  }

  test("The String protocol sends a Bulk as its bytes in a comparison and in the strings LCS compares too") {
    val binary = Bulk(scodec.bits.ByteVector(0xff, 0x80))
    assertEquals(delex(Key("k"), stringtypes.compare.ifNe(binary)).encode, Arr(Bulk("DELEX"), Bulk("k"), Bulk("IFNE"), binary))
    assertEquals(delex(Key("k"), stringtypes.ifEq(binary)).encode, Arr(Bulk("DELEX"), Bulk("k"), Bulk("IFEQ"), binary))
    assertEquals(
      stralgolcs[String](StringP.StrAlgoInput.strings(binary, binary)).encode,
      Arr(Bulk("STRALGO"), Bulk("LCS"), Bulk("STRINGS"), binary, binary)
    )
  }

  test("The String protocol decoding the wrong type gives details about the decoding error") {
    val correct = redis_8_10.strings.set("a", 23)
    assertLeftEquals(
      correct.decode(Arr(Bulk("wrong type"))).leftMap(_.getMessage),
      "RESP type(s) did not match: Arr(Bulk(wrong type))"
    )
  }

  test("The String protocol decoding the correct type with the wrong encoding gives details about the decoding error") {
    val correct = redis_8_10.strings.set("a", 23)
    assertLeftEquals(
      correct.decode(Str("wrong")).leftMap(_.getMessage),
      "RESP type(s) of Str(wrong) matched but failed to deserialize correctly with error Read Error: expected Str(OK) but was Str(wrong)"
    )
  }

  property("The String protocol using append reads the new length") {
    forAll { (k: Key, v: String, n: NonNegInt) =>
      assertEquals(append(k, v).encode, Arr(Bulk("APPEND"), Bulk(k), Bulk(v)))
      assertEquals(append(k, v).decode(Num(n.value.toLong)), n)
    }
  }

  property("The String protocol using decr, decrby and incrby reads the new number") {
    forAll { (k: Key, by: PosLong, n: Long) =>
      assertEquals(decr[Long](k).encode, Arr(Bulk("DECR"), Bulk(k)))
      assertEquals(decr[Long](k).decode(Num(n)), n)
      assertEquals(decrby[Long](k, by).encode, Arr(Bulk("DECRBY"), Bulk(k), Bulk(by)))
      assertEquals(decrby[Long](k, by).decode(Num(n)), n)
      assertEquals(incrby[Long](k, by).encode, Arr(Bulk("INCRBY"), Bulk(k), Bulk(by)))
      assertEquals(incrby[Long](k, by).decode(Num(n)), n)
    }
  }

  property("The String protocol using incrbyfloat reads the new number from a bulk string") {
    forAll { (k: Key, by: NonZeroDouble, d: ValidDouble) =>
      val protocol = incrbyfloat(k, by)
      assertEquals(protocol.encode, Arr(Bulk("INCRBYFLOAT"), Bulk(k), Bulk(by)))
      assertEquals(protocol.decode(Bulk(d)), d.value)
    }
  }

  property("The String protocol using getbit reads a bit") {
    forAll { (k: Key, o: StringLength) =>
      assertEquals(getbit(k, o).encode, Arr(Bulk("GETBIT"), Bulk(k), Bulk(o)))
      assertEquals(getbit(k, o).decode(Num(0L)), stringtypes.bit.unset)
      assertEquals(getbit(k, o).decode(Num(1L)), stringtypes.bit.set)
    }
  }

  property("The String protocol using getrange and setrange sends the positions") {
    forAll { (k: Key, s: Index, e: Index, o: RangeOffset, v: String, n: NonNegInt) =>
      assertEquals(getrange[String](k, s, e).encode, Arr(Bulk("GETRANGE"), Bulk(k), Bulk(s), Bulk(e)))
      assertEquals(getrange[String](k, s, e).decode(Bulk(v)), v)
      assertEquals(setrange(k, o, v).encode, Arr(Bulk("SETRANGE"), Bulk(k), Bulk(o), Bulk(v)))
      assertEquals(setrange(k, o, v).decode(Num(n.value.toLong)), n)
    }
  }

  property("The String protocol using setex and psetex sends the expiry before the value") {
    forAll { (k: Key, t: PosLong, v: String) =>
      assertEquals(setex(k, v, t).encode, Arr(Bulk("SETEX"), Bulk(k), Bulk(t), Bulk(v)))
      assertEquals(setex(k, v, t).decode(Str(OK.value)), OK)
      assertEquals(psetex(k, t, v).encode, Arr(Bulk("PSETEX"), Bulk(k), Bulk(t), Bulk(v)))
      assertEquals(psetex(k, t, v).decode(Str(OK.value)), OK)
    }
  }

  property("The String protocol using setnx and msetnx read whether they set anything") {
    forAll { (k: Key, v: String) =>
      assertEquals(setnx(k, v).encode, Arr(Bulk("SETNX"), Bulk(k), Bulk(v)))
      assertEquals(setnx(k, v).decode(Num(1L)), true)
      assertEquals(setnx(k, v).decode(Num(0L)), false)
      val values = OneOrMore.unsafeFrom(List(k -> v))
      assertEquals(msetnx(values).encode, Arr(Bulk("MSETNX"), Bulk(k), Bulk(v)))
      assertEquals(msetnx(values).decode(Num(1L)), true)
      assertEquals(msetnx(values).decode(Num(0L)), false)
    }
  }

  property("The String protocol using bitopnot sends the destination before the key") {
    forAll { (k: Key, d: Key, n: NonNegInt) =>
      assertEquals(bitopnot(k, d).encode, Arr(Bulk("BITOP"), Bulk("NOT"), Bulk(d), Bulk(k)))
      assertEquals(bitopnot(k, d).decode(Num(n.value.toLong)), n)
    }
  }
}
