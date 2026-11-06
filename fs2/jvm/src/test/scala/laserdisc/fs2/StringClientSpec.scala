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
package fs2

import laserdisc.auto.*
import laserdisc.protocol.KeyP.TTLResponse
import laserdisc.protocol.StringP.{Bit, LcsMatch, LcsRange}

final class RedisStringClientSpec extends StringClientSpec(6379, "redis") {
  test("a redis client compares strings and counts bits with the commands of Redis 8.10")(lcsAndBits(redis_8_10))

  test("a redis client deletes on a match, sets several keys with an expiry and increments within bounds with the commands of Redis 8.10") {
    import laserdisc.redis_8_10.*
    import laserdisc.protocol.StringP.Incremented

    send(del("s:a", "s:b", "s:n", "s:f"))
    send(set("s:a", "hello"))
    val hash = send(digest("s:a")).getOrElse(fail("expected a digest"))
    assertEquals(send(digest("s:none")), None)
    assert(!send(delex("s:a", stringtypes.compare.ifNe("hello"))))
    assert(send(delex("s:a", stringtypes.compare.ifDeq(hash))))
    assert(!send(delex("s:a")))

    assert(
      send(msetex(OneOrMore.unsafeFrom(List(Key("s:a") -> 1, Key("s:b") -> 2)), stringtypes.flag.nx, stringtypes.expiry.seconds(100L)))
    )
    assert(!send(msetex(OneOrMore.unsafeFrom(List(Key("s:a") -> 1)), stringtypes.flag.nx)))

    assertEquals(send(increx("s:n")), Incremented(1L, 1L))
    assertEquals(send(increx("s:n", 5L, stringtypes.increx.int.expiry(stringtypes.expiry.seconds(100L)))), Incremented(6L, 5L))
    assertEquals(send(increx("s:n", 5L, stringtypes.increx.int.upperBound(7L))), Incremented(6L, 0L))
    assertEquals(send(increx("s:n", 5L, stringtypes.increx.int.upperBound(7L).saturate)), Incremented(7L, 1L))
    assertEquals(send(increx("s:f", 1.5, stringtypes.increx.float.lowerBound(0.0))), Incremented(1.5, 1.5))
  }

  test("a redis client combines bits with DIFF, DIFF1, ANDOR and ONE with the commands of Redis 8.10") {
    import laserdisc.redis_8_10.*

    send(del("s:x", "s:y", "s:z", "s:d"))
    val bf = stringtypes.bitfield
    send(bitfield("s:x", bf.set(bf.u(8), bf.bit(0L), 0xf0L)))
    send(bitfield("s:y", bf.set(bf.u(8), bf.bit(0L), 0x3cL)))
    send(bitfield("s:z", bf.set(bf.u(8), bf.bit(0L), 0x0fL)))
    val xy  = TwoOrMoreKeys.unsafeFrom(List(Key("s:x"), Key("s:y")))
    val xyz = TwoOrMoreKeys.unsafeFrom(List(Key("s:x"), Key("s:y"), Key("s:z")))
    val u8  = OneOrMore.unsafeFrom(List(bf.u(8) -> bf.bit(0L)))

    send(bitop(stringtypes.bitwise.diff, xy, "s:d"))
    assertEquals(send(bitfieldro("s:d", u8)), Seq(0xc0L))
    send(bitop(stringtypes.bitwise.diff1, xy, "s:d"))
    assertEquals(send(bitfieldro("s:d", u8)), Seq(0x0cL))
    send(bitop(stringtypes.bitwise.andor, xyz, "s:d"))
    assertEquals(send(bitfieldro("s:d", u8)), Seq(0x30L))
    send(bitop(stringtypes.bitwise.one, xyz, "s:d"))
    assertEquals(send(bitfieldro("s:d", u8)), Seq(0xc3L))
  }
}

final class KeyDbStringClientSpec extends StringClientSpec(6380, "keyDb") {
  test("a keyDb client compares strings with STRALGO") {
    import laserdisc.redis_6_2.*
    import stringtypes.strAlgoInput

    send(del("s:a1", "s:a2"))
    send(set("s:a1", "ohmytext"))
    send(set("s:a2", "mynewtext"))
    val keys    = strAlgoInput.keys("s:a1", "s:a2")
    val strings = strAlgoInput.strings("ohmytext", "mynewtext")
    assertEquals(send(stralgolcs[String](keys)), "mytext")
    assertEquals(send(stralgolcs[String](strings)), "mytext")
    assertEquals(send(stralgolcslen(keys)).value, 6)
    assertEquals(send(stralgolcslen(strings)).value, 6)
    assertEquals(send(stralgolcsidx(keys)).matches.size, 2)
    val matches = send(stralgolcsidx(strings, 4))
    assertEquals(matches.length.value, 6)
    assertEquals(matches.matches, Seq(LcsMatch(LcsRange(NonNegInt(4), NonNegInt(7)), LcsRange(NonNegInt(5), NonNegInt(8)), NonNegInt(4))))
  }
}

final class ValkeyStringClientSpec extends StringClientSpec(6381, "valkey") {
  test("a valkey client compares strings and counts bits with the commands of Valkey 9.1")(lcsAndBits(valkey_9_1))

  test("a valkey client deletes on a match and sets several keys with an expiry with the commands of Valkey 9.1") {
    import laserdisc.valkey_9_1.*

    send(del("s:a", "s:b"))
    send(set("s:a", "hello"))
    assert(!send(delifeq("s:a", "nope")))
    assert(send(delifeq("s:a", "hello")))
    assert(
      send(msetex(OneOrMore.unsafeFrom(List(Key("s:a") -> 1, Key("s:b") -> 2)), stringtypes.flag.nx, stringtypes.expiry.millis(100000L)))
    )
    assert(send(msetex(OneOrMore.unsafeFrom(List(Key("s:a") -> 3)), stringtypes.flag.xx, stringtypes.expiry.keepTtl)))
  }
}

final class DragonflyStringClientSpec extends StringClientSpec(6382, "dragonfly")

abstract class StringClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  protected final def lcsAndBits(version: Redis7_0): Unit = {
    import version.*

    send(del("s:l1", "s:l2", "s:b"))
    send(set("s:l1", "ohmytext"))
    send(set("s:l2", "mynewtext"))
    assertEquals(send(lcs[String]("s:l1", "s:l2")), "mytext")
    assertEquals(send(lcslen("s:l1", "s:l2")).value, 6)
    val matches = send(lcsidx("s:l1", "s:l2", 4))
    assertEquals(matches.length.value, 6)
    assertEquals(matches.matches, Seq(LcsMatch(LcsRange(NonNegInt(4), NonNegInt(7)), LcsRange(NonNegInt(5), NonNegInt(8)), NonNegInt(4))))
    assertEquals(send(lcsidx("s:l1", "s:l2")).matches.size, 2)

    send(setbit("s:b", 3L, stringtypes.bit.set))
    send(setbit("s:b", 9L, stringtypes.bit.set))
    assertEquals(send(bitcount("s:b", 0L, 7L, stringtypes.bitUnit.bit)).value, 1)
    assertEquals(send(bitcount("s:b", 0L, 0L, stringtypes.bitUnit.byte)).value, 1)
    assertEquals(send(bitpos("s:b", stringtypes.bit.set, 4L, 15L, stringtypes.bitUnit.bit)).map(_.value), Some(9))
  }

  test(s"a $dest client reads, writes and increments numbers in a string with BITFIELD") {
    import laserdisc.redis_6_2.*

    send(del("s:f"))
    val bf = stringtypes.bitfield
    assertEquals(
      send(bitfield("s:f", bf.set(bf.u(8), bf.bit(0L), 255L).get(bf.u(8), bf.bit(0L)).incrBy(bf.u(8), bf.bit(0L), 10L))),
      Seq(Some(0L), Some(255L), Some(9L))
    )
    assertEquals(
      send(bitfield("s:f", bf.overflow(bf.fail).incrBy(bf.u(8), bf.bit(0L), 300L).overflow(bf.sat).incrBy(bf.u(8), bf.bit(0L), 300L))),
      Seq(None, Some(255L))
    )
    assertEquals(send(bitfieldro("s:f", OneOrMore.unsafeFrom(List(bf.u(8) -> bf.bit(0L), bf.u(4) -> bf.field(1L))))), Seq(255L, 15L))
  }

  test(s"a $dest client reads keys that may be missing with mget") {
    import laserdisc.redis_6_2.*

    send(del("s:m1", "s:m2"))
    send(set("s:m1", 1))
    val keys = OneOrMoreKeys.unsafeFrom(List(Key("s:m1"), Key("s:m2")))
    assertEquals(send(mget[Int *: Option[Int] *: EmptyTuple](keys)), 1 *: Option.empty[Int] *: EmptyTuple)
    assertEquals(send(mget[Seq[Option[Int]]](keys)), Seq(Some(1), None))
  }

  test(s"a $dest client appends, counts up and down and works on parts of a string") {
    import laserdisc.redis_6_2.*

    send(del("s:a", "s:n", "s:f", "s:r", "s:b", "s:d"))
    assertEquals(send(append("s:a", "foo")).value, 3)
    assertEquals(send(append("s:a", "bar")).value, 6)
    assertEquals(send(getrange[String]("s:a", 1L, 3L)), "oob")
    assertEquals(send(setrange("s:a", 3, "BAR")).value, 6)
    assertEquals(send(get[String]("s:a")), Some("fooBAR"))

    send(set("s:n", 10))
    assertEquals(send(incrby[Long]("s:n", 5L)), 15L)
    assertEquals(send(decrby[Long]("s:n", 3L)), 12L)
    assertEquals(send(decr[Long]("s:n")), 11L)
    send(set("s:f", 1.5))
    assertEquals(send(incrbyfloat("s:f", 1.25)), 2.75)

    send(setbit("s:b", 3L, stringtypes.bit.set))
    assertEquals(send(getbit("s:b", 3L)), stringtypes.bit.set: Bit)
    assertEquals(send(getbit("s:b", 4L)), stringtypes.bit.unset: Bit)
    assertEquals(send(getbit("s:b", 0L)), stringtypes.bit.unset: Bit)
    send(setbit("s:b", 0L, stringtypes.bit.set))
    assertEquals(send(getbit("s:b", 0L)), stringtypes.bit.set: Bit)
    assertEquals(send(bitopnot("s:b", "s:d")).value, 1)
  }

  test(s"a $dest client sets only what is missing, and sets with an expiry") {
    import laserdisc.redis_6_2.*

    send(del("s:nx", "s:mx", "s:ex", "s:px"))
    assert(send(setnx("s:nx", 1)))
    assert(!send(setnx("s:nx", 2)))
    assert(send(msetnx(OneOrMore.unsafeFrom(List(Key("s:mx") -> 1)))))
    assert(!send(msetnx(OneOrMore.unsafeFrom(List(Key("s:mx") -> 2)))))

    assertEquals(send(setex("s:ex", "v", 100L)), OK)
    send(ttl("s:ex")) match {
      case TTLResponse.ExpireAfter(left) => assert(left.value <= 100L)
      case other                         => fail(s"expected a TTL, got $other")
    }
    assertEquals(send(psetex("s:px", 100000L, "v")), OK)
    send(pttl("s:px")) match {
      case TTLResponse.ExpireAfter(left) => assert(left.value <= 100000L)
      case other                         => fail(s"expected a TTL, got $other")
    }
  }
}
