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

final class RedisSetClientSpec extends SetClientSpec(LiveServers.port(6379), "redis") {
  since[Redis7_0]("a redis client counts intersections") { v =>
    import v.*

    send(del("s:a", "s:b"))
    send(sadd("s:a", OneOrMore.unsafeFrom(List(1, 2, 3, 4, 5))))
    send(sadd("s:b", OneOrMore.unsafeFrom(List(4, 5, 6, 7))))

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("s:a"), Key("s:b")))
    assertEquals(send(sintercard(ks)).value, 2)
    assertEquals(send(sintercard(ks, 1)).value, 1)
  }

  since[Redis8_10]("a redis client counts differences and unions") { v =>
    import v.*

    send(del("s:a", "s:b"))
    send(sadd("s:a", OneOrMore.unsafeFrom(List(1, 2, 3, 4, 5))))
    send(sadd("s:b", OneOrMore.unsafeFrom(List(4, 5, 6, 7))))

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("s:a"), Key("s:b")))
    assertEquals(send(sdiffcard(ks)).value, 3)
    assertEquals(send(sdiffcard(ks, 2)).value, 2)
    assertEquals(send(sunioncard(ks)).value, 7)
    assertEquals(send(sunioncard(ks, 3)).value, 3)
    assertEquals(send(sunioncard(ks, settypes.approx, 3)).value, 3)
    assert(send(sunioncard(ks, settypes.approx)).value > 0)
  }
}

final class KeyDbSetClientSpec extends SetClientSpec(LiveServers.port(6380), "keyDb")

final class ValkeySetClientSpec extends SetClientSpec(LiveServers.port(6381), "valkey") {
  since[Valkey7_2]("a valkey client counts intersections") { v =>
    import v.*

    send(del("s:a", "s:b"))
    send(sadd("s:a", OneOrMore.unsafeFrom(List(1, 2, 3))))
    send(sadd("s:b", OneOrMore.unsafeFrom(List(2, 3, 4))))

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("s:a"), Key("s:b")))
    assertEquals(send(sintercard(ks)).value, 2)
    assertEquals(send(sintercard(ks, 1)).value, 1)
  }
}

final class DragonflySetClientSpec extends SetClientSpec(LiveServers.port(6382), "dragonfly")

abstract class SetClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  test(s"a $dest client checks several members at once and pops from a missing set") {
    import laserdisc.redis_6_2.*

    send(del("s:m", "s:none"))
    send(sadd("s:m", OneOrMore.unsafeFrom(List("a", "b"))))

    assertEquals(send(smismember("s:m", OneOrMore.unsafeFrom(List("a", "z", "b")))), Seq(true, false, true))
    assertEquals(send(smismember("s:none", OneOrMore.unsafeFrom(List("a")))), Seq(false))
    assertEquals(send(spop[String]("s:none", 2)), Seq.empty)
    assertEquals(send(srandmembers[String]("s:none", 2)), Seq.empty)
  }

  test(s"a $dest client adds, moves, removes and combines members") {
    import laserdisc.redis_6_2.*

    send(del("s:x", "s:y", "s:d", "s:e"))
    assertEquals(send(scard("s:x")), None)
    assertEquals(send(srandmember[String]("s:x")), None)
    assertEquals(send(sadd("s:x", "a", "b", "c")).value, 3)
    assertEquals(send(sadd("s:y", "b", "c", "d")).value, 3)

    assertEquals(send(scard("s:x")).map(_.value), Some(3))
    assert(send(sismember("s:x", "a")))
    assert(!send(sismember("s:x", "z")))
    assertEquals(send(smembers[String]("s:x")).toSet, Set("a", "b", "c"))
    assert(send(srandmember[String]("s:x")).exists(Set("a", "b", "c")))
    assertEquals(send(srandmembers[String]("s:x", 2)).size, 2)

    val ks = TwoOrMoreKeys.unsafeFrom(List(Key("s:x"), Key("s:y")))
    assertEquals(send(sdiff[String](ks)), Seq("a"))
    assertEquals(send(sinter[String](ks)).toSet, Set("b", "c"))
    assertEquals(send(sunion[String](ks)).toSet, Set("a", "b", "c", "d"))
    assertEquals(send(sdiffstore(ks, "s:d")).value, 1)
    assertEquals(send(sinterstore(ks, "s:d")).value, 2)
    assertEquals(send(sunionstore(ks, "s:d")).value, 4)

    assert(send(smove("s:x", "s:e", "a")))
    assert(!send(smove("s:x", "s:e", "a")))
    assertEquals(send(srem("s:x", "b", "z")).value, 1)
  }
}
