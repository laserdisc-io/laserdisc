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

final class RedisListClientSpec extends ListClientSpec(LiveServers.port(6379), "redis") {
  since[Redis8_10]("a redis client moves several values at once") { v =>
    import v.*
    import listtypes.*

    send(del("l:s", "l:d"))
    send(rpush("l:s", OneOrMore.unsafeFrom(List("a", "b", "c", "d"))))

    assertEquals(
      send(lmovem[String]("l:s", "l:d", ListSide.left, ListSide.left, ListAmount.upTo(2), ListOrder.asBlock)),
      Some(Seq("a", "b"))
    )
    assertEquals(send(lmovem[String]("l:s", "l:d", ListSide.left, ListSide.left, ListAmount.exactly(5), ListOrder.asBlock)), None)
    assertEquals(
      send(blocking.blmovem[String]("l:s", "l:d", ListSide.left, ListSide.left, ListAmount.exactly(2), ListOrder.oneByOne, 1.0)),
      Some(Seq("d", "c"))
    )
    assertEquals(send(lrange[String]("l:d", 0L, -1L)), Seq("d", "c", "a", "b"))
    assertEquals(send(lmovem[String]("l:d", "l:s", ListSide.left, ListSide.left)), Some(Seq("d")))
    assertEquals(send(blocking.blmovem[String]("l:d", "l:s", ListSide.left, ListSide.left, 1.0)), Some(Seq("c")))
  }
}

final class KeyDbListClientSpec extends ListClientSpec(LiveServers.port(6380), "keyDb")

final class ValkeyListClientSpec extends ListClientSpec(LiveServers.port(6381), "valkey") {
  since[Valkey7_2]("a valkey client pops from the first list that has values") { v =>
    import v.*
    import listtypes.*

    send(del("l:a", "l:b"))
    send(rpush("l:b", OneOrMore.unsafeFrom(List("x", "y", "z"))))

    val keys = OneOrMoreKeys.unsafeFrom(List(Key("l:a"), Key("l:b")))
    assertEquals(send(lmpop[String](keys, ListSide.left)), Some(KV(Key("l:b"), Seq("x"))))
    assertEquals(send(blocking.blmpop[String](keys, ListSide.right, 5, 1.0)), Some(KV(Key("l:b"), Seq("z", "y"))))
    assertEquals(send(lmpop[String](keys, ListSide.left)), None)
  }
}

final class DragonflyListClientSpec extends ListClientSpec(LiveServers.port(6382), "dragonfly")

abstract class ListClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  test(s"a $dest client pops several values, moves them and finds their positions") {
    import laserdisc.redis_6_2.*
    import listtypes.*

    send(del("l:x", "l:y", "l:none"))
    send(rpush("l:x", OneOrMore.unsafeFrom(List("a", "b", "a", "c", "a"))))

    assertEquals(send(lpos("l:x", "a")).map(_.value), Some(0))
    assertEquals(send(lpos("l:x", "a", ListSearch.rank(-1))).map(_.value), Some(4))
    assertEquals(send(lpos("l:x", "a", 0)).map(_.value), Seq(0, 2, 4))
    assertEquals(send(lpos("l:x", "a", 0, ListSearch.maxLen(3))).map(_.value), Seq(0, 2))
    assertEquals(send(lpos("l:x", "zz")), None)

    assertEquals(send(lmove[String]("l:x", "l:y", ListSide.right, ListSide.left)), Some("a"))
    assertEquals(send(blocking.blmove[String]("l:x", "l:y", ListSide.left, ListSide.right, 1.0)), Some("a"))
    assertEquals(send(lrange[String]("l:y", 0L, -1L)), Seq("a", "a"))

    assertEquals(send(lpop[String]("l:x", 2)), Some(Seq("b", "a")))
    assertEquals(send(rpop[String]("l:x", 5)), Some(Seq("c")))
    assertEquals(send(lpop[String]("l:none", 2)), None)

    assertEquals(send(rpushx("l:none", OneOrMore.unsafeFrom(List("a", "b")))), None)
    assertEquals(send(lpushx("l:y", OneOrMore.unsafeFrom(List("b", "c")))).map(_.value), Some(4))
  }

  test(s"a $dest client pops from the first list that has an element with blpop") {
    import laserdisc.redis_6_2.*

    send(del("l:b1", "l:b2"))
    send(rpush("l:b2", OneOrMore.unsafeFrom(List("x"))))
    assertEquals(send(blocking.blpop[String](OneOrMoreKeys.unsafeFrom(List(Key("l:b1"), Key("l:b2"))), 1.0)), Some(KV(Key("l:b2"), "x")))
    assertEquals(send(blocking.blpop[String](OneOrMoreKeys.unsafeFrom(List(Key("l:b1"), Key("l:b2"))), 0.1)), None)
  }

  test(s"a $dest client reads nothing to pop with a count, whichever null the server replies with") {
    import laserdisc.redis_6_2.*

    send(del("l:none"))
    assertEquals(send(lpop[String]("l:none", 1)), None)
    assertEquals(send(rpop[String]("l:none", 1)), None)
  }
}
