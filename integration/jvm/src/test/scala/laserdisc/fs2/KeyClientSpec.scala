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

final class RedisKeyClientSpec extends KeyClientSpec(LiveServers.port(6379), "redis") {
  since[Redis7_2]("a redis client sets expiries with conditions and reads them")(v => expiries(v))

  since[Redis8_8]("a redis client reads the type of an array or a module's key, and scans by it") { v =>
    import v.*
    import keytypes.*

    send(del("k:a", "k:v"))
    send(arset("k:a", 0L, OneOrMore.unsafeFrom(List("x"))))
    send(Protocol("VADD", List("k:v", "VALUES", "1", "1", "e")).as[Num, Long])

    assertEquals(send(typeof("k:a")), Some(KeyType.array))
    assertEquals(send(typeof("k:v")), Some(KeyType.other("vectorset")))
    assertEquals(send(typeof("k:none")), None)
    assert(send(scan(0L, 10000, KeyType.array)).values.exists(_.contains(Key("k:a"))))
    assert(send(scan(0L, 10000, KeyType.other("vectorset"))).values.exists(_.contains(Key("k:v"))))
    val _ = send(del("k:a", "k:v"))
  }
}

final class KeyDbKeyClientSpec extends KeyClientSpec(LiveServers.port(6380), "keyDb")

final class ValkeyKeyClientSpec extends KeyClientSpec(LiveServers.port(6381), "valkey") {
  since[Valkey7_2]("a valkey client sets expiries with conditions and reads them")(v => expiries(v))
}

abstract class KeyClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  protected final def expiries(version: Redis7_2): Unit = {
    import version.*
    import keytypes.*

    send(del("k:e"))
    send(set("k:e", 1))
    assertEquals(send(expiretime("k:e")), KeyNoExpireTTLResponse)
    assertEquals(send(expiretime("k:none")), KeyNoKeyTTLResponse)
    assert(send(expire("k:e", 100, KeyExpireCondition.nx)))
    assert(!send(expire("k:e", 50, KeyExpireCondition.gt)))
    assert(send(expire("k:e", 50, KeyExpireCondition.xxLt)))
    send(expiretime("k:e")) match {
      case KeyExpireAt(at) => assert(at.value > 0L)
      case other           => fail(s"expected a timestamp, got $other")
    }
    assertEquals(send(sortro[String]("k:none", KeySort.alpha)), Seq.empty)
    val (local, replicas) = send(waitaof(0, 0, 10L))
    assertEquals(local.value -> replicas.value, 0 -> 0)
  }

  test(s"a $dest client reads the encoding of each kind of key") {
    import laserdisc.redis_6_2.*
    import keytypes.*

    send(del("k:s", "k:n", "k:l", "k:h", "k:t", "k:z", "k:x"))
    send(set("k:s", "short"))
    send(set("k:n", 12))
    send(rpush("k:l", "a"))
    send(hset("k:h", "f", "v"))
    send(sadd("k:t", "a"))
    send(zadd("k:z", "a", 1.0))
    send(xadd("k:x", OneOrMore.unsafeFrom(List(Key("f") -> "v"))))

    val compact = Set[KeyEncoding](KeyEncoding.ziplist, KeyEncoding.listpack)
    assertEquals(send(obj.encoding("k:s")), Some(KeyEncoding.embstr))
    assertEquals(send(obj.encoding("k:n")), Some(KeyEncoding.int))
    assert(send(obj.encoding("k:l")).exists(Set[KeyEncoding](KeyEncoding.quicklist, KeyEncoding.listpack)), dest)
    assert(send(obj.encoding("k:h")).exists(compact), dest)
    assert(send(obj.encoding("k:t")).exists(Set[KeyEncoding](KeyEncoding.hashtable, KeyEncoding.listpack)), dest)
    assert(send(obj.encoding("k:z")).exists(compact), dest)
    assertEquals(send(obj.encoding("k:x")), Some(KeyEncoding.stream))
    assertEquals(send(obj.encoding("k:none")), None)
    send(del("k:s", "k:n", "k:l", "k:h", "k:t", "k:z", "k:x"))
  }

  test(s"a $dest client dumps a key and restores it, as a dump isn't text") {
    import laserdisc.redis_6_2.*
    import keytypes.*

    send(del("k:dump", "k:restored"))
    send(set("k:dump", "héllo"))

    val dumped = send(dump("k:dump")).getOrElse(fail("nothing dumped"))
    assertEquals(send(restore("k:restored", 0L, dumped)), OK)
    assertEquals(send(get[String]("k:restored")), Some("héllo"))
    assertEquals(send(restore("k:restored", 0L, dumped, KeyRestoreMode.replace)), OK)
    assertEquals(send(dump("k:none")), None)
    send(del("k:dump", "k:restored"))
  }

  test(s"a $dest client copies, scans by type and sorts with GET") {
    import laserdisc.redis_6_2.*
    import keytypes.*

    send(del("k:l", "k:c", "k:s", "w_1", "w_2", "o_1", "h_3"))
    send(rpush("k:l", OneOrMore.unsafeFrom(List(1, 2, 3))))
    send(set("w_1", 30))
    send(set("w_2", 10))
    send(set("o_1", "one"))
    send(hset("h_3", "f", "three"))

    assert(send(copy("k:l", "k:c")))
    assert(!send(copy("k:l", "k:c")))
    assert(send(copy("k:l", "k:c", KeyReplace)))
    assertEquals(send(typeof("k:c")), Some(KeyType.list))
    assert(send(scan(0L, 1000, KeyType.list)).values.exists(_.contains(Key("k:c"))))
    assert(send(scan(0L, "k:*", 1000)).values.exists(_.contains(Key("k:c"))))
    assertEquals(send(keys("k:[lc]")).toSet, Set(Key("k:l"), Key("k:c")))

    assertEquals(send(sort[Int]("k:l", KeySort.desc)), Seq(3, 2, 1))
    assertEquals(
      send(sort[String]("k:l", KeySort.by("w_*"), KeySortGet("#", "o_*", "h_*->f"))),
      Seq(Some("3"), None, Some("three"), Some("2"), None, None, Some("1"), Some("one"), None)
    )
    assertEquals(send(sort("k:l", KeySort.alpha.limit(0L, 2L), KeySortGet("#"), "k:s")).value, 2)
    assertEquals(send(lrange[String]("k:s", 0L, -1L)), Seq("1", "2"))
  }
}
