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

final case class HashUser(name: String, age: Int, email: Option[String])

final class RedisHashClientSpec extends HashClientSpec(6379, "redis") {
  test("a redis client expires fields, reads them with an expiry and deletes them with the commands of Redis 8.10")(
    fieldExpiry(redis_8_10, redis_8_10, redis_8_10, redis_8_10, redis_8_10)
  )
}

final class KeyDbHashClientSpec extends HashClientSpec(6380, "keyDb")

final class ValkeyHashClientSpec extends HashClientSpec(6381, "valkey") {
  test("a valkey client expires fields, reads them with an expiry and deletes them with the commands of Valkey 9.1")(
    fieldExpiry(valkey_9_1, valkey_9_1, valkey_9_1, valkey_9_1, valkey_9_1)
  )

  test("a valkey client sets fields only if the hash exists with the commands of Valkey 9.1") {
    import laserdisc.valkey_9_1.*
    import hashtypes.*

    send(del("h:k"))
    val fvs = OneOrMore.unsafeFrom(List(Key("a") -> 1))
    assert(!send(hsetex("h:k", HashKeyCondition.xx, fvs)))
    assert(send(hsetex("h:k", HashKeyCondition.nx, fvs)))
    assert(send(hsetex("h:k", HashKeyCondition.xx, HashFieldsCondition.fxx, stringtypes.expiry.seconds(100L), fvs)))
  }
}

abstract class HashClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  // the version, and the same version again as each of the hash features it has
  protected final def fieldExpiry(
      version: Redis7_2,
      noValues: protocol.HashScanNoValuesP,
      expiry: protocol.HashFieldExpiryP,
      getSet: protocol.HashGetSetExpiryP,
      getDel: protocol.HashGetDelP
  ): Unit = {
    import version.*
    import hashtypes.*
    import noValues.hscannovalues
    import expiry.{hexpire, hexpiretime, hpersist, hpexpireat, hpttl, httl}
    import getSet.{hgetex, hsetex}
    import getDel.hgetdel

    send(del("h:e"))
    send(hset("h:e", OneOrMore.unsafeFrom(List(Key("a") -> 1, Key("b") -> 2, Key("c") -> 3))))

    val ab = OneOrMoreKeys.unsafeFrom(List(Key("a"), Key("b"), Key("zz")))
    val a  = OneOrMoreKeys.unsafeFrom(List(Key("a")))
    val c  = OneOrMoreKeys.unsafeFrom(List(Key("c")))
    assertEquals(send(hexpire("h:e", 100L, ab)), Seq(HashFieldSet, HashFieldSet, HashNoField))
    assertEquals(send(hexpire("h:e", 100L, HashExpireCondition.nx, a)), Seq(HashFieldNotSet))
    assertEquals(send(hpexpireat("h:e", 1L, c)), Seq(HashFieldDeleted))
    send(httl("h:e", a)) match {
      case Seq(HashFieldExpireAfter(ttl)) => assert(ttl.value > 0L && ttl.value <= 100L)
      case other                          => fail(s"expected a TTL, got $other")
    }
    assertEquals(send(hpersist("h:e", ab)), Seq(HashFieldPersisted, HashFieldPersisted, HashNoField))
    assertEquals(send(hexpiretime("h:e", a)), Seq(HashNoExpire))

    assertEquals(send(hgetex[Int]("h:e", stringtypes.expiry.seconds(50L), ab)), Seq(Some(1), Some(2), None))
    assertEquals(send(hpttl("h:e", a)).size, 1)
    assertEquals(send(hgetex[Int]("h:e", stringtypes.expiry.persist, a)), Seq(Some(1)))
    val fvs = OneOrMore.unsafeFrom(List(Key("a") -> 9))
    assert(!send(hsetex("h:e", HashFieldsCondition.fnx, fvs)))
    assert(send(hsetex("h:e", HashFieldsCondition.fxx, stringtypes.expiry.keepTtl, fvs)))
    assertEquals(send(hgetdel[Int]("h:e", OneOrMoreKeys.unsafeFrom(List(Key("a"), Key("zz"))))), Seq(Some(9), None))
    assertEquals(send(hlen("h:e")).value, 1)
    assertEquals(send(hscannovalues("h:e", 0L)).values, Some(Seq(Key("b"))))
  }

  test(s"a $dest client sets several fields at once and picks random fields") {
    import laserdisc.redis_6_2.*

    send(del("h:r", "h:none"))
    assertEquals(send(hset("h:r", OneOrMore.unsafeFrom(List(Key("a") -> 1, Key("b") -> 2)))).value, 2)
    assert(send(hrandfield("h:r")).exists(Set(Key("a"), Key("b"))))
    assertEquals(send(hrandfields("h:r", 5)).toSet, Set(Key("a"), Key("b")))
    assertEquals(send(hrandfieldswithvalues[Int]("h:r", 2)).toSet, Set(Key("a") -> 1, Key("b") -> 2))
    assertEquals(send(hrandfield("h:none")), None)
    assertEquals(send(hrandfields("h:none", 2)), Seq.empty)
  }

  test(s"a $dest client writes a case class to a hash and reads it back by field name") {
    import laserdisc.redis_6_2.*

    send(del("h:u", "h:p"))
    assertEquals(send(hset("h:u", HashUser("ada", 36, None))).value, 2)
    assertEquals(send(hgetall[HashUser]("h:u")), HashUser("ada", 36, None))
    send(hset("h:u", "email", "ada@example.com"))
    send(hset("h:u", "extra", "x"))
    assertEquals(send(hgetall[HashUser]("h:u")), HashUser("ada", 36, Some("ada@example.com")))
    assertEquals(send(hgetall[Map[Key, String]]("h:u")).size, 4)

    send(hset("h:p", "name", "bob"))
    this.client.send(hgetall[HashUser]("h:p")).unsafeRunSync()(using runtime) match {
      case Left(e)  => assert(e.getMessage.endsWith("missing field age"), e.getMessage)
      case Right(u) => fail(s"expected a missing field, got $u")
    }
  }

  test(s"a $dest client reads fields that may be missing with hmget") {
    import laserdisc.redis_6_2.*

    send(del("h:m"))
    send(hset("h:m", "age", 36))
    val fields = OneOrMoreKeys.unsafeFrom(List(Key("age"), Key("name")))
    assertEquals(send(hmget[Int *: Option[String] *: EmptyTuple]("h:m", fields)), 36 *: Option.empty[String] *: EmptyTuple)
    this.client.send(hmget[Int *: String *: EmptyTuple]("h:m", fields)).unsafeRunSync()(using runtime) match {
      case Left(e)  => assert(e.getMessage.endsWith("error at element 2: Read[OneOf] error: missing value"), e.getMessage)
      case Right(v) => fail(s"expected a missing value, got $v")
    }
  }
}
