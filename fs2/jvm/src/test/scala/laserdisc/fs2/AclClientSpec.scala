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

final class RedisAclClientSpec extends AclClientSpec(6379, "redis") {
  test("a redis client checks permissions and sets selectors with the commands of Redis 8.10")(selectors(redis_8_10))
}

final class KeyDbAclClientSpec extends AclClientSpec(6380, "keyDb")

final class ValkeyAclClientSpec extends AclClientSpec(6381, "valkey") {
  test("a valkey client checks permissions and sets selectors with the commands of Valkey 9.1")(selectors(valkey_9_1))

  test("a valkey client limits a user and its selectors to databases") {
    import valkey_9_1.{acl, servertypes}

    val rules = servertypes.aclDatabaseRules
    assertEquals(
      send(
        acl.setuser(
          "ld-acl-d",
          rules.reset.databases(OneOrMore.unsafeFrom(List[DbIndex](0, 1))).selector(rules.databases(OneOrMore.unsafeFrom(List[DbIndex](2))))
        )
      ),
      OK
    )
    val user = send(acl.getuser("ld-acl-d")).getOrElse(fail("expected the user"))
    assertEquals(user.databases, Some("db=0,1"))
    assertEquals(user.selectors.map(_.databases), Seq(Some("db=2")))
    assertEquals(send(acl.setuser("ld-acl-d", rules.resetDatabases)), OK)
    assertEquals(send(acl.getuser("ld-acl-d")).flatMap(_.databases), Some(""))
    assertEquals(send(acl.setuser("ld-acl-d", rules.allDatabases)), OK)
    assertEquals(send(acl.getuser("ld-acl-d")).flatMap(_.databases), Some("alldbs"))
    assertEquals(send(acl.deluser(OneOrMoreKeys.unsafeFrom(List(Key("ld-acl-d"))))).value, 1L)
  }
}

abstract class AclClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  protected final def selectors(version: Redis7_2): Unit = {
    import version.{acl, del, get, servertypes, set}

    val rules = servertypes.aclSelectorRules.reset.on
      .password("secret")
      .keys("app:*")
      .allow("get")
      .selector(servertypes.aclSelectorRules.keys("w:*").allow("set"))
    assertEquals(send(acl.setuser("ld-acl-s", rules)), OK)
    assertEquals(send(acl.dryrun("ld-acl-s", get[String]("app:1"))), None)
    assertEquals(send(acl.dryrun("ld-acl-s", set("w:1", 1))), None)
    assert(send(acl.dryrun("ld-acl-s", del("app:1"))).exists(_.contains("del")))
    assertEquals(send(acl.getuser("ld-acl-s")).map(_.selectors.map(_.keys)), Some(Seq("~w:*")))
    assertEquals(send(acl.setuser("ld-acl-s", servertypes.aclSelectorRules.resetKeys.readWriteKeys("both:*"))), OK)
    assertEquals(send(acl.getuser("ld-acl-s")).map(_.keys), Some("~both:*"))
    assertEquals(send(acl.deluser(OneOrMoreKeys.unsafeFrom(List(Key("ld-acl-s"))))).value, 1L)
  }

  test(s"a $dest client creates, reads and deletes a user") {
    val v = laserdisc.redis_6_2
    import v.{acl, servertypes}

    val rules = servertypes.aclRules.reset.on.password("secret").keys("app:*").allow("get").allow("set")
    assertEquals(send(acl.setuser("ld-acl", rules)), OK)
    val user = send(acl.getuser("ld-acl")).getOrElse(fail("expected the user"))
    assert(user.flags.contains("on"))
    assertEquals(user.passwords.size, 1)
    assert(user.keys.contains("~app:*"))
    assert(send(acl.users).contains("ld-acl"))
    assert(send(acl.list).exists(_.startsWith("user ld-acl ")))
    assertEquals(send(acl.whoami), "default")
    assert(send(acl.cat).contains("string"))
    assert(send(acl.cat("string")).contains("get"))
    assertEquals(send(acl.genpass(32)).length, 8)
    // a failed AUTH is what writes an entry to the log
    assert(this.client.send(Protocol("AUTH", "ld-acl-none" *: "wrong" *: EmptyTuple).as[Str, OK]).unsafeRunSync()(using runtime).isLeft)
    val entries = send(acl.log(5))
    assert(entries.size <= 5)
    assert(entries.exists(e => e.reason == "auth" && e.denied == "AUTH" && e.username == "ld-acl-none"), entries.toString)
    assertEquals(send(acl.getuser("ld-acl-none")), None)
    assertEquals(send(acl.deluser(OneOrMoreKeys.unsafeFrom(List(Key("ld-acl"), Key("ld-acl-none"))))).value, 1L)
  }
}
