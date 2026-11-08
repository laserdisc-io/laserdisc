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

final class AclPSpec extends BaseSpec with ServerP {
  import Acl.*

  private def bulks(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)
  private val hash                    = "2bb80d537b1da3e38bd30361aa855686bde0eacd7162fef6a25fe97bf527a25b"

  test("The ACL protocol using setuser sends the rules in order") {
    val rules =
      servertypes.aclRules.reset.on.password(Key("secret")).keys(GlobPattern("app:*")).allow(Key("get")).denyCategory(Key("dangerous"))
    assertEquals(
      acl.setuser(Key("u"), rules).encode,
      bulks("ACL", "SETUSER", "u", "reset", "on", ">secret", "~app:*", "+get", "-@dangerous")
    )
    assertEquals(acl.setuser(Key("u"), servertypes.aclRules).encode, bulks("ACL", "SETUSER", "u"))
    val withSelectors =
      servertypes.aclSelectorRules.on
        .readKeys(GlobPattern("r:*"))
        .selector(servertypes.aclSelectorRules.keys(GlobPattern("b:*")).allow(Key("set")))
    assertEquals(acl.setuser(Key("u"), withSelectors).encode, bulks("ACL", "SETUSER", "u", "on", "%R~r:*", "(~b:* +set)"))
    assertEquals(
      acl.setuser(Key("u"), servertypes.aclSelectorRules.readWriteKeys(GlobPattern("k:*"))).encode,
      bulks("ACL", "SETUSER", "u", "%RW~k:*")
    )
  }

  test("The ACL protocol using setuser sends the databases a user or selector may use") {
    val rules = servertypes.aclDatabaseRules
    assertEquals(
      acl.setuser(Key("u"), rules.databases(OneOrMore.unsafeFrom(List(DbIndex(0), DbIndex(15))))).encode,
      bulks("ACL", "SETUSER", "u", "db=0,15")
    )
    assertEquals(acl.setuser(Key("u"), rules.allDatabases).encode, bulks("ACL", "SETUSER", "u", "alldbs"))
    assertEquals(acl.setuser(Key("u"), rules.resetDatabases).encode, bulks("ACL", "SETUSER", "u", "resetdbs"))
    assertEquals(
      acl.setuser(Key("u"), rules.selector(rules.databases(OneOrMore.unsafeFrom(List(DbIndex(1)))).keys(GlobPattern("b:*")))).encode,
      bulks("ACL", "SETUSER", "u", "(db=1 ~b:*)")
    )
  }

  test("The ACL protocol using setuser on Dragonfly sends the keys that may only be read or written") {
    assertEquals(
      dragonfly_2_0.acl
        .setuser(Key("u"), dragonfly_2_0.servertypes.aclDragonflyRules.readKeys(GlobPattern("r:*")).readWriteKeys(GlobPattern("k:*")))
        .encode,
      bulks("ACL", "SETUSER", "u", "%R~r:*", "%RW~k:*")
    )
  }

  test(
    "The ACL protocol using getuser reads patterns as rules on Redis 6.2's import only, and Redis 7.0's and Valkey's selectors and databases"
  ) {
    val redis62 = Arr(
      Bulk("flags"),
      bulks("on", "allchannels"),
      Bulk("passwords"),
      bulks(hash),
      Bulk("commands"),
      Bulk("-@all +get +set"),
      Bulk("keys"),
      bulks("app:*", "other:*"),
      Bulk("channels"),
      bulks("*")
    )
    redis_6_2.acl.getuser(Key("u")).decode(redis62).onRight {
      _ == Some(AclUserOf6_2(Seq("on", "allchannels"), Seq(hash), "-@all +get +set", "~app:* ~other:*", "&*"))
    }
    assert(acl.getuser(Key("u")).decode(redis62).isLeft)
    val valkey = Arr(
      Bulk("flags"),
      bulks("on"),
      Bulk("passwords"),
      bulks(hash),
      Bulk("commands"),
      Bulk("-@all +get"),
      Bulk("keys"),
      Bulk("~a:*"),
      Bulk("channels"),
      Bulk("&c:*"),
      Bulk("databases"),
      Bulk("alldbs"),
      Bulk("selectors"),
      Arr(bulks("commands", "-@all +set", "keys", "~b:*", "channels", "", "databases", "db=1"))
    )
    // Valkey 9.1's import reads the databases of the user and of its selectors
    acl.getuser(Key("u")).decode(valkey).onRight {
      _ == Some(
        AclUserOfValkey9_1(
          Seq("on"),
          Seq(hash),
          "-@all +get",
          "~a:*",
          "&c:*",
          Seq(AclSelectorOfValkey9_1("-@all +set", "~b:*", "", "db=1")),
          "alldbs"
        )
      )
    }
    // the imports before read the same reply without what they don't have
    redis_7_0.acl.getuser(Key("u")).decode(valkey).onRight {
      _ == Some(AclUserOf7_0(Seq("on"), Seq(hash), "-@all +get", "~a:*", "&c:*", Seq(AclSelectorOf7_0("-@all +set", "~b:*", ""))))
    }
    redis_6_2.acl.getuser(Key("u")).decode(valkey).onRight {
      _ == Some(AclUserOf6_2(Seq("on"), Seq(hash), "-@all +get", "~a:*", "&c:*"))
    }
    assert(compileErrors("redis_7_0.acl.getuser(Key(\"u\")).decode(valkey).map(_.map(_.databases))").nonEmpty)
    assertEquals(acl.getuser(Key("nosuch")).decode(NullBulk), None)
    assert(acl.getuser(Key("nosuch")).decode(NilArr).isLeft)
  }

  test("The ACL protocol using log reads Redis 6.2's entries and Redis 7.2's, which have an id and timestamps") {
    def entry(extra: List[RESP]) = Arr(
      Bulk("count") :: Num(1L) :: bulks(
        "reason",
        "auth",
        "context",
        "toplevel",
        "object",
        "AUTH",
        "username",
        "u",
        "age-seconds",
        "0.5",
        "client-info",
        "id=1"
      ).elements ::: extra
    )
    val withIds = entry(List(Bulk("entry-id"), Num(0L), Bulk("timestamp-created"), Num(17L), Bulk("timestamp-last-updated"), Num(18L)))
    redis_6_2.acl.log.decode(Arr(entry(Nil), withIds)).onRight {
      _ == Seq.fill(2)(AclLogEntryOf6_2(1L, "auth", "toplevel", "AUTH", "u", 0.5, "id=1"))
    }
    acl.log(PosInt(1)).decode(Arr(withIds)).onRight(_.map(e => (e.entryId, e.createdAt, e.updatedAt)) == Seq((0L, 17L, 18L)))
    assert(acl.log.decode(Arr(entry(Nil))).isLeft)
  }

  test("The ACL protocol using dryrun sends the command it is given and reads OK as None and a reason as Some") {
    import laserdisc.auto.*
    val del = redis_8_10.keys.del("app:1")
    assertEquals(acl.dryrun("u", del).encode, bulks("ACL", "DRYRUN", "u", "DEL", "app:1"))
    assertEquals(acl.dryrun("u", del).decode(Str("OK")), None)
    assertEquals(
      acl.dryrun("u", del).decode(Bulk("User u has no permissions to run the 'del' command")),
      Some("User u has no permissions to run the 'del' command")
    )
  }
}
