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
      _ == Some(AclUser(Seq("on", "allchannels"), Seq(hash), "-@all +get +set", "~app:* ~other:*", "&*", Nil, None))
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
      Arr(bulks("commands", "-@all +set", "keys", "~b:*", "channels", ""))
    )
    acl.getuser(Key("u")).decode(valkey).onRight {
      _ == Some(AclUser(Seq("on"), Seq(hash), "-@all +get", "~a:*", "&c:*", Seq(AclSelector("-@all +set", "~b:*", "")), Some("alldbs")))
    }
    redis_6_2.acl.getuser(Key("u")).decode(valkey).onRight {
      _ == Some(AclUser(Seq("on"), Seq(hash), "-@all +get", "~a:*", "&c:*", Seq(AclSelector("-@all +set", "~b:*", "")), Some("alldbs")))
    }
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
    acl.log.decode(Arr(entry(Nil))).onRight(_ == Seq(AclLogEntry(1L, "auth", "toplevel", "AUTH", "u", 0.5, "id=1", None, None, None)))
    acl
      .log(PosInt(1))
      .decode(Arr(entry(List(Bulk("entry-id"), Num(0L), Bulk("timestamp-created"), Num(17L), Bulk("timestamp-last-updated"), Num(18L)))))
      .onRight {
        _.map(e => (e.entryId, e.createdAt, e.updatedAt)) == Seq((Some(0L), Some(17L), Some(18L)))
      }
  }

  test("The ACL protocol using dryrun sends the command it is given and reads OK as None and a reason as Some") {
    import laserdisc.auto.*
    val del = families.keys.del("app:1")
    assertEquals(acl.dryrun("u", del).encode, bulks("ACL", "DRYRUN", "u", "DEL", "app:1"))
    assertEquals(acl.dryrun("u", del).decode(Str("OK")), None)
    assertEquals(
      acl.dryrun("u", del).decode(Bulk("User u has no permissions to run the 'del' command")),
      Some("User u has no permissions to run the 'del' command")
    )
  }
}
