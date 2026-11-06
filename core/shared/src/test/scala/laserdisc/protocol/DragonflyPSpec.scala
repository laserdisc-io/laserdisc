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

final class DragonflyPSpec extends BaseSpec with DragonflyP {
  import DragonflyP.{FieldExpireResult, FieldTtl, Rm, Throttle}

  private def bulks(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)
  private val a                       = Key("a")

  test("The Dragonfly protocol using prepend, saddex and stick sends their arguments") {
    assertEquals(prepend(a, "x").encode, bulks("PREPEND", "a", "x"))
    assertEquals(prepend(a, "x").decode(Num(3L)), NonNegLong(3L))
    assertEquals(saddex(a, PosLong(5L), OneOrMore.unsafeFrom(List(1, 2))).encode, bulks("SADDEX", "a", "5", "1", "2"))
    assertEquals(
      saddex(a, dragonflytypes.keepTtl, PosLong(5L), OneOrMore.unsafeFrom(List(1))).encode,
      bulks("SADDEX", "a", "KEEPTTL", "5", "1")
    )
    assertEquals(stick(OneOrMoreKeys.unsafeFrom(List(a, Key("b")))).encode, bulks("STICK", "a", "b"))
    assertEquals(repltakeover(PosInt(5)).encode, bulks("REPLTAKEOVER", "5"))
  }

  test("The Dragonfly protocol using fieldexpire and fieldttl reads what the numbers mean") {
    val fields = OneOrMoreKeys.unsafeFrom(List(Key("f"), Key("g"), Key("h")))
    assertEquals(fieldexpire(a, NonNegLong(5L), fields).encode, bulks("FIELDEXPIRE", "a", "5", "f", "g", "h"))
    assertEquals(
      fieldexpire(a, NonNegLong(5L), fields).decode(Arr(Num(1L), Num(2L), Num(-2L))),
      Seq[FieldExpireResult](FieldExpireResult.Set, FieldExpireResult.Deleted, FieldExpireResult.Missing)
    )
    assert(fieldexpire(a, NonNegLong(5L), fields).decode(Arr(Num(9L))).isLeft)
    assertEquals(fieldttl(a, Key("f")).encode, bulks("FIELDTTL", "a", "f"))
    assertEquals(fieldttl(a, Key("f")).decode(Num(7L)), FieldTtl.Expires(NonNegLong(7L)): FieldTtl)
    assertEquals(fieldttl(a, Key("f")).decode(Num(-1L)), FieldTtl.NoExpiry: FieldTtl)
    assertEquals(fieldttl(a, Key("f")).decode(Num(-2L)), FieldTtl.NoKey: FieldTtl)
    assertEquals(fieldttl(a, Key("f")).decode(Num(-3L)), FieldTtl.NoField: FieldTtl)
  }

  test("The Dragonfly protocol using clthrottle sends the rate and reads whether it was allowed") {
    assertEquals(clthrottle(a, NonNegLong(0L), PosLong(1L), PosLong(60L)).encode, bulks("CL.THROTTLE", "a", "0", "1", "60"))
    assertEquals(
      clthrottle(a, NonNegLong(0L), PosLong(1L), PosLong(60L), NonNegLong(2L)).encode,
      bulks("CL.THROTTLE", "a", "0", "1", "60", "2")
    )
    assertEquals(
      clthrottle(a, NonNegLong(0L), PosLong(1L), PosLong(60L)).decode(Arr(Num(0L), Num(1L), Num(0L), Num(-1L), Num(60L))),
      Throttle(false, NonNegLong(1L), NonNegLong(0L), None, NonNegLong(60L))
    )
    assertEquals(
      clthrottle(a, NonNegLong(0L), PosLong(1L), PosLong(60L)).decode(Arr(Num(1L), Num(1L), Num(0L), Num(30L), Num(60L))),
      Throttle(true, NonNegLong(1L), NonNegLong(0L), Some(NonNegLong(30L)), NonNegLong(60L))
    )
  }

  test("The Dragonfly protocol using rm sends the options and reads the next cursor and how many it deleted") {
    assertEquals(rm(NonNegLong(0L)).encode, bulks("RM", "0"))
    val options = dragonflytypes.rmOptions.pattern(GlobPattern("k*")).keyType(KeyP.Type.string).count(PosInt(5))
    assertEquals(rm(NonNegLong(3L), options).encode, bulks("RM", "3", "MATCH", "k*", "TYPE", "string", "COUNT", "5"))
    assertEquals(rm(NonNegLong(0L)).decode(Arr(Bulk("3"), Num(10L))), Rm(NonNegLong(3L), NonNegLong(10L)))
  }

  test("The Dragonfly protocol using role reads a primary and its replicas, and a replica and its primary") {
    import dragonflytypes.role.*
    assertEquals(role.decode(Arr(Bulk("master"), Arr(Nil))), Primary(Nil): DragonflyP.Role)
    assertEquals(
      role.decode(Arr(Bulk("master"), Arr(bulks("10.0.0.3", "6379", "online")))),
      Primary(Seq(Replica(Host("10.0.0.3"), Port(6379), "online"))): DragonflyP.Role
    )
    assertEquals(
      role.decode(bulks("slave", "df-primary", "6379", "online")),
      Follower(Host("df-primary"), Port(6379), "online"): DragonflyP.Role
    )
    assert(role.decode(Arr(Bulk("sentinel"), Arr(Nil))).isLeft)
  }

  test("The replies Dragonfly sends differently are read on every version") {
    assertEquals(redis_8_10.time.decode(Arr(Num(5L), Num(6L))), Time(NonNegLong(5L), NonNegLong(6L)))
    assertEquals(redis_8_10.time.decode(bulks("5", "6")), Time(NonNegLong(5L), NonNegLong(6L)))
    assertEquals(redis_8_10.zrandmember[String](a).decode(bulks("m")), Some("m"))
    assertEquals(redis_8_10.zrandmember[String](a).decode(Bulk("m")), Some("m"))
    assertEquals(redis_8_10.zrandmember[String](a).decode(NullBulk), None)
    assertEquals(
      redis_8_10.xpending(a, a).decode(Arr(Num(1L), Bulk("1-1"), Bulk("1-1"), Arr(Arr(Bulk("c"), Num(1L))))).map(_.consumers),
      Seq(Key("c") -> NonNegLong(1L))
    )
    assertEquals(
      redis_8_10.xpending(a, a).decode(Arr(Num(1L), Bulk("1-1"), Bulk("1-1"), Arr(Arr(Bulk("c"), Bulk("1"))))).map(_.consumers),
      Seq(Key("c") -> NonNegLong(1L))
    )
    assertEquals(redis_8_10.info.encode, bulks("INFO"))
    assertEquals(
      dragonfly_2_0.module.list.decode(Arr(Arr(Str("name"), Str("search"), Str("ver"), Num(5L)))),
      Seq(ServerInspection.ModuleInfo("search", 5L, None, Nil))
    )
    assertEquals(
      redis_8_10.module.list
        .decode(Arr(Arr(Bulk("name"), Bulk("m"), Bulk("ver"), Num(5L), Bulk("path"), Bulk("/p"), Bulk("args"), Arr(Nil)))),
      Seq(ServerInspection.ModuleInfo("m", 5L, Some("/p"), Nil))
    )
  }

  test("The Dragonfly protocol reads ACL GETUSER as Dragonfly replies") {
    val reply = Arr(
      Str("flags"),
      Arr(Str("on")),
      Str("passwords"),
      Str("#abc"),
      Str("commands"),
      Str("-@all +get"),
      Str("keys"),
      Arr(Nil),
      Str("channels"),
      Str("resetchannels &c*")
    )
    val user = dragonfly_2_0.acl.getuser(a).decode(reply).getOrElse(fail("expected a user")).getOrElse(fail("expected a user"))
    assertEquals(user.flags, Seq("on"))
    assertEquals(user.passwords, Seq("#abc"))
    assertEquals(user.commands, "-@all +get")
    assertEquals(user.keys, "")
    assertEquals(user.channels, "resetchannels &c*")
    assertEquals(dragonfly_2_0.acl.getuser(a).decode(NullBulk), None)
  }
}
