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

import scodec.bits.*

final class ScriptingPSpec extends BaseSpec with ScriptingP {
  import scriptingtypes.*

  private def bulks(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)
  private val sha                     = Sha1("e0e1f9fabfc9d4800c877a703b823ac0578ff8db")
  private val keys                    = OneOrMoreKeys.unsafeFrom(List(Key("k1"), Key("k2")))
  private val args                    = OneOrMore.unsafeFrom(List[Value](1, "x"))

  test("The Scripting protocol sends the number of keys, the keys, then the arguments, and reads the reply as asked") {
    assertEquals(eval[Num, Long]("return 1").encode, bulks("EVAL", "return 1", "0"))
    assertEquals(eval[Num, Long]("return 1", keys).encode, bulks("EVAL", "return 1", "2", "k1", "k2"))
    assertEquals(evalargs[Num, Long]("return 1", args).encode, bulks("EVAL", "return 1", "0", "1", "x"))
    assertEquals(eval[Num, Long]("return 1", keys, args).encode, bulks("EVAL", "return 1", "2", "k1", "k2", "1", "x"))
    assertEquals(eval[Num, Long]("return 1").decode(Num(1L)), 1L)
    assertEquals(eval[Bulk, String]("return 'a'").decode(Bulk("a")), "a")
    assert(eval[Num, Long]("return 1").decode(Bulk("a")).isLeft)

    assertEquals(evalsha[Num, Long](sha, keys, args).encode, bulks("EVALSHA", sha.value, "2", "k1", "k2", "1", "x"))
    assertEquals(evalshaargs[Num, Long](sha, args).encode, bulks("EVALSHA", sha.value, "0", "1", "x"))
    assertEquals(evalro[Num, Long]("return 1", keys).encode, bulks("EVAL_RO", "return 1", "2", "k1", "k2"))
    assertEquals(evalroargs[Num, Long]("return 1", args).encode, bulks("EVAL_RO", "return 1", "0", "1", "x"))
    assertEquals(evalsharo[Num, Long](sha).encode, bulks("EVALSHA_RO", sha.value, "0"))
    assertEquals(evalsharoargs[Num, Long](sha, args).encode, bulks("EVALSHA_RO", sha.value, "0", "1", "x"))
    assertEquals(fcall[Num, Long](Key("f"), keys, args).encode, bulks("FCALL", "f", "2", "k1", "k2", "1", "x"))
    assertEquals(fcallargs[Num, Long](Key("f"), args).encode, bulks("FCALL", "f", "0", "1", "x"))
    assertEquals(fcallro[Num, Long](Key("f")).encode, bulks("FCALL_RO", "f", "0"))
    assertEquals(fcallroargs[Num, Long](Key("f"), args).encode, bulks("FCALL_RO", "f", "0", "1", "x"))
  }

  test("The Scripting protocol reads a table of one type with Arr.Of, nil as Unit, and a value or nil as an Option") {
    assertEquals(eval[Arr.Of[Num], Seq[Long]]("return {1, 2}").decode(Arr(Num(1L), Num(2L))), Seq(1L, 2L))
    assertEquals(eval[Arr.Of[Num], Seq[Long]]("return {}").decode(Arr(Nil)), Seq.empty[Long])
    assert(eval[Arr.Of[Num], Seq[Long]]("return {1, 'a'}").decode(Arr(Num(1L), Bulk("a"))).isLeft)
    assertEquals(eval[Arr.Of[Bulk], Seq[Int]]("return {'1', '2'}").decode(Arr(Bulk("1"), Bulk("2"))), Seq(1, 2))
    assert(eval[Arr.Of[Bulk], Seq[Int]]("return {'1', 'x'}").decode(Arr(Bulk("1"), Bulk("x"))).isLeft)
    assertEquals(
      eval[Arr.Of[Arr.Of[Num]], Seq[Seq[Long]]]("return {{1}, {2, 3}}").decode(Arr(Arr(Num(1L)), Arr(Num(2L), Num(3L)))),
      Seq(Seq(1L), Seq(2L, 3L))
    )
    assertEquals(eval[NullBulk, Unit]("return nil").decode(NullBulk), ())
    assert(eval[NullBulk, Unit]("return nil").decode(Bulk("a")).isLeft)
    assertEquals(eval[OrNullBulk[Bulk], Option[String]]("return nil").decode(NullBulk), None)
    assertEquals(eval[OrNullBulk[Bulk], Option[String]]("return 'a'").decode(Bulk("a")), Some("a"))
    assertEquals(eval[OrNullBulk[Num], Option[Long]]("return nil").decode(NullBulk), None)
    assertEquals(eval[OrNilArr[Arr.Of[Num]], Option[Seq[Long]]]("return nil").decode(NilArr), None)
  }

  test("The Scripting protocol using script loads, checks, shows, flushes and kills scripts") {
    assertEquals(script.load("return 1").encode, bulks("SCRIPT", "LOAD", "return 1"))
    assertEquals(script.load("return 1").decode(Bulk(sha.value)), sha)
    assert(script.load("return 1").decode(Bulk("nope")).isLeft)
    assertEquals(script.exists(OneOrMore.unsafeFrom(List(sha))).encode, bulks("SCRIPT", "EXISTS", sha.value))
    assertEquals(script.exists(OneOrMore.unsafeFrom(List(sha, sha))).decode(Arr(Num(1L), Num(0L))), Seq(true, false))
    assertEquals(script.show(sha).encode, bulks("SCRIPT", "SHOW", sha.value))
    assertEquals(script.flush(ScriptingFlushMode.async).encode, bulks("SCRIPT", "FLUSH", "ASYNC"))
    assertEquals(script.kill.encode, bulks("SCRIPT", "KILL"))
  }

  // as Redis 8.10 and Valkey 9.1 reply
  private val library = Arr(
    Bulk("library_name"),
    Bulk("mylib"),
    Bulk("engine"),
    Bulk("LUA"),
    Bulk("functions"),
    Arr(
      Arr(Bulk("name"), Bulk("f2"), Bulk("description"), NullBulk, Bulk("flags"), Arr(Nil)),
      Arr(Bulk("name"), Bulk("f1"), Bulk("description"), Bulk("counts keys"), Bulk("flags"), Arr(Str("no-writes")))
    ),
    Bulk("library_code"),
    Bulk("#!lua name=mylib")
  )

  test("The Scripting protocol using function lists libraries, with their functions and code") {
    assertEquals(function.list.encode, bulks("FUNCTION", "LIST"))
    assertEquals(function.list(GlobPattern("my*"), ScriptingWithCode).encode, bulks("FUNCTION", "LIST", "LIBRARYNAME", "my*", "WITHCODE"))
    assertEquals(
      function.list(ScriptingWithCode).decode(Arr(library)),
      Seq(
        ScriptingLibrary(
          Key("mylib"),
          "LUA",
          Seq(ScriptingFunction(Key("f2"), None, Nil), ScriptingFunction(Key("f1"), Some("counts keys"), Seq("no-writes"))),
          Some("#!lua name=mylib")
        )
      )
    )
    assertEquals(function.list.decode(Arr(Nil)), Seq.empty[ScriptingLibrary])
    assert(function.list.decode(Arr(bulks("library_name", "mylib"))).isLeft)
  }

  test("The Scripting protocol using function loads, deletes, flushes and kills libraries") {
    assertEquals(function.load("#!lua name=l").encode, bulks("FUNCTION", "LOAD", "#!lua name=l"))
    assertEquals(function.load("#!lua name=l", ScriptingReplace).encode, bulks("FUNCTION", "LOAD", "REPLACE", "#!lua name=l"))
    assertEquals(function.load("#!lua name=l").decode(Bulk("l")), Key("l"))
    assertEquals(function.delete(Key("l")).encode, bulks("FUNCTION", "DELETE", "l"))
    assertEquals(function.flush(ScriptingFlushMode.sync).encode, bulks("FUNCTION", "FLUSH", "SYNC"))
    assertEquals(function.kill.encode, bulks("FUNCTION", "KILL"))
  }

  test("The Scripting protocol using function dumps and restores libraries, keeping the payload's bytes") {
    val payload = Bulk(hex"f5c30301ff")

    assertEquals(function.dump.encode, bulks("FUNCTION", "DUMP"))
    assertEquals(function.dump.decode(payload), payload)
    assertEquals(function.restore(payload).encode, Arr(Bulk("FUNCTION"), Bulk("RESTORE"), payload))
    assertEquals(
      function.restore(payload, ScriptingRestorePolicy.append).encode,
      Arr(Bulk("FUNCTION"), Bulk("RESTORE"), payload, Bulk("APPEND"))
    )
    assertEquals(
      function.restore(payload, ScriptingRestorePolicy.flush).encode,
      Arr(Bulk("FUNCTION"), Bulk("RESTORE"), payload, Bulk("FLUSH"))
    )
    assertEquals(
      function.restore(payload, ScriptingRestorePolicy.replace).encode,
      Arr(Bulk("FUNCTION"), Bulk("RESTORE"), payload, Bulk("REPLACE"))
    )
    assertEquals(function.restore(payload).decode(Str("OK")), OK)
  }

  test("The Scripting protocol using function reads the stats, with or without a function running") {
    val engines = Arr(Bulk("LUA"), Arr(Bulk("libraries_count"), Num(2L), Bulk("functions_count"), Num(3L)))
    assertEquals(
      function.stats.decode(Arr(Bulk("running_script"), NullBulk, Bulk("engines"), engines)),
      ScriptingStats(None, Seq(ScriptingEngine("LUA", NonNegLong(2L), NonNegLong(3L))))
    )
    val running = Arr(Bulk("name"), Bulk("spin"), Bulk("command"), bulks("FCALL", "spin", "0"), Bulk("duration_ms"), Num(5002L))
    assertEquals(
      function.stats.decode(Arr(Bulk("running_script"), running, Bulk("engines"), engines)),
      ScriptingStats(
        Some(ScriptingRunning(Key("spin"), Seq("FCALL", "spin", "0"), NonNegLong(5002L))),
        Seq(ScriptingEngine("LUA", NonNegLong(2L), NonNegLong(3L)))
      )
    )
  }
}
