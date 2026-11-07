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

final class RedisScriptingClientSpec extends ScriptingClientSpec(6379, "redis") {
  test("a redis client loads, calls and lists functions with the commands of Redis 8.10")(functions(redis_8_10))
}
final class KeyDbScriptingClientSpec  extends ScriptingClientSpec(6380, "keyDb")
final class ValkeyScriptingClientSpec extends ScriptingClientSpec(6381, "valkey") {
  test("a valkey client loads, calls and lists functions with the commands of Valkey 9.1")(functions(valkey_9_1))

  test("a valkey client shows a cached script with the commands of Valkey 9.1") {
    import laserdisc.valkey_9_1.*

    val sha = send(script.load("return 'shown'"))
    assertEquals(send(script.show(sha)), "return 'shown'")
  }
}

abstract class ScriptingClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  protected final def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  private def failure[A](protocol: Protocol.Aux[A]): String =
    client.send(protocol).unsafeRunSync()(using runtime).fold(_.getMessage, a => fail(s"$dest should have failed but replied $a"))

  private val scriptKeys = OneOrMoreKeys.unsafeFrom(List(Key("s:a"), Key("s:b")))
  private val args       = OneOrMore.unsafeFrom(List[Value](1, "two", 3.5))

  test(s"a $dest client runs scripts with keys and arguments, and caches them") {
    import laserdisc.redis_6_2.*

    val counts = "return #KEYS .. ' ' .. #ARGV"
    assertEquals(send(eval[Bulk, String](counts)), "0 0")
    assertEquals(send(eval[Bulk, String](counts, scriptKeys)), "2 0")
    assertEquals(send(evalargs[Bulk, String](counts, args)), "0 3")
    assertEquals(send(eval[Bulk, String](counts, scriptKeys, args)), "2 3")
    assertEquals(send(eval[Bulk, String]("return KEYS[2] .. ARGV[2]", scriptKeys, args)), "s:btwo")
    assert(failure(eval[Num, Long]("return 'not a number'")).nonEmpty)

    assertEquals(send(eval[Arr.Of[Num], Seq[Long]]("return {#KEYS, #ARGV}", scriptKeys, args)), Seq(2L, 3L))
    assertEquals(send(eval[NullBulk, Unit]("return nil")), ())
    send(del("s:a"))
    assertEquals(send(eval[OrNullBulk[Bulk], Option[String]]("return redis.call('GET', KEYS[1])", scriptKeys)), None)
    send(set("s:a", "here"))
    assertEquals(send(eval[OrNullBulk[Bulk], Option[String]]("return redis.call('GET', KEYS[1])", scriptKeys)), Some("here"))

    send(script.flush)
    val sha = send(script.load(counts))
    assertEquals(send(script.exists(OneOrMore.unsafeFrom(List(sha)))), Seq(true))
    assertEquals(send(evalsha[Bulk, String](sha, scriptKeys, args)), "2 3")
    assertEquals(send(evalshaargs[Bulk, String](sha, args)), "0 3")
    assertEquals(send(script.flush(scriptingtypes.ScriptingFlushMode.sync)), OK)
    assertEquals(send(script.exists(OneOrMore.unsafeFrom(List(sha)))), Seq(false))
    assert(failure(evalsha[Bulk, String](sha)).startsWith("NOSCRIPT"))
    assert(failure(script.kill).startsWith("NOTBUSY"))
  }

  protected final def functions(version: Redis7_0): Unit = {
    import version.*
    import scriptingtypes.*

    val code =
      """#!lua name=sclib
        |redis.register_function{function_name='skeys', callback=function(keys, args) return #keys end, flags={'no-writes'}, description='counts keys'}
        |redis.register_function('sarg', function(keys, args) return args[1] end)""".stripMargin

    send(function.flush)
    assertEquals(send(function.load(code)), Key("sclib"))
    assert(failure(function.load(code)).nonEmpty)
    assertEquals(send(function.load(code, ScriptingReplace)), Key("sclib"))

    assertEquals(send(fcall[Num, Long]("skeys", scriptKeys)), 2L)
    assertEquals(send(fcallro[Num, Long]("skeys", scriptKeys, args)), 2L)
    assertEquals(send(fcallargs[Bulk, String]("sarg", args)), "1")
    assert(failure(fcallroargs[Bulk, String]("sarg", args)).nonEmpty)
    assertEquals(send(evalro[Num, Long]("return #KEYS", scriptKeys)), 2L)

    val listed = send(function.list(GlobPattern("sc*"), ScriptingWithCode))
    assertEquals(listed.map(_.name), Seq(Key("sclib")))
    assertEquals(
      listed.flatMap(_.functions).map(f => f.name -> f.description).toSet,
      Set[(Key, Option[String])](Key("skeys") -> Some("counts keys"), Key("sarg") -> None)
    )
    assertEquals(listed.flatMap(_.code), Seq(code))
    assertEquals(send(function.list(GlobPattern("nope*"))), Seq.empty[ScriptingLibrary])
    assertEquals(send(function.stats), ScriptingStats(None, Seq(ScriptingEngine("LUA", NonNegLong(1L), NonNegLong(2L)))))

    val dumped = send(function.dump)
    assert(failure(function.restore(dumped)).nonEmpty)
    assert(failure(function.restore(dumped, ScriptingRestorePolicy.append)).nonEmpty)
    assertEquals(send(function.restore(dumped, ScriptingRestorePolicy.replace)), OK)
    assertEquals(send(function.restore(dumped, ScriptingRestorePolicy.flush)), OK)
    send(function.flush)
    assertEquals(send(function.restore(dumped)), OK)
    assertEquals(send(function.list).map(_.name), Seq(Key("sclib")))

    assertEquals(send(function.delete("sclib")), OK)
    assert(failure(function.kill).startsWith("NOTBUSY"))
  }
}
