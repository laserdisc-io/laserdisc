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

import laserdisc.protocol.Features

import scala.io.Source

// checks each version against what that server version says it has, from core/jvm/src/test/resources/servers/
final class VersionsSpec extends BaseSpec {

  private def server(name: String): Map[String, Set[String]] = {
    val source = Source.fromResource(s"servers/$name.txt")
    try
      source
        .getLines()
        .map { line =>
          val (command, keywords) = line.span(_ != ':')
          command -> keywords.drop(1).trim.split(' ').filter(_.nonEmpty).toSet
        }
        .toMap
    finally source.close()
  }

  private def covered(features: Features): Map[String, Set[String]] =
    features.supported
      .map { entry =>
        val (command, keywords) = entry.span(_ != ':')
        command -> keywords.drop(1).trim.split(' ').filter(_.nonEmpty).toSet
      }
      .groupBy(_._1)
      .map { case (command, entries) => command -> entries.flatMap(_._2).toSet }

  private def check(features: Features, name: String): Unit = {
    val has  = server(name)
    val ours = covered(features)

    val notOnServer = ours.toList.sortBy(_._1).flatMap { case (command, keywords) =>
      has.get(command) match {
        case None           => List(command)
        case Some(accepted) => (keywords -- accepted).toList.sorted.map(k => s"$command $k")
      }
    }
    assert(notOnServer.isEmpty, s"$name doesn't have: ${notOnServer.mkString(", ")}")

    val missing = ours.keys.toList.sorted.flatMap(c => (has(c) -- ours(c)).toList.sorted.map(k => s"$c $k"))
    if (missing.nonEmpty) println(s"--- $name has, but laserdisc doesn't: ${missing.mkString(", ")}")
  }

  test("redis_6_2 matches Redis 6.2")(check(redis_6_2, "redis-6.2"))
  test("redis_7_0 matches Redis 7.0")(check(redis_7_0, "redis-7.0"))
  test("redis_7_2 matches Redis 7.2")(check(redis_7_2, "redis-7.2"))
  test("redis_7_4 matches Redis 7.4")(check(redis_7_4, "redis-7.4"))
  test("redis_8_0 matches Redis 8.0")(check(redis_8_0, "redis-8.0"))
  test("redis_8_2 matches Redis 8.2")(check(redis_8_2, "redis-8.2"))
  test("redis_8_4 matches Redis 8.4")(check(redis_8_4, "redis-8.4"))
  test("redis_8_6 matches Redis 8.6")(check(redis_8_6, "redis-8.6"))
  test("redis_8_8 matches Redis 8.8")(check(redis_8_8, "redis-8.8"))
  test("redis_8_10 matches Redis 8.10")(check(redis_8_10, "redis-8.10"))
  test("valkey_7_2 matches Valkey 7.2")(check(valkey_7_2, "valkey-7.2"))
  test("valkey_8_0 matches Valkey 8.0")(check(valkey_8_0, "valkey-8.0"))
  test("valkey_8_1 matches Valkey 8.1")(check(valkey_8_1, "valkey-8.1"))
  test("valkey_9_0 matches Valkey 9.0")(check(valkey_9_0, "valkey-9.0"))
  test("valkey_9_1 matches Valkey 9.1")(check(valkey_9_1, "valkey-9.1"))

  test("a version claiming something its server doesn't have fails") {
    object wrong extends Redis8_2 with protocol.StringSetIfEqP
    assert(scala.util.Try(check(wrong, "redis-8.2")).isFailure)
  }
}
