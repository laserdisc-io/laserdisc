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

final class RedisArrayClientSpec extends LaserdiscFs2Suite(LiveServers.port(6379)) {
  private def send[A](protocol: Protocol[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"redis replied with an error: ${e.getMessage}", e), identity)

  private def failure[A](protocol: Protocol[A]): String =
    client.send(protocol).unsafeRunSync()(using runtime).fold(_.getMessage, r => fail(s"expected an error, got $r"))

  private def indexes(ns: NonNegLong*): OneOrMore[NonNegLong] = OneOrMore.unsafeFrom(ns.toList)

  since[Redis8_8]("a redis client sets, gets, deletes and scans elements, with gaps") { v =>
    import v.*

    send(del("ar:s"))
    assertEquals(send(arset("ar:s", 0L, OneOrMore.unsafeFrom(List("x", "y", "z")))).value, 3L)
    assertEquals(send(armset("ar:s", OneOrMore.unsafeFrom(List[(NonNegLong, String)](NonNegLong(10L) -> "w")))).value, 1L)

    assertEquals(send(arget[String]("ar:s", 1L)), Some("y"))
    assertEquals(send(arget[String]("ar:s", 5L)), None)
    assertEquals(send(armget[String]("ar:s", indexes(0L, 5L))), Seq(Some("x"), None))
    assertEquals(send(argetrange[String]("ar:s", 2L, 3L)), Seq(Some("z"), None))
    assertEquals(send(arlen("ar:s")).value, 11L)
    assertEquals(send(arcount("ar:s")).value, 4L)

    assertEquals(
      send(arscan[String]("ar:s", 0L, 20L)).map { case (i, v) => i.value -> v },
      Seq(0L -> "x", 1L -> "y", 2L -> "z", 10L -> "w")
    )
    assertEquals(send(arscan[String]("ar:s", 0L, 20L, 1L)).map(_._2), Seq("x"))

    assertEquals(send(ardel("ar:s", indexes(0L, 999L))).value, 1L)
    assertEquals(send(ardelrange("ar:s", OneOrMore.unsafeFrom(List[(NonNegLong, NonNegLong)](NonNegLong(1L) -> NonNegLong(2L))))).value, 2L)
    assertEquals(send(arcount("ar:s")).value, 1L)
  }

  since[Redis8_8]("a redis client inserts at the cursor, and uses a ring buffer") { v =>
    import v.*
    import v.arraytypes.*

    send(del("ar:i", "ar:r"))
    assertEquals(send(arseek("ar:i", 3L)), false)
    assertEquals(send(arinsert("ar:i", OneOrMore.unsafeFrom(List("a", "b")))).value, 1L)
    assertEquals(send(arnext("ar:i")).value, 2L)
    assertEquals(send(arseek("ar:i", 10L)), true)
    assertEquals(send(arinsert("ar:i", OneOrMore.unsafeFrom(List("c")))).value, 10L)
    assertEquals(send(arlastitems[String]("ar:i", 1L)), Seq(Some("c")))
    assertEquals(send(arlastitems[String]("ar:i", 2L, ArrayRev)), Seq(Some("c"), None))

    send(arring("ar:r", 2L, OneOrMore.unsafeFrom(List("a", "b", "c"))))
    assertEquals(send(argetrange[String]("ar:r", 0L, 1L)), Seq(Some("c"), Some("b")))
  }

  since[Redis8_8]("a redis client searches elements") { v =>
    import v.*
    import v.arraytypes.*

    send(del("ar:g"))
    send(arset("ar:g", 0L, OneOrMore.unsafeFrom(List("aa", "Ab", "bc"))))
    val all  = (ArrayBound.min, ArrayBound.max)
    val a    = OneOrMore.unsafeFrom(List(ArrayPredicate.matching("a")))
    val aOrB = OneOrMore.unsafeFrom(List(ArrayPredicate.matching("a"), ArrayPredicate.exact("bc")))

    assertEquals(send(argrep("ar:g", all._1, all._2, a)).map(_.value), Seq(0L))
    assertEquals(send(argrep("ar:g", all._1, all._2, a, ArrayGrepOptions.nocase)).map(_.value), Seq(0L, 1L))
    assertEquals(send(argrep("ar:g", all._1, all._2, aOrB, ArrayGrepOptions.and)).map(_.value), Seq.empty[Long])
    assertEquals(send(argrep("ar:g", all._1, all._2, aOrB, ArrayGrepOptions.or)).map(_.value), Seq(0L, 2L))
    assertEquals(
      send(argrep("ar:g", ArrayBound.at(1L), ArrayBound.at(2L), OneOrMore.unsafeFrom(List(ArrayPredicate.glob("b*"))))).map(_.value),
      Seq(2L)
    )
    assertEquals(send(argrep("ar:g", all._1, all._2, OneOrMore.unsafeFrom(List(ArrayPredicate.re("^A"))))).map(_.value), Seq(1L))
    assertEquals(send(argrepwithvalues[String]("ar:g", all._1, all._2, a)).map { case (i, v) => i.value -> v }, Seq(0L -> "aa"))
    assertEquals(
      send(argrepwithvalues[String]("ar:g", all._1, all._2, aOrB, ArrayGrepOptions.or.limit(1L))).map { case (i, v) => i.value -> v },
      Seq(0L -> "aa")
    )
  }

  since[Redis8_8]("a redis client aggregates elements") { v =>
    import v.*
    import v.arraytypes.*

    send(del("ar:n", "ar:d", "ar:none"))
    send(arset("ar:n", 0L, OneOrMore.unsafeFrom(List(1, 2, 3))))
    send(arset("ar:d", 0L, OneOrMore.unsafeFrom(List(1.5d, 2.5d))))

    assertEquals(send(arop("ar:n", 0L, 5L, ArrayOp.sum)), Some(6d))
    assertEquals(send(arop("ar:d", 0L, 5L, ArrayOp.min)), Some(1.5d))
    assertEquals(send(arop("ar:d", 0L, 5L, ArrayOp.max)), Some(2.5d))
    assertEquals(send(arop("ar:none", 0L, 5L, ArrayOp.sum)), None)
    assertEquals(send(arop("ar:n", 0L, 5L, ArrayOp.and)), Some(0L))
    assertEquals(send(arop("ar:n", 0L, 5L, ArrayOp.or)), Some(3L))
    assertEquals(send(arop("ar:n", 0L, 5L, ArrayOp.xor)), Some(0L))
    assertEquals(send(arop("ar:none", 0L, 5L, ArrayOp.and)), None)
    assertEquals(send(arop("ar:n", 0L, 5L, ArrayOp.used)).value, 3L)
    assertEquals(send(arop("ar:n", 0L, 5L, ArrayOp.matching(2))).value, 1L)
  }

  since[Redis8_8]("a redis client reads an array's information") { v =>
    import v.*
    import v.arraytypes.*

    send(del("ar:f"))
    assert(failure(arinfo("ar:f")).contains("no such key"))
    send(arset("ar:f", 3L, OneOrMore.unsafeFrom(List("x"))))

    val info = send(arinfo("ar:f"))
    assertEquals(info.count.value, 1L)
    assertEquals(info.length.value, 4L)
    assertEquals(send(arinfo("ar:f", ArrayFull)).info, info)
  }
}
