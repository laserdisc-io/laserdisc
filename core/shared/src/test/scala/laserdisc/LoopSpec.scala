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

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.*

final class LoopSpec extends BaseSpec {
  import LoopSpec.*
  import redis_8_10.streamtypes.*

  private def bulks(ss: String*): Arr               = Arr(ss.map(Bulk(_)).toList)
  private def entry(id: String, value: String): Arr = Arr(Bulk(id), bulks("a", value))
  private def id(ms: Long, seq: Long): StreamId     = StreamId(NonNegLong.unsafeFrom(ms), NonNegLong.unsafeFrom(seq))

  test("A loop sends each command once the previous reply is in, until a reply says it's done") {
    val client = new Scripted(Arr(Bulk("5"), bulks("a", "b")), Arr(Bulk("0"), bulks("c")))
    assertEquals(client.stream(redis_8_10.scanAll(GlobPattern("k*"), PosInt(10))), List(Key("a"), Key("b"), Key("c")) -> None)
    assertEquals(client.commands, List(bulks("SCAN", "0", "MATCH", "k*", "COUNT", "10"), bulks("SCAN", "5", "MATCH", "k*", "COUNT", "10")))
    assertEquals(client.timeouts, List(20.seconds, 20.seconds))
  }

  test("A loop stops at the first failed reply") {
    val client = new Scripted(Arr(Bulk("5"), bulks("a")), Err("ERR no"))
    assertEquals(client.stream(redis_8_10.scanAll), List(Key("a")) -> Some(Err("ERR no")))
  }

  test("The cursor loops send their command with each cursor, for keys, hashes, sets and sorted sets") {
    val replies = List(Arr(Bulk("3"), bulks("f", "1")), Arr(Bulk("0"), NilArr))

    val hashes = new Scripted(replies*)
    assertEquals(hashes.stream(redis_8_10.hscanAll(Key("h"), PosInt(2))), List(KV(Key("f"), "1")) -> None)
    assertEquals(hashes.commands, List(bulks("HSCAN", "h", "0", "COUNT", "2"), bulks("HSCAN", "h", "3", "COUNT", "2")))

    val sets = new Scripted(replies*)
    assertEquals(sets.stream(redis_8_10.sscanAll[String](Key("s"))), List("f", "1") -> None)
    assertEquals(sets.commands, List(bulks("SSCAN", "s", "0"), bulks("SSCAN", "s", "3")))

    val sortedSets = new Scripted(replies*)
    assertEquals(sortedSets.stream(redis_8_10.zscanAll[String](Key("z"), GlobPattern("f*"))), List("f" -> 1.0) -> None)
    assertEquals(sortedSets.commands, List(bulks("ZSCAN", "z", "0", "MATCH", "f*"), bulks("ZSCAN", "z", "3", "MATCH", "f*")))

    val fields = new Scripted(replies*)
    assertEquals(fields.stream(redis_8_10.hscannovaluesAll(Key("h"))), List(Key("f"), Key("1")) -> None)
    assertEquals(fields.commands, List(bulks("HSCAN", "h", "0", "NOVALUES"), bulks("HSCAN", "h", "3", "NOVALUES")))

    val members = new Scripted(replies*)
    assertEquals(members.stream(valkey_9_1.zscannoscoresAll[String](Key("z"), PosInt(4))), List("f", "1") -> None)
    assertEquals(
      members.commands,
      List(bulks("ZSCAN", "z", "0", "COUNT", "4", "NOSCORES"), bulks("ZSCAN", "z", "3", "COUNT", "4", "NOSCORES"))
    )

    val keys = new Scripted(replies*)
    assertEquals(keys.stream(redis_8_10.scanAll(PosInt(5), redis_8_10.keytypes.KeyType.hash)), List(Key("f"), Key("1")) -> None)
    assertEquals(keys.commands.head, bulks("SCAN", "0", "COUNT", "5", "TYPE", "hash"))
  }

  test("xrangeAll reads a page at a time, from after the last entry, until a page comes short") {
    val client           = new Scripted(Arr(entry("1-1", "1"), entry("1-2", "2")), Arr(entry("2-0", "3")))
    val (entries, ended) = client.stream(redis_8_10.xrangeAll[Int](Key("x"), PosLong(2L)))
    assertEquals(entries.map(_.id), List(id(1, 1), id(1, 2), id(2, 0)))
    assertEquals(ended, None)
    assertEquals(client.commands, List(bulks("XRANGE", "x", "-", "+", "COUNT", "2"), bulks("XRANGE", "x", "(1-2", "+", "COUNT", "2")))
  }

  test("xrevrangeAll reads a page at a time from the end, from before the last entry, until a page comes short") {
    val client           = new Scripted(Arr(entry("2-0", "3"), entry("1-2", "2")), Arr(Nil))
    val (entries, ended) = client.stream(redis_8_10.xrevrangeAll[Int](Key("x"), PosLong(2L)))
    assertEquals(entries.map(_.id), List(id(2, 0), id(1, 2)))
    assertEquals(ended, None)
    assertEquals(client.commands, List(bulks("XREVRANGE", "x", "+", "-", "COUNT", "2"), bulks("XREVRANGE", "x", "(1-2", "-", "COUNT", "2")))
  }

  test("xreadFollow waits for entries, reads on after the last one of each stream, and adds its wait to each timeout") {
    val client = new Scripted(NilArr, Arr(Arr(Bulk("x"), Arr(entry("1-1", "1"), entry("1-2", "2")))))
    val follow =
      redis_8_10.blocking
        .xreadFollow[Int](OneOrMore.unsafeFrom(List(Key("x") -> StreamFrom.latest, Key("y") -> StreamFrom.latest)), PosLong(100L))
    val (values, ended) = client.stream(follow, 1.second)
    assertEquals(values.map(kv => kv.key -> kv.value.id), List(Key("x") -> id(1, 1), Key("x") -> id(1, 2)))
    assertEquals(ended, Some(ScriptEnded))
    assertEquals(
      client.commands,
      List(
        bulks("XREAD", "BLOCK", "100", "STREAMS", "x", "y", "$", "$"),
        bulks("XREAD", "BLOCK", "100", "STREAMS", "x", "y", "$", "$"),
        bulks("XREAD", "BLOCK", "100", "STREAMS", "x", "y", "1-2", "$")
      )
    )
    assertEquals(client.timeouts.distinct, List(1100.millis))
  }

  test("xreadgroupFollow reads the consumer's pending entries first, then new entries") {
    val client = new Scripted(Arr(Arr(Bulk("x"), Arr(entry("1-1", "1")))), Arr(Arr(Bulk("x"), Arr(Nil))), NilArr)
    val follow = redis_8_10.blocking.xreadgroupFollow[Int](
      Key("g"),
      Key("c"),
      OneOrMore.unsafeFrom(List(Key("x") -> StreamGroupFrom.pendingAfter(id(0, 0)))),
      PosLong(50L),
      PosLong(10L)
    )
    assertEquals(client.stream(follow)._1.map(_.value.id), List(id(1, 1)))
    assertEquals(
      client.commands.map(_.elements.takeRight(2)),
      List(List(Bulk("x"), Bulk("0-0")), List(Bulk("x"), Bulk("1-1")), List(Bulk("x"), Bulk(">")), List(Bulk("x"), Bulk(">")))
    )
  }
}

object LoopSpec {
  type Id[A]        = A
  type Collected[A] = (List[A], Option[Throwable])

  case object ScriptEnded extends RuntimeException("no more replies")

  implicit final val idFunctor: Functor[Id] = new Functor[Id] {
    override def map[A, B](fa: A)(f: A => B): B = f(fa)
  }

  // the values, and the failure that ended the loop, if any
  implicit final val collect: Unfold[Id, Collected] = new Unfold[Id, Collected] {
    override def unfold[St, A](start: St)(next: St => Maybe[(Seq[A], Option[St])]): Collected[A] = {
      @tailrec def go(st: St, acc: List[A]): Collected[A] = next(st) match {
        case Left(e)                => acc         -> Some(e)
        case Right((as, Some(st1))) => go(st1, acc ++ as)
        case Right((as, None))      => (acc ++ as) -> None
      }
      go(start, Nil)
    }
  }

  // a client that answers each command with the next reply, and remembers what it sent and with which timeout
  final class Scripted(replies: RESP*) extends ClientBase[Id, Scripted] {
    private val remaining = ListBuffer(replies*)
    private val sent      = ListBuffer.empty[(Arr, FiniteDuration)]
    private var timeout   = defaultTimeout

    def commands: List[Arr]            = sent.map(_._1).toList
    def timeouts: List[FiniteDuration] = sent.map(_._2).toList

    def answer(command: Seq[RESP]): Maybe[RESP] = {
      command.foreach { case arr: Arr => sent += arr -> timeout; case _ => () }
      if (remaining.isEmpty) Left(ScriptEnded) else Right(remaining.remove(0))
    }

    override def send[In <: Tuple, Out <: Tuple](in: In, timeout: FiniteDuration)(
        implicit handler: Handler.Aux[Id, Scripted, In, Out]
    ): Out = {
      this.timeout = timeout
      handler(this, in)
    }
  }

  implicit final def scriptedHandler[A]: Handler.Aux[Id, Scripted, Protocol[A] *: EmptyTuple, Maybe[A] *: EmptyTuple] =
    new Handler[Id, Scripted, Protocol[A] *: EmptyTuple] {
      override type LOut = Maybe[A] *: EmptyTuple
      override def apply(env: Scripted, in: Protocol[A] *: EmptyTuple): Maybe[A] *: EmptyTuple = in match {
        case protocol *: _ =>
          env.answer(protocol.frames).flatMap(reply => protocol.decodeReplies(Seq(reply))) *: EmptyTuple
      }
    }
}
