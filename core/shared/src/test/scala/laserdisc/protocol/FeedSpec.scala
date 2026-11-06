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

import scodec.bits._

final class FeedSpec extends BaseSpec with PublishShardP with MonitorP {
  import Feed.{Message, MonitorEvent, PatternMessage, ShardMessage}

  private def bulks(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)
  private val keys                    = OneOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))

  test("A subscribe feed sends the channels, waits for each, and reads messages as the type asked for") {
    val feed = subscribe[Int](keys)
    assertEquals(feed.command, bulks("SUBSCRIBE", "a", "b"))
    assertEquals(feed.confirmations, Set("a", "b"))
    assertEquals(feed.confirmed(Arr(Bulk("subscribe"), Bulk("a"), Num(1L))), Some("a"))
    assertEquals(feed.confirmed(bulks("message", "a", "1")), None)
    assertEquals(feed.message(bulks("message", "a", "1")), Some(Message(Key("a"), 1)))
    assertEquals(feed.message(Arr(Bulk("subscribe"), Bulk("a"), Num(1L))), None)
    assert(feed.message(bulks("message", "a", "x")).isLeft)
    assert(feed.message(bulks("smessage", "a", "1")).isLeft)
    assertLeftEquals(feed.message(Err("boom")), Err("boom"))
  }

  test("A subscribe feed keeps the bytes of a message") {
    val payload = Bulk(hex"f5c30301ff")
    assertEquals(subscribe[Bulk](keys).message(Arr(Bulk("message"), Bulk("a"), payload)), Some(Message(Key("a"), payload)))
  }

  test("A psubscribe feed sends the patterns, and reads the pattern and channel of each message") {
    val feed = psubscribe[String](OneOrMore.unsafeFrom(List(GlobPattern("a*"))))
    assertEquals(feed.command, bulks("PSUBSCRIBE", "a*"))
    assertEquals(feed.confirmed(Arr(Bulk("psubscribe"), Bulk("a*"), Num(1L))), Some("a*"))
    assertEquals(feed.message(bulks("pmessage", "a*", "ab", "m")), Some(PatternMessage(GlobPattern("a*"), Key("ab"), "m")))
    assert(feed.message(bulks("pmessage", "z*", "ab", "m")).isLeft)
  }

  test("An ssubscribe feed sends the channels, and reads each message") {
    val feed = ssubscribe[String](keys)
    assertEquals(feed.command, bulks("SSUBSCRIBE", "a", "b"))
    assertEquals(feed.confirmed(Arr(Bulk("ssubscribe"), Bulk("a"), Num(1L))), Some("a"))
    assertEquals(feed.message(bulks("smessage", "a", "m")), Some(ShardMessage(Key("a"), "m")))
  }

  test("The monitor feed reads each server's line, and what's escaped in it") {
    assertEquals(monitor.command, bulks("MONITOR"))
    assertEquals(monitor.confirmed(Str("OK")), Some("OK"))
    assertEquals(
      monitor.message(Str("""1793945914.511813 [0 172.17.0.1:52012] "SET" "k" "1"""")),
      Some(MonitorEvent(BigDecimal("1793945914.511813"), NonNegInt(0), "172.17.0.1:52012", Seq("SET", "k", "1")))
    )
    assertEquals(
      monitor.message(Str("""1793945915.2341746 [3 lua] "get" "k"""")),
      Some(MonitorEvent(BigDecimal("1793945915.2341746"), NonNegInt(3), "lua", Seq("get", "k")))
    )
    assertEquals(
      Feed.monitorArguments("\"a \\\"b\\\" c\" \"d\\\\e\" \"f\\ng\\r\\t\" \"\\xc3\\xa9\" "),
      Some(List("a \"b\" c", "d\\e", "f\ng\r\t", "\u00e9"))
    )
    assertEquals(Feed.monitorArguments("\"not closed"), None)
    assertEquals(Feed.monitorArguments("not quoted"), None)
    assert(monitor.message(Str("something else")).isLeft)
  }
}
