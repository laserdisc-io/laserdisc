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

final class PublishPSpec extends BaseSpec with PublishShardP {
  private def bulks(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)
  private val channelNames            = OneOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))

  test("The Publish protocol publishes to a channel and to a shard channel, and reads how many subscribers received it") {
    assertEquals(publish(Key("c"), "m").encode, bulks("PUBLISH", "c", "m"))
    assertEquals(publish(Key("c"), "m").decode(Num(2L)), NonNegInt(2))
    assertEquals(spublish(Key("c"), "m").encode, bulks("SPUBLISH", "c", "m"))
    assertEquals(spublish(Key("c"), "m").decode(Num(0L)), NonNegInt(0))
  }

  test("The Publish protocol using pubsub reads the channels with a subscriber") {
    assertEquals(pubsub.channels.encode, bulks("PUBSUB", "CHANNELS"))
    assertEquals(pubsub.channels(GlobPattern("c*")).encode, bulks("PUBSUB", "CHANNELS", "c*"))
    assertEquals(pubsub.channels.decode(bulks("a", "b")), Seq(Key("a"), Key("b")))
    assertEquals(pubsub.channels.decode(Arr(Nil)), Seq.empty[Key])
    assertEquals(pubsub.shardchannels.encode, bulks("PUBSUB", "SHARDCHANNELS"))
    assertEquals(pubsub.shardchannels(GlobPattern("c*")).encode, bulks("PUBSUB", "SHARDCHANNELS", "c*"))
    assertEquals(pubsub.shardchannels.decode(bulks("a")), Seq(Key("a")))
  }

  test("The Publish protocol using pubsub reads how many subscribers there are") {
    assertEquals(pubsub.numpat.encode, bulks("PUBSUB", "NUMPAT"))
    assertEquals(pubsub.numpat.decode(Num(3L)), NonNegInt(3))
    assertEquals(pubsub.numsub(channelNames).encode, bulks("PUBSUB", "NUMSUB", "a", "b"))
    assertEquals(
      pubsub.numsub(channelNames).decode(Arr(Bulk("a"), Num(1L), Bulk("b"), Num(0L))),
      Seq(Key("a") -> NonNegInt(1), Key("b") -> NonNegInt(0))
    )
    assertEquals(pubsub.shardnumsub(channelNames).encode, bulks("PUBSUB", "SHARDNUMSUB", "a", "b"))
    assertEquals(
      pubsub.shardnumsub(channelNames).decode(Arr(Bulk("a"), Num(2L), Bulk("b"), Num(0L))),
      Seq(Key("a") -> NonNegInt(2), Key("b") -> NonNegInt(0))
    )
  }
}
