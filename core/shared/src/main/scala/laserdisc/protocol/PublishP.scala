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

import Protocol.Single

/** PUBSUB, under `pubsub`. Redis 6.2 */
trait PubSubCommands extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("PUBSUB CHANNELS", "PUBSUB HELP", "PUBSUB NUMPAT", "PUBSUB NUMSUB")

  val help: Single[Seq[String]] = Protocol.help("PUBSUB")

  /** The channels with a subscriber, or those whose name matches the pattern */
  val channels: Single[Seq[Key]]                       = Protocol("PUBSUB", "CHANNELS").as[Arr.Of[Bulk], Seq[Key]]
  def channels(pattern: GlobPattern): Single[Seq[Key]] =
    Protocol("PUBSUB", "CHANNELS" *: pattern *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]

  /** How many patterns clients are subscribed to */
  val numpat: Single[NonNegInt] = Protocol("PUBSUB", "NUMPAT").as[Num, NonNegInt]

  /** How many subscribers each channel has, not counting those subscribed by a pattern */
  def numsub(channels: OneOrMoreKeys): Single[Seq[(Key, NonNegInt)]] =
    Protocol("PUBSUB", "NUMSUB" :: channels.value.map(_.value)).as[Arr.OfPairs[Bulk, Num], Seq[(Key, NonNegInt)]]
  def numsub(channel: Key, channels: Key*): Single[Seq[(Key, NonNegInt)]] =
    numsub(OneOrMoreKeys.unsafeFrom(channel :: channels.toList))
}

/** PUBSUB for shard channels, under `pubsub`. Redis 7.0 */
trait PubSubShardCommands extends PubSubCommands {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("PUBSUB SHARDCHANNELS", "PUBSUB SHARDNUMSUB")

  /** The shard channels with a subscriber, or those whose name matches the pattern */
  val shardchannels: Single[Seq[Key]]                       = Protocol("PUBSUB", "SHARDCHANNELS").as[Arr.Of[Bulk], Seq[Key]]
  def shardchannels(pattern: GlobPattern): Single[Seq[Key]] =
    Protocol("PUBSUB", "SHARDCHANNELS" *: pattern *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]

  /** How many subscribers each shard channel has */
  def shardnumsub(channels: OneOrMoreKeys): Single[Seq[(Key, NonNegInt)]] =
    Protocol("PUBSUB", "SHARDNUMSUB" :: channels.value.map(_.value)).as[Arr.OfPairs[Bulk, Num], Seq[(Key, NonNegInt)]]
  def shardnumsub(channel: Key, channels: Key*): Single[Seq[(Key, NonNegInt)]] =
    shardnumsub(OneOrMoreKeys.unsafeFrom(channel :: channels.toList))
}

trait PublishP extends SubscribeP {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "PUBLISH"
    ) ++ pubsub.supported

  lazy val pubsub: PubSubCommands = new PubSubCommands {}

  final def publish[A: Show](channel: Key, value: A): Single[NonNegInt] =
    Protocol("PUBLISH", channel *: value *: EmptyTuple).as[Num, NonNegInt]
}

/** SPUBLISH, and PUBSUB for shard channels under `pubsub`. Redis 7.0 */
trait PublishShardP extends PublishP with SubscribeShardP {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("SPUBLISH")

  override lazy val pubsub: PubSubShardCommands = new PubSubShardCommands {}

  /** Publishes to a shard channel, and replies with how many subscribers received it. In a cluster, send it to the node holding the
    * channel's slot
    */
  final def spublish[A: Show](channel: Key, value: A): Single[NonNegInt] =
    Protocol("SPUBLISH", channel *: value *: EmptyTuple).as[Num, NonNegInt]
}
