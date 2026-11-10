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

import java.nio.charset.StandardCharsets.UTF_8

/** What the server keeps sending on a connection of its own once asked, with no request behind each part: SUBSCRIBE, PSUBSCRIBE, SSUBSCRIBE
  * or MONITOR. `RedisSubscriber` opens the connection for one
  */
final class Feed[A] private[protocol] (
    private[laserdisc] val command: RESP,
    private[laserdisc] val confirmations: Set[String],
    private[laserdisc] val confirmed: RESP => Option[String],
    private[laserdisc] val message: RESP => Maybe[Option[A]]
)

object Feed {

  /** A message published to a channel */
  final case class Message[A](channel: Key, value: A)

  /** A message published to a channel that matches a pattern */
  final case class PatternMessage[A](pattern: GlobPattern, channel: Key, value: A)

  /** A message published to a shard channel */
  final case class ShardMessage[A](channel: Key, value: A)

  /** A command the server received, as MONITOR reports it: when, in seconds since the epoch, in which database, from which client (an
    * address, or `lua` for a script), and the command with its arguments
    */
  final case class MonitorEvent(time: BigDecimal, db: NonNegInt, client: String, command: Seq[String])

  /** A notice of client tracking: these keys changed, or all of them, as after FLUSHALL */
  sealed trait Invalidation
  object Invalidation {
    final case class Keys(keys: Seq[Key]) extends Invalidation
    case object Everything                extends Invalidation
  }

  private final def command(name: String, arguments: List[String]): RESP = Arr(Bulk(name) :: arguments.map(Bulk(_)))

  private final def confirmedAs(kind: String): RESP => Option[String] = {
    case Arr(Bulk(`kind`) :: Bulk(name) :: Num(_) :: Nil) => Some(name)
    case _                                                => None
  }

  private final def unexpected[A](resp: RESP): Maybe[Option[A]] = resp match {
    case err: Err => Left(err)
    case other    => Left(RESPDecErr(s"Unexpected message on a subscribed connection: $other"))
  }

  private final def keyOf(channel: String): Maybe[Key] = Read[Bulk, Key].read(Bulk(channel))

  private[protocol] final def subscribe[A: Read[Bulk, _]](channels: OneOrMoreKeys): Feed[Message[A]] = {
    val confirmation = confirmedAs("subscribe")
    new Feed[Message[A]](
      command("SUBSCRIBE", channels.value.map(_.value)),
      channels.value.map(_.value).toSet,
      confirmation,
      {
        case Arr(Bulk("message") :: Bulk(channel) :: (value: Bulk) :: Nil) =>
          for {
            key <- keyOf(channel)
            a   <- Read[Bulk, A].read(value)
          } yield Some(Message(key, a))
        case resp if confirmation(resp).nonEmpty => Right(None)
        case resp                                => unexpected(resp)
      }
    )
  }

  private[protocol] final def psubscribe[A: Read[Bulk, _]](patterns: OneOrMore[GlobPattern]): Feed[PatternMessage[A]] = {
    val confirmation = confirmedAs("psubscribe")
    val byName       = patterns.value.map(p => p.value -> p).toMap
    new Feed[PatternMessage[A]](
      command("PSUBSCRIBE", patterns.value.map(_.value)),
      byName.keySet,
      confirmation,
      {
        case Arr(Bulk("pmessage") :: Bulk(pattern) :: Bulk(channel) :: (value: Bulk) :: Nil) if byName.contains(pattern) =>
          for {
            key <- keyOf(channel)
            a   <- Read[Bulk, A].read(value)
          } yield Some(PatternMessage(byName(pattern), key, a))
        case resp if confirmation(resp).nonEmpty => Right(None)
        case resp                                => unexpected(resp)
      }
    )
  }

  private[protocol] final def ssubscribe[A: Read[Bulk, _]](channels: OneOrMoreKeys): Feed[ShardMessage[A]] = {
    val confirmation = confirmedAs("ssubscribe")
    new Feed[ShardMessage[A]](
      command("SSUBSCRIBE", channels.value.map(_.value)),
      channels.value.map(_.value).toSet,
      confirmation,
      {
        case Arr(Bulk("smessage") :: Bulk(channel) :: (value: Bulk) :: Nil) =>
          for {
            key <- keyOf(channel)
            a   <- Read[Bulk, A].read(value)
          } yield Some(ShardMessage(key, a))
        case resp if confirmation(resp).nonEmpty => Right(None)
        case resp                                => unexpected(resp)
      }
    )
  }

  private final val invalidationChannel = "__redis__:invalidate"

  private[protocol] final val invalidations: Feed[Invalidation] = {
    val confirmation = confirmedAs("subscribe")
    new Feed[Invalidation](
      command("SUBSCRIBE", List(invalidationChannel)),
      Set(invalidationChannel),
      confirmation,
      {
        case Arr(Bulk("message") :: Bulk(`invalidationChannel`) :: NullBulk :: Nil)  => Right(Some(Invalidation.Everything))
        case Arr(Bulk("message") :: Bulk(`invalidationChannel`) :: Arr(keys) :: Nil) =>
          keys
            .foldRight[Maybe[List[Key]]](Right(Nil)) {
              case (bulk: Bulk, rest) => rest.flatMap(found => Read[Bulk, Key].read(bulk).map(_ :: found))
              case (other, _)         => Left(RESPDecErr(s"Unexpected key in a tracking notice: $other"))
            }
            .map(found => Some(Invalidation.Keys(found)))
        case resp if confirmation(resp).nonEmpty => Right(None)
        case resp                                => unexpected(resp)
      }
    )
  }

  private final val MonitorLine = """(\d+\.\d+) \[(\d+) ([^\]]*)\] (.*)""".r

  // the command and its arguments, each in quotes, with what can't be shown as it is escaped
  private[protocol] final def monitorArguments(text: String): Option[List[String]] = {
    def hex(c: Char): Int = Character.digit(c, 16)

    @scala.annotation.tailrec
    def quoted(i: Int, bytes: Vector[Byte]): Option[(String, Int)] =
      if (i >= text.length) None
      else
        text.charAt(i) match {
          case '"'                                                      => Some(new String(bytes.toArray, UTF_8) -> (i + 1))
          case '\\' if i + 3 < text.length && text.charAt(i + 1) == 'x' =>
            quoted(i + 4, bytes :+ (hex(text.charAt(i + 2)) * 16 + hex(text.charAt(i + 3))).toByte)
          case '\\' if i + 1 < text.length =>
            val escaped = text.charAt(i + 1) match {
              case 'n'   => '\n'
              case 'r'   => '\r'
              case 't'   => '\t'
              case 'a'   => '\u0007'
              case 'b'   => '\b'
              case other => other
            }
            quoted(i + 2, bytes ++ escaped.toString.getBytes(UTF_8))
          case other =>
            quoted(i + 1, bytes ++ other.toString.getBytes(UTF_8))
        }

    @scala.annotation.tailrec
    def arguments(i: Int, found: List[String]): Option[List[String]] =
      if (i >= text.length) Some(found.reverse)
      else if (text.charAt(i) == ' ') arguments(i + 1, found)
      else if (text.charAt(i) != '"') None
      else
        quoted(i + 1, Vector.empty) match {
          case Some((argument, next)) => arguments(next, argument :: found)
          case None                   => None
        }

    arguments(0, Nil)
  }

  private[protocol] final val monitor: Feed[MonitorEvent] = {
    val confirmation: RESP => Option[String] = {
      case Str("OK") => Some("OK")
      case _         => None
    }
    new Feed[MonitorEvent](
      command("MONITOR", Nil),
      Set("OK"),
      confirmation,
      {
        case Str("OK")                                                         => Right(None)
        case resp @ Str(MonitorLine(time, ToInt(NonNegInt(db)), client, line)) =>
          monitorArguments(line).fold[Maybe[Option[MonitorEvent]]](unexpected(resp))(arguments =>
            Right(Some(MonitorEvent(BigDecimal(time), db, client, arguments)))
          )
        case resp => unexpected(resp)
      }
    )
  }
}

/** SUBSCRIBE and PSUBSCRIBE, for `RedisSubscriber`: Redis 6.2 */
trait SubscribeP extends Features {
  import Feed.{Message, PatternMessage}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("PSUBSCRIBE", "SUBSCRIBE")

  /** The messages published to the channels */
  final def subscribe[A: Read[Bulk, _]](channels: OneOrMoreKeys): Feed[Message[A]] = Feed.subscribe[A](channels)

  /** The messages published to channels that match the patterns */
  final def psubscribe[A: Read[Bulk, _]](patterns: OneOrMore[GlobPattern]): Feed[PatternMessage[A]] = Feed.psubscribe[A](patterns)
}

/** SSUBSCRIBE, for `RedisSubscriber`: Redis 7.0 */
trait SubscribeShardP extends Features {
  import Feed.ShardMessage

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SSUBSCRIBE")

  /** The messages published to the shard channels. In a cluster, connect to the node that serves their slot */
  final def ssubscribe[A: Read[Bulk, _]](channels: OneOrMoreKeys): Feed[ShardMessage[A]] = Feed.ssubscribe[A](channels)
}

/** MONITOR, for `RedisSubscriber`: Redis 6.2 */
trait MonitorP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("MONITOR")

  /** Every command the server receives, from every client. This slows the server down, so it's for debugging */
  final val monitor: Feed[Feed.MonitorEvent] = Feed.monitor
}
