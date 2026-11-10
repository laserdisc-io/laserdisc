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

object DragonflyP {

  /** KEEPTTL, for SADDEX: members already in the set keep their expiry, and only new ones get this one */
  sealed trait KeepTtl
  object KeepTtl extends KeepTtl {
    implicit val keepTtlShow: Show[KeepTtl] = Show.const("KEEPTTL")
  }

  /** What FIELDEXPIRE did to a field: set its expiry, deleted it, or found no such field or key */
  sealed trait FieldExpireResult
  object FieldExpireResult {
    case object Set     extends FieldExpireResult
    case object Deleted extends FieldExpireResult
    case object Missing extends FieldExpireResult

    implicit final val num2FieldExpireResultRead: Read[Num, FieldExpireResult] = Read.instance {
      case Num(1L)    => Right(Set)
      case Num(2L)    => Right(Deleted)
      case Num(-2L)   => Right(Missing)
      case Num(other) => Left(RESPDecErr(s"Unexpected FIELDEXPIRE result. Was $other"))
    }
  }

  /** FIELDTTL's reply: the seconds a field has left, or that it never expires, or that there's no such field or key */
  sealed trait FieldTtl
  object FieldTtl {
    final case class Expires(seconds: NonNegLong) extends FieldTtl
    case object NoExpiry                          extends FieldTtl
    case object NoKey                             extends FieldTtl
    case object NoField                           extends FieldTtl

    implicit final val num2FieldTtlRead: Read[Num, FieldTtl] = Read.instance {
      case Num(-1L) => Right(NoExpiry)
      case Num(-2L) => Right(NoKey)
      case Num(-3L) => Right(NoField)
      case other    => Read[Num, NonNegLong].read(other).map(Expires(_))
    }
  }

  /** CL.THROTTLE's reply: whether the action was refused, the total limit, what's left of it, how long to wait to retry when it was
    * refused, and how long until the limit is fully restored, both in seconds
    */
  final case class Throttle(
      limited: Boolean,
      limit: NonNegLong,
      remaining: NonNegLong,
      retryAfterSeconds: Option[NonNegLong],
      resetAfterSeconds: NonNegLong
  )

  private final type ThrottleReply = Arr.AsTuple[Num *: Num *: Num *: Num *: Num *: EmptyTuple]
  private[protocol] final val throttle: Read[ThrottleReply, Throttle] = Read.instance { reply =>
    Read[ThrottleReply, Boolean *: NonNegLong *: NonNegLong *: Num *: NonNegLong *: EmptyTuple].read(reply).flatMap {
      case limited *: limit *: remaining *: retry *: reset *: _ =>
        Read.numMinusOneIsNone[NonNegLong].read(retry).map(Throttle(limited, limit, remaining, _, reset))
    }
  }

  /** ROLE as Dragonfly replies: a primary and its replicas, or a replica and its primary, each with its state, like `online`. There are no
    * replication offsets. A replica of a cluster's node can also replicate other primaries, which ADDREPLICAOF adds
    */
  sealed trait Role
  object Role {
    final case class Replica(host: Host, port: Port, state: String)
    final case class Primary(replicas: Seq[Replica])                                                     extends Role
    final case class Follower(host: Host, port: Port, state: String, otherPrimaries: Seq[Replica] = Nil) extends Role

    private type ReplicaReply  = Arr.AsTuple[Bulk *: Bulk *: Bulk *: EmptyTuple]
    private type PrimaryReply  = Arr.AsTuple[Bulk *: Arr.Of[ReplicaReply] *: EmptyTuple]
    private type FollowerReply = Arr.AsTupleThen[Bulk *: Bulk *: Bulk *: Bulk *: EmptyTuple, Bulk]

    private implicit final val hostRead: Read[Bulk, Host] = Read.instancePF("Bulk(Host)") { case Bulk(Host(h)) => h }
    private implicit final val portRead: Read[Bulk, Port] = Read.instancePF("Bulk(Port)") { case Bulk(ToInt(Port(p))) => p }

    private implicit final val replicaRead: Read[ReplicaReply, Replica] =
      Read[ReplicaReply, Host *: Port *: String *: EmptyTuple].map { case host *: port *: state *: _ => Replica(host, port, state) }
    private implicit final val primaryRead: Read[PrimaryReply, Role] = Read.instance { reply =>
      Read[PrimaryReply, String *: Seq[Replica] *: EmptyTuple].read(reply).flatMap {
        case "master" *: replicas *: _ => Right(Primary(replicas))
        case name *: _                 => Left(RESPDecErr(s"Expected the role master but was $name"))
      }
    }
    // each other primary as its host, port and state
    private final def otherPrimaries(rest: Seq[String]): Either[RESPDecErr, List[Replica]] =
      rest.grouped(3).foldRight[Either[RESPDecErr, List[Replica]]](Right(Nil)) {
        case (Seq(Host(host), ToInt(Port(port)), state), others) => others.map(Replica(host, port, state) :: _)
        case (other, _) => Left(RESPDecErr(s"Expected another primary's host, port and state but got ${other.mkString(" ")}"))
      }

    private implicit final val followerRead: Read[FollowerReply, Role] = Read.instance { reply =>
      Read[FollowerReply, (String *: Host *: Port *: String *: EmptyTuple, Seq[String])].read(reply).flatMap {
        // "replica" when Dragonfly runs with --info_replication_valkey_compatible=false
        case (("slave" | "replica") *: host *: port *: state *: _, rest) => otherPrimaries(rest).map(Follower(host, port, state, _))
        case (name *: _, _)                                              => Left(RESPDecErr(s"Expected the role slave but was $name"))
      }
    }

    private[protocol] final val roleRead: Read[OneOf[PrimaryReply *: FollowerReply *: EmptyTuple], Role] =
      Read[OneOf[PrimaryReply *: FollowerReply *: EmptyTuple], Role]
  }

  /** MATCH, TYPE and COUNT, for RM: which keys to delete, and roughly how many to look at in one call */
  final class RmOptions private (val params: List[String]) {
    def pattern(pattern: GlobPattern): RmOptions = new RmOptions(params ++ List("MATCH", pattern.value))
    def keyType(keyType: KeyP.Type): RmOptions   = new RmOptions(params ++ List("TYPE", Show[KeyP.Type].show(keyType)))
    def count(count: PosInt): RmOptions          = new RmOptions(params ++ List("COUNT", Show[PosInt].show(count)))
  }
  object RmOptions {
    private final val none = new RmOptions(Nil)

    def pattern(pattern: GlobPattern): RmOptions = none.pattern(pattern)
    def keyType(keyType: KeyP.Type): RmOptions   = none.keyType(keyType)
    def count(count: PosInt): RmOptions          = none.count(count)
  }

  /** RM's reply: the cursor to go on from, which is 0 when it's done, and how many keys this call deleted */
  final case class Rm(next: NonNegLong, deleted: NonNegLong)

  private[protocol] final val rm: Read[Arr.AsTuple[Bulk *: Num *: EmptyTuple], Rm] =
    Read[Arr.AsTuple[Bulk *: Num *: EmptyTuple], NonNegLong *: NonNegLong *: EmptyTuple].map { case next *: deleted *: _ =>
      Rm(next, deleted)
    }
}

/** The commands only Dragonfly has */
trait DragonflyP extends Features {
  import DragonflyP.{FieldExpireResult, FieldTtl, KeepTtl, Rm, RmOptions, Role, Throttle}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("CL.THROTTLE", "FIELDEXPIRE", "FIELDTTL", "PREPEND", "REPLTAKEOVER", "RM", "ROLE", "SADDEX", "STICK")

  object dragonflytypes {
    final val fieldExpireResult = FieldExpireResult
    final val fieldTtl          = FieldTtl
    final val keepTtl           = KeepTtl
    final val rmOptions         = RmOptions
    final val role              = Role
  }

  /** Allows or refuses an action on the key, with the generic cell rate algorithm: `count` actions per `periodSeconds`, with bursts of up
    * to `maxBurst` more, and each action counting as `quantity` of them (1 when not given)
    */
  final def clthrottle(key: Key, maxBurst: NonNegLong, count: PosLong, periodSeconds: PosLong): Single[Throttle] =
    Protocol("CL.THROTTLE", key *: maxBurst *: count *: periodSeconds *: EmptyTuple).using(RESPRead.instance(DragonflyP.throttle))
  final def clthrottle(key: Key, maxBurst: NonNegLong, count: PosLong, periodSeconds: PosLong, quantity: NonNegLong): Single[Throttle] =
    Protocol("CL.THROTTLE", key *: maxBurst *: count *: periodSeconds *: quantity *: EmptyTuple)
      .using(RESPRead.instance(DragonflyP.throttle))

  /** Sets the expiry of fields of a hash, or members of a set */
  final def fieldexpire(key: Key, seconds: PosLong, fields: OneOrMoreKeys): Single[Seq[FieldExpireResult]] =
    Protocol("FIELDEXPIRE", key *: seconds *: fields.value *: EmptyTuple).as[Arr.Of[Num], Seq[FieldExpireResult]]
  final def fieldexpire(key: Key, seconds: PosLong, field: Key, fields: Key*): Single[Seq[FieldExpireResult]] =
    fieldexpire(key, seconds, OneOrMoreKeys.unsafeFrom(field :: fields.toList))

  /** The expiry of a field of a hash, or a member of a set */
  final def fieldttl(key: Key, field: Key): Single[FieldTtl] = Protocol("FIELDTTL", key *: field *: EmptyTuple).as[Num, FieldTtl]

  /** Puts the value before the one at the key, and replies with the new length */
  final def prepend[A: Show](key: Key, value: A): Single[NonNegLong] =
    Protocol("PREPEND", key *: value *: EmptyTuple).as[Num, NonNegLong]

  final val role: Single[Role] = Protocol("ROLE", EmptyTuple).using(RESPRead.instance(Role.roleRead))

  /** Takes over from the primary this server replicates, waiting for it up to `timeoutSeconds`. It isn't in Dragonfly's command reference
    */
  final def repltakeover(timeoutSeconds: PosInt): Single[OK] = Protocol("REPLTAKEOVER", timeoutSeconds).as[Str, OK]

  /** Deletes keys while going through them like SCAN, from cursor 0 until it replies 0 */
  final def rm(cursor: NonNegLong): Single[Rm]                     = Protocol("RM", cursor).using(RESPRead.instance(DragonflyP.rm))
  final def rm(cursor: NonNegLong, options: RmOptions): Single[Rm] =
    Protocol("RM", cursor *: options.params *: EmptyTuple).using(RESPRead.instance(DragonflyP.rm))

  /** Adds members to a set that each expire after `seconds`, and replies how many were added */
  final def saddex[A: Show](key: Key, seconds: PosLong, members: OneOrMore[A]): Single[NonNegLong] =
    Protocol("SADDEX", key *: seconds *: members.value *: EmptyTuple).as[Num, NonNegLong]
  final def saddex[A: Show](key: Key, seconds: PosLong, member: A, members: A*): Single[NonNegLong] =
    saddex(key, seconds, OneOrMore.unsafeFrom(member :: members.toList))
  final def saddex[A: Show](key: Key, keepTtl: KeepTtl, seconds: PosLong, members: OneOrMore[A]): Single[NonNegLong] =
    Protocol("SADDEX", key *: keepTtl *: seconds *: members.value *: EmptyTuple).as[Num, NonNegLong]
  final def saddex[A: Show](key: Key, keepTtl: KeepTtl, seconds: PosLong, member: A, members: A*): Single[NonNegLong] =
    saddex(key, keepTtl, seconds, OneOrMore.unsafeFrom(member :: members.toList))

  /** Makes the keys never get evicted when Dragonfly runs as a cache, and replies how many it changed */
  final def stick(keys: OneOrMoreKeys): Single[NonNegLong]  = Protocol("STICK", keys.value).as[Num, NonNegLong]
  final def stick(key: Key, keys: Key*): Single[NonNegLong] =
    stick(OneOrMoreKeys.unsafeFrom(key :: keys.toList))
}

/** CLIENT KILL as Dragonfly has it: one filter, with no others after it */
trait ClientDragonflyKillCommands extends Features {
  import Show.{hostShow, portShow}

  /** Closes the client with this id, and replies how many it closed */
  def kill(id: NonNegLong): Single[NonNegLong] = Protocol("CLIENT", "KILL" *: "ID" *: id *: EmptyTuple).as[Num, NonNegLong]

  /** Closes the client connected from this address, and replies how many it closed */
  def killaddress(host: Host, port: Port): Single[NonNegLong] =
    Protocol("CLIENT", "KILL" :: "ADDR" :: s"${hostShow.show(host)}:${portShow.show(port)}" :: Nil).as[Num, NonNegLong]

  /** Closes the clients connected to this address of the server, and replies how many it closed */
  def killlocaladdress(host: Host, port: Port): Single[NonNegLong] =
    Protocol("CLIENT", "KILL" :: "LADDR" :: s"${hostShow.show(host)}:${portShow.show(port)}" :: Nil).as[Num, NonNegLong]
}
