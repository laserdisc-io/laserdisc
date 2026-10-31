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

import scala.annotation.nowarn

@nowarn("msg=Unused import")
object ClusterP {
  import scala.language.dynamics

  final class Info(private val properties: Map[String, String]) extends AnyVal with Dynamic {
    def selectDynamic[A](field: String)(implicit R: Read[String, A]): Maybe[A] =
      properties
        .get(field)
        .toRight(RESPDecErr(s"no key $field of the expected type found"))
        .flatMap(R.read)
        .widenLeft[Throwable]
  }
  object Info {
    implicit val infoRead: Read[Bulk, Info] =
      KVPS.contramap[Bulk](_.value.split(CRLF).toList).map(kv => new Info(kv.toMap))
  }

  final case class Node(
      id: NodeId,
      address: Address,
      flags: Seq[Flag],
      maybeMaster: Option[NodeId],
      ping: NonNegInt,
      pong: NonNegInt,
      configEpoch: NonNegInt,
      link: LinkState,
      slots: Seq[SlotType]
  )
  final case class Nodes(nodes: Seq[Node]) extends AnyVal
  object Nodes {
    private final val A: Read[String, Address] = {
      val HPCP = raw"([A-Za-z0-9\-\.]*):(\d+)@(\d+)".r
      val HP   = raw"([A-Za-z0-9\-\.]*):(\d+)".r
      Read.instance {
        case HPCP("", ToInt(Port(p)), ToInt(Port(cp)))      => Right(Address(LoopbackHost, p, cp))
        case HPCP(Host(h), ToInt(Port(p)), ToInt(Port(cp))) => Right(Address(h, p, cp))
        case HP("", ToInt(Port(p)))                         => Right(Address(LoopbackHost, p, p))
        case HP(Host(h), ToInt(Port(p)))                    => Right(Address(h, p, p))
        case other                                          => Left(RESPDecErr(s"Unexpected encoding for Address. Was $other"))
      }
    }
    private final val Fs: Read[String, Seq[Flag]] = Read.instance {
      case "noflags" => Right(Seq.empty)
      case s         =>
        s.split(COMMA_CH).foldRight[Either[RESPDecErr, List[Flag]]](Right(Nil)) {
          case ("myself", Right(fgs))    => Right(Flag.myself :: fgs)
          case ("master", Right(fgs))    => Right(Flag.master :: fgs)
          case ("slave", Right(fgs))     => Right(Flag.replica :: fgs)
          case ("fail?", Right(fgs))     => Right(Flag.possiblefail :: fgs)
          case ("fail", Right(fgs))      => Right(Flag.fail :: fgs)
          case ("handshake", Right(fgs)) => Right(Flag.handshake :: fgs)
          case ("noaddr", Right(fgs))    => Right(Flag.noaddress :: fgs)
          case (other, Right(_))         => Left(RESPDecErr(s"Unexpected flag encoding. Was $other"))
          case (_, left)                 => left
        }
    }
    private final val MM: Read[String, Option[NodeId]] = Read.instance {
      case "-"        => Right(None)
      case NodeId(id) => Right(Some(id))
      case other      => Left(RESPDecErr(s"Wrong encoding for Node Id. Was $other"))
    }
    private final val L: Read[String, LinkState] = Read.instance {
      case "connected"    => Right(LinkState.connected)
      case "disconnected" => Right(LinkState.disconnected)
      case other          => Left(RESPDecErr(s"Wrong encoding for link state. Was $other"))
    }
    private final val Ss: Read[Seq[String], Seq[SlotType]] = {
      val R  = raw"(\d+)-(\d+)".r
      val IS = raw"\[(\d+)-<-($NodeIdRegex)\]".r
      val MS = raw"\[(\d+)->-($NodeIdRegex)\]".r
      Read.instance { case ss =>
        ss.foldRight[Either[RESPDecErr, List[SlotType]]](Right(Nil)) {
          case (ToInt(Slot(s)), Right(fgs))                    => Right(SlotType.Single(s) :: fgs)
          case (R(ToInt(Slot(f)), ToInt(Slot(t))), Right(fgs)) => Right(SlotType.Range(f, t) :: fgs)
          case (IS(ToInt(Slot(s)), NodeId(id)), Right(fgs))    => Right(SlotType.ImportingSlot(s, id) :: fgs)
          case (MS(ToInt(Slot(s)), NodeId(id)), Right(fgs))    => Right(SlotType.MigratingSlot(s, id) :: fgs)
          case (other, Right(_))                               => Left(RESPDecErr(s"Wrong encoding for Node's slot type. Was $other"))
          case (_, left)                                       => left
        }
      }
    }
    private final val ND: Read[String, Node] = {
      val errorS = "Read[String, Node], Error decoding a cluster Node. Error was: "
      _.split(SPACE_CH).toList match {
        case NodeId(id) :: A(Right(a)) :: Fs(Right(fs)) :: MM(Right(mm)) :: ToInt(NonNegInt(ps)) :: ToInt(NonNegInt(pr)) ::
            ToInt(NonNegInt(ce)) :: L(Right(l)) :: Ss(Right(ss)) =>
          Right(Node(id, a, fs, mm, ps, pr, ce, l, ss))
        case _ :: A(Left(e)) :: _ :: _ :: _ :: _ :: _ :: _ :: _  => Left(RESPDecErr(s"$errorS $e"))
        case _ :: _ :: Fs(Left(e)) :: _ :: _ :: _ :: _ :: _ :: _ => Left(RESPDecErr(s"$errorS $e"))
        case _ :: _ :: _ :: MM(Left(e)) :: _ :: _ :: _ :: _ :: _ => Left(RESPDecErr(s"$errorS $e"))
        case _ :: _ :: _ :: _ :: _ :: _ :: _ :: L(Left(e)) :: _  => Left(RESPDecErr(s"$errorS $e"))
        case _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: Ss(Left(e)) => Left(RESPDecErr(s"$errorS $e"))
        case other                                               =>
          Left(
            RESPDecErr(
              s"Unexpected encoding for a cluster node. Expected [node id, address, [flags], master id, pings, pongs, epoch, link status, slots] but was $other"
            )
          )
      }
    }

    implicit final val nodesRead: Read[Bulk, Nodes] = Read.instance { case Bulk(s) =>
      s.split(LF_CH)
        .foldRight[Either[RESPDecErr, (List[Node], Int)]](Right(Nil -> 0)) {
          case (ND(Right(node)), Right((ns, nsl))) => Right((node :: ns) -> (nsl + 1))
          case (ND(Left(e)), Right((_, nsl)))      =>
            Left(RESPDecErr(s"Read[Bulk, Nodes], Error decoding the cluster's node ${nsl + 1}. Error was: $e"))
          case (_, left) => left
        }
        .map(n => Nodes(n._1))
    }
  }

  sealed trait FailoverMode
  object FailoverMode {
    case object force    extends FailoverMode
    case object takeover extends FailoverMode

    implicit final val failoverModeShow: Show[FailoverMode] = Show.instance {
      case `force`    => "FORCE"
      case `takeover` => "TAKEOVER"
    }
  }

  sealed trait Flag
  object Flag {
    case object myself       extends Flag
    case object master       extends Flag
    case object replica      extends Flag
    case object possiblefail extends Flag
    case object fail         extends Flag
    case object handshake    extends Flag
    case object noaddress    extends Flag

    implicit final val flagShow: Show[Flag] = Show.instance {
      case `myself`       => "myself"
      case `master`       => "master"
      case `replica`      => "slave"
      case `possiblefail` => "fail?"
      case `fail`         => "fail"
      case `handshake`    => "handshake"
      case `noaddress`    => "noaddr"
    }
  }

  final case class HostPort(host: Host, port: Port)
  final case class HostPortNodeId(host: Host, port: Port, nodeId: NodeId)

  sealed trait LinkState
  object LinkState {
    case object connected    extends LinkState
    case object disconnected extends LinkState

    implicit final val linkStateShow: Show[LinkState] = Show.unsafeFromToString
  }

  final case class Address(host: Host, port: Port, clusterPort: Port)

  sealed trait ResetMode
  object ResetMode {
    case object hard extends ResetMode
    case object soft extends ResetMode

    implicit final val resetModeShow: Show[ResetMode] = Show.instance {
      case `hard` => "HARD"
      case `soft` => "SOFT"
    }
  }

  sealed trait SetSlotMode
  object SetSlotMode {
    case object importing extends SetSlotMode
    case object migrating extends SetSlotMode
    case object node      extends SetSlotMode

    implicit final val setSlotModeShow: Show[SetSlotMode] = Show.instance {
      case `importing` => "IMPORTING"
      case `migrating` => "MIGRATING"
      case `node`      => "NODE"
    }
  }

  sealed trait SlotInfo
  object SlotInfo {
    final case class NewSlotInfo(master: HostPortNodeId, replicas: Seq[HostPortNodeId]) extends SlotInfo
    final case class OldSlotInfo(master: HostPort, replicas: Seq[HostPort])             extends SlotInfo
  }

  sealed trait SlotType
  object SlotType {
    final case class Single(slot: Slot)                            extends SlotType
    final case class Range(from: Slot, to: Slot)                   extends SlotType
    final case class ImportingSlot(slot: Slot, fromNodeId: NodeId) extends SlotType
    final case class MigratingSlot(slot: Slot, toNodeId: NodeId)   extends SlotType
  }

  final case class Slots(slots: Map[SlotType.Range, SlotInfo]) extends AnyVal {
    final def infoFor(slot: Slot): Option[SlotInfo] =
      slots
        .find { case (SlotType.Range(from, to), _) =>
          slot.value >= from.value && slot.value <= to.value
        }
        .map { case (_, value) => value }
  }
  object Slots {
    import SlotInfo.*
    import SlotType.Range
    private val H: Read[Bulk, Host] = Read.instance {
      case Bulk("")      => Right(LoopbackHost)
      case Bulk(Host(h)) => Right(h)
      case Bulk(other)   => Left(RESPDecErr(s"Wrong host encoding. Was $other"))
    }

    private val SI: Read[Seq[RESP], SlotInfo] = {
      val HPNIs: Read[Seq[RESP], Seq[HostPortNodeId]] = Read.instance {
        _.foldRight[Either[RESPDecErr, Seq[HostPortNodeId]]](Right(Seq.empty)) {
          case (Arr(H(Right(h)) :: Num(ToInt(Port(p))) :: Bulk(NodeId(id)) :: Nil), Right(hsts)) =>
            Right(HostPortNodeId(h, p, id) +: hsts)
          case (Arr(H(Left(e)) :: _), Right(_)) => Left(RESPDecErr(s"Unexpected new replica's host encoding with error ${e.message}"))
          case (Arr(other), Right(_)) => Left(RESPDecErr(s"Unexpected new replica encoding. Expected [host, port, node id] but was $other"))
          case (_, left)              => left
        }
      }
      val HPs: Read[Seq[RESP], Seq[HostPort]] = Read.instance {
        _.foldRight[Either[RESPDecErr, Seq[HostPort]]](Right(Seq.empty)) {
          case (Arr(H(Right(h)) :: Num(ToInt(Port(p))) :: Nil), Right(hsts)) =>
            Right(HostPort(h, p) +: hsts)
          case (Arr(H(Left(e)) :: _), Right(_)) => Left(RESPDecErr(s"Unexpected old replica's host encoding with error ${e.message}"))
          case (Arr(other), Right(_)) => Left(RESPDecErr(s"Unexpected old replica encoding. Expected [host, port] but was $other"))
          case (_, left)              => left
        }
      }

      Read.instance {
        case Arr(H(Right(h)) :: Num(ToInt(Port(p))) :: Nil) +: HPs(Right(rs)) =>
          Right(OldSlotInfo(HostPort(h, p), rs))
        case Arr(H(Right(h)) :: Num(ToInt(Port(p))) :: Bulk(NodeId(id)) :: Nil) +: HPNIs(Right(rs)) =>
          Right(NewSlotInfo(HostPortNodeId(h, p, id), rs))
        case Arr(H(Left(e)) :: _) +: _ => Left(RESPDecErr(s"Unexpected host encoding in the slot info. ${e.message}"))
        case Arr(_ :: otherPort) +: _  => Left(RESPDecErr(s"Unexpected port encoding in the slot info. Was $otherPort"))
        case other                     =>
          Left(
            RESPDecErr(
              s"Unexpected slot info encoding. Expected [[host, port, node id], replicas]] or [[host, port], replicas] but was $other"
            )
          )
      }
    }

    implicit final val slotsRead: Read[Arr, Slots] = Read.instance { case Arr(arrays) =>
      arrays
        .foldRight[Either[RESPDecErr, (Map[SlotType.Range, SlotInfo], Int)]](Right(Map.empty -> 0)) {
          case (Arr(Num(ToInt(Slot(from))) :: Num(ToInt(Slot(to))) :: Arr(SI(Right(si))) :: Nil), Right((sts, stsl))) =>
            Right((sts + (Range(from, to) -> si)) -> (stsl + 1))
          case (Arr(Num(ToInt(Slot(_))) :: Num(ToInt(Slot(_))) :: Arr(Nil) :: Nil), Right((_, stsl))) =>
            Left(RESPDecErr(s"Unexpected slot assignment encoding at element ${stsl + 1}. The assignment list was empty"))
          case (Arr(other), Right((_, stsl))) =>
            Left(
              RESPDecErr(
                s"Read[Arr, Slots] unexpected slot encoding at element ${stsl + 1}. Expected [from, to, [[host, port], node id, replicas]] or [from, to, [[host, port], replicas]] but was $other"
              )
            )
          case (_, left) => left
        }
        .map(r => Slots(r._1))
    }
  }
}

trait ClusterP {
  object clustertypes {
    final type ClusterAddress           = ClusterP.Address
    final type ClusterFailoverMode      = ClusterP.FailoverMode
    final type ClusterFlag              = ClusterP.Flag
    final type ClusterHostPort          = ClusterP.HostPort
    final type ClusterHostPortNodeId    = ClusterP.HostPortNodeId
    final type ClusterImportingSlotType = ClusterP.SlotType.ImportingSlot
    final type ClusterInfo              = ClusterP.Info
    final type ClusterLinkState         = ClusterP.LinkState
    final type ClusterMigratingSlotType = ClusterP.SlotType.MigratingSlot
    final type ClusterNewSlotInfo       = ClusterP.SlotInfo.NewSlotInfo
    final type ClusterNode              = ClusterP.Node
    final type ClusterNodes             = ClusterP.Nodes
    final type ClusterOldSlotInfo       = ClusterP.SlotInfo.OldSlotInfo
    final type ClusterRangeSlotType     = ClusterP.SlotType.Range
    final type ClusterResetMode         = ClusterP.ResetMode
    final type ClusterSetSlotMode       = ClusterP.SetSlotMode
    final type ClusterSingleSlotType    = ClusterP.SlotType.Single
    final type ClusterSlots             = ClusterP.Slots

    final val ClusterAddress           = ClusterP.Address
    final val ClusterFailoverMode      = ClusterP.FailoverMode
    final val ClusterFlag              = ClusterP.Flag
    final val ClusterHostPort          = ClusterP.HostPort
    final val ClusterHostPortNodeId    = ClusterP.HostPortNodeId
    final val ClusterImportingSlotType = ClusterP.SlotType.ImportingSlot
    final val ClusterInfo              = ClusterP.Info
    final val ClusterLinkState         = ClusterP.LinkState
    final val ClusterMigratingSlotType = ClusterP.SlotType.MigratingSlot
    final val ClusterNewSlotInfo       = ClusterP.SlotInfo.NewSlotInfo
    final val ClusterNode              = ClusterP.Node
    final val ClusterNodes             = ClusterP.Nodes
    final val ClusterOldSlotInfo       = ClusterP.SlotInfo.OldSlotInfo
    final val ClusterRangeSlotType     = ClusterP.SlotType.Range
    final val ClusterResetMode         = ClusterP.ResetMode
    final val ClusterSetSlotMode       = ClusterP.SetSlotMode
    final val ClusterSingleSlotType    = ClusterP.SlotType.Single
    final val ClusterSlots             = ClusterP.Slots
  }

  import clustertypes.*

  final def addslots(slots: OneOrMore[Slot]): Single.Aux[OK] = Protocol("CLUSTER", "ADDSLOTS" *: slots.value *: EmptyTuple).as[Str, OK]

  final val clusterinfo: Single.Aux[ClusterInfo] = Protocol("CLUSTER", "INFO").as[Bulk, ClusterInfo]

  final def countfailurereports(nodeId: NodeId): Single.Aux[NonNegInt] =
    Protocol("CLUSTER", "COUNT-FAILURE-REPORTS" *: nodeId *: EmptyTuple).as[Num, NonNegInt]

  final def countkeysinslot(slot: Slot): Single.Aux[NonNegInt] =
    Protocol("CLUSTER", "COUNTKEYSINSLOT" *: slot *: EmptyTuple).as[Num, NonNegInt]

  final def delslots(slots: OneOrMore[Slot]): Single.Aux[OK] = Protocol("CLUSTER", "DELSLOTS" *: slots.value *: EmptyTuple).as[Str, OK]

  final val failover: Single.Aux[OK]                            = Protocol("CLUSTER", "FAILOVER").as[Str, OK]
  final def failover(mode: ClusterFailoverMode): Single.Aux[OK] = Protocol("CLUSTER", "FAILOVER" *: mode *: EmptyTuple).as[Str, OK]

  final def forget(nodeId: NodeId): Single.Aux[OK] = Protocol("CLUSTER", "FORGET" *: nodeId *: EmptyTuple).as[Str, OK]

  final def getkeysinslot(slot: Slot, count: PosInt): Single.Aux[Seq[Key]] =
    Protocol("CLUSTER", "GETKEYSINSLOT" *: slot *: count *: EmptyTuple).as[Arr, Seq[Key]]

  final def keyslot(key: Key): Single.Aux[Slot] = Protocol("CLUSTER", "KEYSLOT" *: key *: EmptyTuple).as[Num, Slot]

  final def meet(host: Host, port: Port): Single.Aux[OK] = Protocol("CLUSTER", "MEET" *: host *: port *: EmptyTuple).as[Str, OK]

  final val nodes: Single.Aux[ClusterNodes] = Protocol("CLUSTER", "NODES").as[Bulk, ClusterNodes]

  final val readonly: Single.Aux[OK] = Protocol("CLUSTER", "READONLY").as[Str, OK]

  final val readwrite: Single.Aux[OK] = Protocol("CLUSTER", "READWRITE").as[Str, OK]

  final def replicas(nodeId: NodeId): Single.Aux[ClusterNodes] =
    Protocol("CLUSTER", "REPLICAS" *: nodeId *: EmptyTuple).as[Bulk, ClusterNodes]

  final def replicate(nodeId: NodeId): Single.Aux[OK] = Protocol("CLUSTER", "REPLICATE" *: nodeId *: EmptyTuple).as[Str, OK]

  final def reset(mode: ClusterResetMode): Single.Aux[OK] = Protocol("CLUSTER", "RESET" *: mode *: EmptyTuple).as[Str, OK]
  final val reset: Single.Aux[OK]                         = reset(ClusterResetMode.soft)

  final val saveconfig: Single.Aux[OK] = Protocol("CLUSTER", "SAVECONFIG").as[Str, OK]

  final def setconfigepoch(configEpoch: NonNegInt): Single.Aux[OK] =
    Protocol("CLUSTER", "SET-CONFIG-EPOCH" *: configEpoch *: EmptyTuple).as[Str, OK]

  final def setslot(slot: Slot): Single.Aux[OK] = Protocol("CLUSTER", "SETSLOT" *: slot *: "STABLE" *: EmptyTuple).as[Str, OK]
  final def setslot(slot: Slot, mode: ClusterSetSlotMode, node: NodeId): Single.Aux[OK] =
    Protocol("CLUSTER", "SETSLOT" *: slot *: mode *: node *: EmptyTuple).as[Str, OK]

  final def slaves(nodeId: NodeId): Single.Aux[ClusterNodes] =
    Protocol("CLUSTER", "SLAVES" *: nodeId *: EmptyTuple).as[Bulk, ClusterNodes]

  final val slots: Single.Aux[ClusterSlots] = Protocol("CLUSTER", "SLOTS").as[Arr, ClusterSlots]
}
