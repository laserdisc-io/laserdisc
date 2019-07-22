package laserdisc
package protocol

object ClusterP {
  import scala.language.dynamics

  private[protocol] final val KVPairRegex = "(.*):(.*)".r

  final class ClusterInfo(private val properties: Map[String, String]) extends AnyVal with Dynamic {
    def selectDynamic[A](field: String)(implicit R: String ==> A): Maybe[A] =
      properties.get(field).flatMap(R.read).toRight(Err(s"no key $field of the provided type found"))
  }
  final object ClusterInfo {
    implicit final val clusterInfoRead: Bulk ==> ClusterInfo = Read.instancePF {
      case Bulk(s) => new ClusterInfo(s.split(CRLF).collect { case KVPairRegex(k, v) => k -> v }.toMap)
    }
  }

  final case class ClusterNode(
      nodeId: NodeId,
      nodeAddress: NodeAddress,
      flags: Seq[Flag],
      maybeMaster: Option[NodeId],
      pingSent: NonNegInt,
      pongReceived: NonNegInt,
      configEpoch: NonNegInt,
      linkState: LinkState,
      slots: Seq[SlotType]
  )
  final case class ClusterNodes(clusterNodes: Seq[ClusterNode]) extends AnyVal
  final object ClusterNodes {
    private final val NA: String ==> NodeAddress = {
      val HPCP = raw"([A-Za-z0-9\-\.]*):(\d+)@(\d+)".r
      val HP   = raw"([A-Za-z0-9\-\.]*):(\d+)".r
      Read.instancePF {
        case HPCP("", ToInt(Port(p)), ToInt(Port(cp)))      => NodeAddress(LoopbackHost, p, cp)
        case HPCP(Host(h), ToInt(Port(p)), ToInt(Port(cp))) => NodeAddress(h, p, cp)
        case HP("", ToInt(Port(p)))                         => NodeAddress(LoopbackHost, p, p)
        case HP(Host(h), ToInt(Port(p)))                    => NodeAddress(h, p, p)
      }
    }
    private final val Fs: String ==> Seq[Flag] = Read.instancePF {
      case "noflags" => Seq.empty
      case s =>
        s.split(COMMA_CH).toList.collect {
          case "myself"    => Flag.myself
          case "master"    => Flag.master
          case "slave"     => Flag.replica
          case "fail?"     => Flag.possiblefail
          case "fail"      => Flag.fail
          case "handshake" => Flag.handshake
          case "noaddr"    => Flag.noaddress
        }
    }
    private final val OptNodeId: String ==> Option[NodeId] = Read.instancePF {
      case "-"            => None
      case NodeId(nodeId) => Some(nodeId)
    }
    private final val LS: String ==> LinkState = Read.instancePF {
      case "connected"    => LinkState.connected
      case "disconnected" => LinkState.disconnected
    }
    private final val STs: Seq[String] ==> Seq[SlotType] = {
      val R  = raw"(\d+)-(\d+)".r
      val IS = raw"\[(\d+)-<-(${NodeIdRegexWit.value})\]".r
      val MS = raw"\[(\d+)->-(${NodeIdRegexWit.value})\]".r
      Read.instancePF {
        case ss =>
          ss.collect {
            case ToInt(Slot(s))                    => SlotType.Single(s)
            case R(ToInt(Slot(f)), ToInt(Slot(t))) => SlotType.Range(f, t)
            case IS(ToInt(Slot(s)), NodeId(fid))   => SlotType.ImportingSlot(s, fid)
            case MS(ToInt(Slot(s)), NodeId(tid))   => SlotType.MigratingSlot(s, tid)
          }.toList
      }
    }

    implicit final val clusterNodesRead: Bulk ==> ClusterNodes = Read.instancePF {
      case Bulk(s) =>
        ClusterNodes(
          s.split(LF_CH)
            .flatMap {
              _.split(SPACE_CH).toSeq match {
                case NodeId(nid) +: NA(na) +: Fs(fs) +: OptNodeId(mmid) +: ToInt(NonNegInt(ps)) +:
                      ToInt(NonNegInt(pr)) +: ToInt(NonNegInt(ce)) +: LS(ls) +: STs(ss) =>
                  Some(ClusterNode(nid, na, fs, mmid, ps, pr, ce, ls, ss))
                case _ => None
              }
            }
            .toList
        )
    }
  }

  sealed trait FailoverMode
  final object FailoverMode {
    final case object force    extends FailoverMode
    final case object takeover extends FailoverMode

    implicit final val failoverModeShow: Show[FailoverMode] = Show.instance {
      case `force`    => "FORCE"
      case `takeover` => "TAKEOVER"
    }
  }

  sealed trait Flag
  final object Flag {
    final case object myself       extends Flag
    final case object master       extends Flag
    final case object replica      extends Flag
    final case object possiblefail extends Flag
    final case object fail         extends Flag
    final case object handshake    extends Flag
    final case object noaddress    extends Flag

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
  final object LinkState {
    final case object connected    extends LinkState
    final case object disconnected extends LinkState

    implicit final val linkStateShow: Show[LinkState] = Show.unsafeFromToString
  }

  final case class NodeAddress(host: Host, port: Port, clusterPort: Port)

  sealed trait ResetMode
  final object ResetMode {
    final case object hard extends ResetMode
    final case object soft extends ResetMode

    implicit final val resetModeShow: Show[ResetMode] = Show.instance {
      case `hard` => "HARD"
      case `soft` => "SOFT"
    }
  }

  sealed trait SlotInfo
  final object SlotInfo {
    final case class NewSlotInfo(master: HostPortNodeId, replicas: Seq[HostPortNodeId]) extends SlotInfo
    final case class OldSlotInfo(master: HostPort, replicas: Seq[HostPort])             extends SlotInfo
  }

  sealed trait SlotType
  final object SlotType {
    final case class Single(slot: Slot)                            extends SlotType
    final case class Range(from: Slot, to: Slot)                   extends SlotType
    final case class ImportingSlot(slot: Slot, fromNodeId: NodeId) extends SlotType
    final case class MigratingSlot(slot: Slot, toNodeId: NodeId)   extends SlotType
  }

  final case class Slots(slots: Map[SlotType.Range, SlotInfo]) extends AnyVal {
    final def slotInfoFor(slot: Slot): Option[SlotInfo] =
      slots
        .find {
          case (SlotType.Range(from, to), _) => slot.value >= from.value && slot.value <= to.value
        }
        .map { case (_, value) => value }
  }
  final object Slots {
    import SlotInfo._
    import SlotType.Range
    private val H: Bulk ==> Host = Read.instancePF {
      case Bulk("")         => LoopbackHost
      case Bulk(Host(host)) => host
    }
    private val NSI: Seq[RESP] ==> NewSlotInfo = {
      val HPNIs: Seq[RESP] ==> Seq[HostPortNodeId] = Read.instancePF {
        case arr =>
          arr.collect {
            case Arr(H(host) +: Num(ToInt(Port(port))) +: Bulk(NodeId(nodeId)) +: Seq()) => HostPortNodeId(host, port, nodeId)
          }.toList
      }
      Read.instancePF {
        case Arr(H(masterHost) +: Num(ToInt(Port(masterPort))) +: Bulk(NodeId(masterNodeId)) +: Seq()) +: HPNIs(replicas) =>
          NewSlotInfo(HostPortNodeId(masterHost, masterPort, masterNodeId), replicas)
      }
    }
    private val OSI: Seq[RESP] ==> OldSlotInfo = {
      val HPs: Seq[RESP] ==> Seq[HostPort] = Read.instancePF {
        case arr => arr.collect { case Arr(H(host) +: Num(ToInt(Port(port))) +: Seq()) => HostPort(host, port) }.toList
      }
      Read.instancePF {
        case Arr(H(masterHost) +: Num(ToInt(Port(masterPort))) +: Seq()) +: HPs(replicas) =>
          OldSlotInfo(HostPort(masterHost, masterPort), replicas)
      }
    }

    implicit final val slotsRead: Arr ==> Slots = Read.instancePF {
      case arr =>
        Slots(arr.elements.collect {
          case Arr(Num(ToInt(Slot(from))) +: Num(ToInt(Slot(to))) +: OSI(oldSlotInfo)) => Range(from, to) -> oldSlotInfo
          case Arr(Num(ToInt(Slot(from))) +: Num(ToInt(Slot(to))) +: NSI(newSlotInfo)) => Range(from, to) -> newSlotInfo
        }.toMap)
    }
  }
}

trait ClusterP {
  import shapeless._

  final object clusters {
    final type Info         = ClusterP.ClusterInfo
    final type FailoverMode = ClusterP.FailoverMode
    final type Nodes        = ClusterP.ClusterNodes
    final type ResetMode    = ClusterP.ResetMode
    final type Slots        = ClusterP.Slots

    final val failoverMode = ClusterP.FailoverMode
    final val Nodes        = ClusterP.ClusterNodes
    final val resetMode    = ClusterP.ResetMode
  }

  import clusters._

  final def addslots(slots: OneOrMore[Slot]): Protocol.Aux[OK] = Protocol("CLUSTER", "ADDSLOTS" :: slots.value :: HNil).as[Str, OK]

  final val clusterinfo: Protocol.Aux[Info] = Protocol("CLUSTER", "INFO").as[Bulk, Info]

  final def countfailurereports(nodeId: NodeId): Protocol.Aux[NonNegInt] =
    Protocol("CLUSTER", "COUNT-FAILURE-REPORTS" :: nodeId :: HNil).as[Num, NonNegInt]

  final def countkeysinslot(slot: Slot): Protocol.Aux[NonNegInt] = Protocol("CLUSTER", "COUNTKEYSINSLOT" :: slot :: HNil).as[Num, NonNegInt]

  final def delslots(slots: OneOrMore[Slot]): Protocol.Aux[OK] = Protocol("CLUSTER", "DELSLOTS" :: slots.value :: HNil).as[Str, OK]

  final val failover: Protocol.Aux[OK]                     = Protocol("CLUSTER", "FAILOVER").as[Str, OK]
  final def failover(mode: FailoverMode): Protocol.Aux[OK] = Protocol("CLUSTER", "FAILOVER" :: mode :: HNil).as[Str, OK]

  final def forget(nodeId: NodeId): Protocol.Aux[OK] = Protocol("CLUSTER", "FORGET" :: nodeId :: HNil).as[Str, OK]

  final def getkeysinslot(slot: Slot, count: PosInt): Protocol.Aux[Seq[Key]] =
    Protocol("CLUSTER", "GETKEYSINSLOT" :: slot :: count :: HNil).as[Arr, Seq[Key]]

  final def keyslot(key: Key): Protocol.Aux[Slot] = Protocol("CLUSTER", "KEYSLOT" :: key :: HNil).as[Num, Slot]

  final def meet(host: Host, port: Port): Protocol.Aux[OK] = Protocol("CLUSTER", "MEET" :: host :: port :: HNil).as[Str, OK]

  final val nodes: Protocol.Aux[Nodes] = Protocol("CLUSTER", "NODES").as[Bulk, Nodes]

  final val readonly: Protocol.Aux[OK] = Protocol("CLUSTER", "READONLY").as[Str, OK]

  final val readwrite: Protocol.Aux[OK] = Protocol("CLUSTER", "READWRITE").as[Str, OK]

  final def replicas(nodeId: NodeId): Protocol.Aux[Nodes] = Protocol("CLUSTER", "REPLICAS" :: nodeId :: HNil).as[Bulk, Nodes]

  final def replicate(nodeId: NodeId): Protocol.Aux[OK] = Protocol("CLUSTER", "REPLICATE" :: nodeId :: HNil).as[Str, OK]

  final def reset(mode: ResetMode): Protocol.Aux[OK] = Protocol("CLUSTER", "RESET" :: mode :: HNil).as[Str, OK]
  final val reset: Protocol.Aux[OK]                  = reset(resetMode.soft)

  final val saveconfig: Protocol.Aux[OK] = Protocol("CLUSTER", "SAVECONFIG").as[Str, OK]

  final def setconfigepoch(configEpoch: NonNegInt): Protocol.Aux[OK] =
    Protocol("CLUSTER", "SET-CONFIG-EPOCH" :: configEpoch :: HNil).as[Str, OK]

  final def setslotimporting(slot: Slot, from: NodeId): Protocol.Aux[OK] =
    Protocol("CLUSTER", "SETSLOT" :: slot :: "IMPORTING" :: from :: HNil).as[Str, OK]
  final def setslotmigrating(slot: Slot, to: NodeId): Protocol.Aux[OK] =
    Protocol("CLUSTER", "SETSLOT" :: slot :: "MIGRATING" :: to :: HNil).as[Str, OK]
  final def setslotnode(slot: Slot, node: NodeId): Protocol.Aux[OK] =
    Protocol("CLUSTER", "SETSLOT" :: slot :: "NODE" :: node :: HNil).as[Str, OK]
  final def setslotstable(slot: Slot): Protocol.Aux[OK] = Protocol("CLUSTER", "SETSLOT" :: slot :: "STABLE" :: HNil).as[Str, OK]

  final def slaves(nodeId: NodeId): Protocol.Aux[Nodes] = Protocol("CLUSTER", "SLAVES" :: nodeId :: HNil).as[Bulk, Nodes]

  final val slots: Protocol.Aux[Slots] = Protocol("CLUSTER", "SLOTS").as[Arr, Slots]
}
