package laserdisc
package protocol

object ClusterP {
  import scala.language.dynamics

  private[this] final val loopbackHost = Host("127.0.0.1")

  final class ClusterInfo(private val properties: Map[String, String]) extends AnyVal with Dynamic {
    import RESP.err

    def selectDynamic[A](field: String)(implicit R: String ==> A): Maybe[A] =
      properties.get(field).flatMap(R.read).toRight(err(s"no key $field of the provided type found"))
  }
  final object ClusterInfo {
    private final val KVPair = "(.*):(.*)".r

    implicit final val infoRead: Bulk ==> ClusterInfo = Read.instancePF {
      case Bulk(s) => new ClusterInfo(s.split("\r\n").collect { case KVPair(k, v) => k -> v }.toMap)
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
      val R0 = raw"(\w*):(\d+)@(\d+)".r
      val R1 = raw"(\w*):(\d+)".r
      Read.instancePF {
        case R0("", ToInt(Port(port)), ToInt(Port(clusterPort)))         => NodeAddress(loopbackHost, port, clusterPort)
        case R0(Host(host), ToInt(Port(port)), ToInt(Port(clusterPort))) => NodeAddress(host, port, clusterPort)
        case R1("", ToInt(Port(port)))                                   => NodeAddress(loopbackHost, port, port)
        case R1(Host(host), ToInt(Port(port)))                           => NodeAddress(host, port, port)
      }
    }
    private final val Fs: String ==> Seq[Flag] = Read.instancePF {
      case "noflags" => Seq.empty
      case s =>
        s.split(",").toList.collect {
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
      val R0 = raw"(\d+)-(\d+)".r
      val R1 = raw"\[(\d+)-<-([0-9a-f]{40})\]".r
      val R2 = raw"\[(\d+)->-([0-9a-f]{40})\]".r
      Read.instancePF {
        case ss =>
          ss.collect {
            case ToInt(Slot(slot))                         => SlotType.Single(slot)
            case R0(ToInt(Slot(from)), ToInt(Slot(to)))    => SlotType.Range(from, to)
            case R1(ToInt(Slot(slot)), NodeId(fromNodeId)) => SlotType.ImportingSlot(slot, fromNodeId)
            case R2(ToInt(Slot(slot)), NodeId(toNodeId))   => SlotType.MigratingSlot(slot, toNodeId)
          }.toList
      }
    }

    implicit final val clusterNodesRead: Bulk ==> ClusterNodes = Read.instancePF {
      case Bulk(s) =>
        ClusterNodes(
          s.split("\n")
            .flatMap {
              _.split(" ").toSeq match {
                case NodeId(nodeId) +: NA(nodeAddress) +: Fs(flags) +: OptNodeId(maybeMaster) +: ToInt(NonNegInt(pingSent)) +:
                      ToInt(NonNegInt(pongReceived)) +: ToInt(NonNegInt(configEpoch)) +: LS(linkState) +: STs(slots) =>
                  Some(ClusterNode(nodeId, nodeAddress, flags, maybeMaster, pingSent, pongReceived, configEpoch, linkState, slots))
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
  }

  final case class HostPort(host: Host, port: Port)
  final case class HostPortNodeId(host: Host, port: Port, nodeId: NodeId)

  sealed trait LinkState
  final object LinkState {
    final case object connected    extends LinkState
    final case object disconnected extends LinkState
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
      case Bulk("")         => loopbackHost
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
