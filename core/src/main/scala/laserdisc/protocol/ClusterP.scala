package laserdisc
package protocol

object ClusterP {
  import scala.language.dynamics

  private[protocol] final val KVPairRegex = "(.*):(.*)".r

  final class Info(private val properties: Map[String, String]) extends AnyVal with Dynamic {
    def selectDynamic[A](field: String)(implicit R: String ==> A): Maybe[A] =
      properties.get(field).flatMap(R.read).toRight(Err(s"no key $field of the provided type found"))
  }
  final object Info {
    implicit final val infoRead: Bulk ==> Info = Read.instancePF {
      case Bulk(s) => new Info(s.split(CRLF).collect { case KVPairRegex(k, v) => k -> v }.toMap)
    }
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
  final object Nodes {
    private final val A: String ==> Address = {
      val HPCP = raw"([A-Za-z0-9\-\.]*):(\d+)@(\d+)".r
      val HP   = raw"([A-Za-z0-9\-\.]*):(\d+)".r
      Read.instancePF {
        case HPCP("", ToInt(Port(p)), ToInt(Port(cp)))      => Address(LoopbackHost, p, cp)
        case HPCP(Host(h), ToInt(Port(p)), ToInt(Port(cp))) => Address(h, p, cp)
        case HP("", ToInt(Port(p)))                         => Address(LoopbackHost, p, p)
        case HP(Host(h), ToInt(Port(p)))                    => Address(h, p, p)
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
    private final val MM: String ==> Option[NodeId] = Read.instancePF {
      case "-"        => None
      case NodeId(id) => Some(id)
    }
    private final val L: String ==> LinkState = Read.instancePF {
      case "connected"    => LinkState.connected
      case "disconnected" => LinkState.disconnected
    }
    private final val Ss: Seq[String] ==> Seq[SlotType] = {
      val R  = raw"(\d+)-(\d+)".r
      val IS = raw"\[(\d+)-<-(${NodeIdRegexWit.value})\]".r
      val MS = raw"\[(\d+)->-(${NodeIdRegexWit.value})\]".r
      Read.instancePF {
        case ss =>
          ss.collect {
            case ToInt(Slot(s))                    => SlotType.Single(s)
            case R(ToInt(Slot(f)), ToInt(Slot(t))) => SlotType.Range(f, t)
            case IS(ToInt(Slot(s)), NodeId(id))    => SlotType.ImportingSlot(s, id)
            case MS(ToInt(Slot(s)), NodeId(id))    => SlotType.MigratingSlot(s, id)
          }.toList
      }
    }

    implicit final val nodesRead: Bulk ==> Nodes = Read.instancePF {
      case Bulk(s) =>
        Nodes(
          s.split(LF_CH)
            .flatMap {
              _.split(SPACE_CH).toSeq match {
                case NodeId(id) +: A(a) +: Fs(fs) +: MM(mm) +:
                      ToInt(NonNegInt(ps)) +: ToInt(NonNegInt(pr)) +: ToInt(NonNegInt(ce)) +: L(l) +: Ss(ss) =>
                  Some(Node(id, a, fs, mm, ps, pr, ce, l, ss))
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

  final case class Address(host: Host, port: Port, clusterPort: Port)

  sealed trait ResetMode
  final object ResetMode {
    final case object hard extends ResetMode
    final case object soft extends ResetMode

    implicit final val resetModeShow: Show[ResetMode] = Show.instance {
      case `hard` => "HARD"
      case `soft` => "SOFT"
    }
  }

  sealed trait SetSlotMode
  final object SetSlotMode {
    final case object importing extends SetSlotMode
    final case object migrating extends SetSlotMode
    final case object node      extends SetSlotMode

    implicit final val setSlotModeShow: Show[SetSlotMode] = Show.instance {
      case `importing` => "IMPORTING"
      case `migrating` => "MIGRATING"
      case `node`      => "NODE"
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
    final def infoFor(slot: Slot): Option[SlotInfo] =
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
      case Bulk("")      => LoopbackHost
      case Bulk(Host(h)) => h
    }
    private val NSI: Seq[RESP] ==> NewSlotInfo = {
      val HPNIs: Seq[RESP] ==> Seq[HostPortNodeId] = Read.instancePF {
        case arr => arr.collect { case Arr(H(h) +: Num(ToInt(Port(p))) +: Bulk(NodeId(id)) +: Seq()) => HostPortNodeId(h, p, id) }.toList
      }
      Read.instancePF {
        case Arr(H(h) +: Num(ToInt(Port(p))) +: Bulk(NodeId(id)) +: Seq()) +: HPNIs(rs) => NewSlotInfo(HostPortNodeId(h, p, id), rs)
      }
    }
    private val OSI: Seq[RESP] ==> OldSlotInfo = {
      val HPs: Seq[RESP] ==> Seq[HostPort] = Read.instancePF {
        case arr => arr.collect { case Arr(H(h) +: Num(ToInt(Port(p))) +: Seq()) => HostPort(h, p) }.toList
      }
      Read.instancePF {
        case Arr(H(h) +: Num(ToInt(Port(p))) +: Seq()) +: HPs(rs) => OldSlotInfo(HostPort(h, p), rs)
      }
    }

    implicit final val slotsRead: Arr ==> Slots = Read.instancePF {
      case arr =>
        Slots(arr.elements.collect {
          case Arr(Num(ToInt(Slot(from))) +: Num(ToInt(Slot(to))) +: OSI(osi)) => Range(from, to) -> osi
          case Arr(Num(ToInt(Slot(from))) +: Num(ToInt(Slot(to))) +: NSI(nsi)) => Range(from, to) -> nsi
        }.toMap)
    }
  }
}

trait ClusterP {
  import shapeless._

  final object clustertypes {
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

  import clustertypes._

  final def addslots(slots: OneOrMore[Slot]): Protocol.Aux[OK] = Protocol("CLUSTER", "ADDSLOTS" :: slots.value :: HNil).as[Str, OK]

  final val clusterinfo: Protocol.Aux[ClusterInfo] = Protocol("CLUSTER", "INFO").as[Bulk, ClusterInfo]

  final def countfailurereports(nodeId: NodeId): Protocol.Aux[NonNegInt] =
    Protocol("CLUSTER", "COUNT-FAILURE-REPORTS" :: nodeId :: HNil).as[Num, NonNegInt]

  final def countkeysinslot(slot: Slot): Protocol.Aux[NonNegInt] = Protocol("CLUSTER", "COUNTKEYSINSLOT" :: slot :: HNil).as[Num, NonNegInt]

  final def delslots(slots: OneOrMore[Slot]): Protocol.Aux[OK] = Protocol("CLUSTER", "DELSLOTS" :: slots.value :: HNil).as[Str, OK]

  final val failover: Protocol.Aux[OK]                            = Protocol("CLUSTER", "FAILOVER").as[Str, OK]
  final def failover(mode: ClusterFailoverMode): Protocol.Aux[OK] = Protocol("CLUSTER", "FAILOVER" :: mode :: HNil).as[Str, OK]

  final def forget(nodeId: NodeId): Protocol.Aux[OK] = Protocol("CLUSTER", "FORGET" :: nodeId :: HNil).as[Str, OK]

  final def getkeysinslot(slot: Slot, count: PosInt): Protocol.Aux[Seq[Key]] =
    Protocol("CLUSTER", "GETKEYSINSLOT" :: slot :: count :: HNil).as[Arr, Seq[Key]]

  final def keyslot(key: Key): Protocol.Aux[Slot] = Protocol("CLUSTER", "KEYSLOT" :: key :: HNil).as[Num, Slot]

  final def meet(host: Host, port: Port): Protocol.Aux[OK] = Protocol("CLUSTER", "MEET" :: host :: port :: HNil).as[Str, OK]

  final val nodes: Protocol.Aux[ClusterNodes] = Protocol("CLUSTER", "NODES").as[Bulk, ClusterNodes]

  final val readonly: Protocol.Aux[OK] = Protocol("CLUSTER", "READONLY").as[Str, OK]

  final val readwrite: Protocol.Aux[OK] = Protocol("CLUSTER", "READWRITE").as[Str, OK]

  final def replicas(nodeId: NodeId): Protocol.Aux[ClusterNodes] = Protocol("CLUSTER", "REPLICAS" :: nodeId :: HNil).as[Bulk, ClusterNodes]

  final def replicate(nodeId: NodeId): Protocol.Aux[OK] = Protocol("CLUSTER", "REPLICATE" :: nodeId :: HNil).as[Str, OK]

  final def reset(mode: ClusterResetMode): Protocol.Aux[OK] = Protocol("CLUSTER", "RESET" :: mode :: HNil).as[Str, OK]
  final val reset: Protocol.Aux[OK]                         = reset(ClusterResetMode.soft)

  final val saveconfig: Protocol.Aux[OK] = Protocol("CLUSTER", "SAVECONFIG").as[Str, OK]

  final def setconfigepoch(configEpoch: NonNegInt): Protocol.Aux[OK] =
    Protocol("CLUSTER", "SET-CONFIG-EPOCH" :: configEpoch :: HNil).as[Str, OK]

  final def setslot(slot: Slot): Protocol.Aux[OK] = Protocol("CLUSTER", "SETSLOT" :: slot :: "STABLE" :: HNil).as[Str, OK]
  final def setslot(slot: Slot, mode: ClusterSetSlotMode, node: NodeId): Protocol.Aux[OK] =
    Protocol("CLUSTER", "SETSLOT" :: slot :: mode :: node :: HNil).as[Str, OK]

  final def slaves(nodeId: NodeId): Protocol.Aux[ClusterNodes] = Protocol("CLUSTER", "SLAVES" :: nodeId :: HNil).as[Bulk, ClusterNodes]

  final val slots: Protocol.Aux[ClusterSlots] = Protocol("CLUSTER", "SLOTS").as[Arr, ClusterSlots]
}
