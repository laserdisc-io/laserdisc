package laserdisc
package protocol

object ClusterP {
  import scala.language.dynamics

  final class Info(private val properties: Map[String, String]) extends AnyVal with Dynamic {
    def selectDynamic[A](field: String)(implicit R: String ==> A): Maybe[A] =
      properties
        .get(field)
        .toRight(RESPDecErr(s"no key $field of the expected type found"))
        .flatMap(R.read)
        .widenLeft[Throwable]
  }
  final object Info {
    implicit val infoRead: Bulk ==> Info =
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
  final object Nodes {
    private final val A: String ==> Address = {
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
    private final val Fs: String ==> Seq[Flag] = Read.instance {
      case "noflags" => Right(Seq.empty)
      case s =>
        s.split(COMMA_CH).foldRight[RESPDecErr | List[Flag]](Right(Nil)) {
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
    private final val MM: String ==> Option[NodeId] = Read.instance {
      case "-"        => Right(None)
      case NodeId(id) => Right(Some(id))
      case other      => Left(RESPDecErr(s"Wrong encoding for Node Id. Was $other"))
    }
    private final val L: String ==> LinkState = Read.instance {
      case "connected"    => Right(LinkState.connected)
      case "disconnected" => Right(LinkState.disconnected)
      case other          => Left(RESPDecErr(s"Wrong encoding for link state. Was $other"))
    }
    private final val Ss: Seq[String] ==> Seq[SlotType] = {
      val R  = raw"(\d+)-(\d+)".r
      val IS = raw"\[(\d+)-<-(${NodeIdRegexWit.value})\]".r
      val MS = raw"\[(\d+)->-(${NodeIdRegexWit.value})\]".r
      Read.instance {
        case ss =>
          ss.foldRight[RESPDecErr | List[SlotType]](Right(Nil)) {
            case (ToInt(Slot(s)), Right(fgs))                    => Right(SlotType.Single(s) :: fgs)
            case (R(ToInt(Slot(f)), ToInt(Slot(t))), Right(fgs)) => Right(SlotType.Range(f, t) :: fgs)
            case (IS(ToInt(Slot(s)), NodeId(id)), Right(fgs))    => Right(SlotType.ImportingSlot(s, id) :: fgs)
            case (MS(ToInt(Slot(s)), NodeId(id)), Right(fgs))    => Right(SlotType.MigratingSlot(s, id) :: fgs)
            case (other, Right(_))                               => Left(RESPDecErr(s"Wrong encoding for Node's slot type. Was $other"))
            case (_, left)                                       => left
          }
      }
    }
    private final val ND: String ==> Node = {
      val errorS = "String ==> Node, Error decoding a cluster Node. Error was: "
      _.split(SPACE_CH).toList match {
        case NodeId(id) :: A(Right(a)) :: Fs(Right(fs)) :: MM(Right(mm)) :: ToInt(NonNegInt(ps)) :: ToInt(NonNegInt(pr)) ::
              ToInt(NonNegInt(ce)) :: L(Right(l)) :: Ss(Right(ss)) =>
          Right(Node(id, a, fs, mm, ps, pr, ce, l, ss))
        case _ :: A(Left(e)) :: _ :: _ :: _ :: _ :: _ :: _ :: _  => Left(RESPDecErr(s"$errorS $e"))
        case _ :: _ :: Fs(Left(e)) :: _ :: _ :: _ :: _ :: _ :: _ => Left(RESPDecErr(s"$errorS $e"))
        case _ :: _ :: _ :: MM(Left(e)) :: _ :: _ :: _ :: _ :: _ => Left(RESPDecErr(s"$errorS $e"))
        case _ :: _ :: _ :: _ :: _ :: _ :: _ :: L(Left(e)) :: _  => Left(RESPDecErr(s"$errorS $e"))
        case _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: Ss(Left(e)) => Left(RESPDecErr(s"$errorS $e"))
        case other =>
          Left(
            RESPDecErr(
              s"Unexpected encoding for a cluster node. Expected [node id, address, [flags], master id, pings, pongs, epoch, link status, slots] but was $other"
            )
          )
      }
    }

    implicit final val nodesRead: Bulk ==> Nodes = Read.instance {
      case Bulk(s) =>
        s.split(LF_CH)
          .foldRight[RESPDecErr | (List[Node], Int)](Right(Nil -> 0)) {
            case (ND(Right(node)), Right((ns, nsl))) => Right((node :: ns) -> (nsl + 1))
            case (ND(Left(e)), Right((_, nsl))) =>
              Left(RESPDecErr(s"Bulk ==> Nodes, Error decoding the cluster's node ${nsl + 1}. Error was: $e"))
            case (_, left) => left
          }
          .map(n => Nodes(n._1))
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
    private val H: Bulk ==> Host = Read.instance {
      case Bulk("")      => Right(LoopbackHost)
      case Bulk(Host(h)) => Right(h)
      case Bulk(other)   => Left(RESPDecErr(s"Wrong host encoding. Was $other"))
    }

    private val SI: Seq[RESP] ==> SlotInfo = {
      val HPNIs: Seq[RESP] ==> Seq[HostPortNodeId] = Read.instance {
        _.foldRight[RESPDecErr | Seq[HostPortNodeId]](Right(Seq.empty)) {
          case (Arr(H(Right(h)) :: Num(ToInt(Port(p))) :: Bulk(NodeId(id)) :: Nil), Right(hsts)) =>
            Right(HostPortNodeId(h, p, id) +: hsts)
          case (Arr(H(Left(e)) :: _), Right(_)) => Left(RESPDecErr(s"Unexpected new replica's host encoding with error ${e.message}"))
          case (Arr(other), Right(_))           => Left(RESPDecErr(s"Unexpected new replica encoding. Expected [host, port, node id] but was $other"))
          case (_, left)                        => left
        }
      }
      val HPs: Seq[RESP] ==> Seq[HostPort] = Read.instance {
        _.foldRight[RESPDecErr | Seq[HostPort]](Right(Seq.empty)) {
          case (Arr(H(Right(h)) :: Num(ToInt(Port(p))) :: Nil), Right(hsts)) =>
            Right(HostPort(h, p) +: hsts)
          case (Arr(H(Left(e)) :: _), Right(_)) => Left(RESPDecErr(s"Unexpected old replica's host encoding with error ${e.message}"))
          case (Arr(other), Right(_))           => Left(RESPDecErr(s"Unexpected old replica encoding. Expected [host, port] but was $other"))
          case (_, left)                        => left
        }
      }

      Read.instance {
        case Arr(H(Right(h)) :: Num(ToInt(Port(p))) :: Nil) +: HPs(Right(rs)) =>
          Right(OldSlotInfo(HostPort(h, p), rs))
        case Arr(H(Right(h)) :: Num(ToInt(Port(p))) :: Bulk(NodeId(id)) :: Nil) +: HPNIs(Right(rs)) =>
          Right(NewSlotInfo(HostPortNodeId(h, p, id), rs))
        case Arr(H(Left(e)) :: _) +: _ => Left(RESPDecErr(s"Unexpected host encoding in the slot info. ${e.message}"))
        case Arr(_ :: otherPort) +: _  => Left(RESPDecErr(s"Unexpected port encoding in the slot info. Was $otherPort"))
        case other =>
          Left(
            RESPDecErr(
              s"Unexpected slot info encoding. Expected [[host, port, node id], replicas]] or [[host, port], replicas] but was $other"
            )
          )
      }
    }

    implicit final val slotsRead: Arr ==> Slots = Read.instance {
      case Arr(arrays) =>
        arrays.foldRight[RESPDecErr | (Map[SlotType.Range, SlotInfo], Int)](Right(Map.empty -> 0)) {
          case (Arr(Num(ToInt(Slot(from))) :: Num(ToInt(Slot(to))) :: Arr(SI(Right(si))) :: Nil), Right((sts, stsl))) =>
            Right((sts + (Range(from, to) -> si)) -> (stsl + 1))
          case (Arr(Num(ToInt(Slot(_))) :: Num(ToInt(Slot(_))) :: Arr(Nil) :: Nil), Right((_, stsl))) =>
            Left(RESPDecErr(s"Unexpected slot assignment encoding at element ${stsl + 1}. The assignment list was empty"))
          case (Arr(other), Right((_, stsl))) =>
            Left(
              RESPDecErr(
                s"Arr ==> Slots unexpected slot encoding at element ${stsl + 1}. Expected [from, to, [[host, port], node id, replicas]] or [from, to, [[host, port], replicas]] but was $other"
              )
            )
          case (_, left) => left
        } map (r => Slots(r._1))
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
