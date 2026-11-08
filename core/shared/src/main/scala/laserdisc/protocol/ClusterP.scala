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

object ClusterP {
  final class Info(private val properties: Map[String, String]) extends AnyVal {
    def get[A](field: String)(implicit R: Read[String, A]): Maybe[A] =
      properties
        .get(field)
        .toRight(RESPDecErr(s"no field $field"))
        .flatMap(R.read)
        .widenLeft[Throwable]
  }
  object Info {
    implicit val infoRead: Read[Bulk, Info] =
      KVPS.contramap[Bulk](_.value.split(CRLF).toList).map(kv => new Info(kv.toMap))
  }

  /** A node of CLUSTER NODES. `address` is `None` when the server doesn't know where the node is. `ping` and `pong` are when the last ping
    * was sent and the last pong received, in milliseconds since the epoch, or 0 if there was none
    */
  sealed trait Node6_2 {
    def id: NodeId
    def address: Option[Address6_2]
    def flags: Seq[Flag]
    def maybeMaster: Option[NodeId]
    def ping: NonNegLong
    def pong: NonNegLong
    def configEpoch: NonNegInt
    def link: LinkState
    def slots: Seq[SlotType]
  }

  /** A node of CLUSTER NODES from Redis 7.0, whose address also has the hostname the node announces */
  sealed trait Node7_0 extends Node6_2 {
    override def address: Option[Address7_0]
  }

  private[laserdisc] final case class NodeOf6_2(
      id: NodeId,
      address: Option[Address6_2],
      flags: Seq[Flag],
      maybeMaster: Option[NodeId],
      ping: NonNegLong,
      pong: NonNegLong,
      configEpoch: NonNegInt,
      link: LinkState,
      slots: Seq[SlotType]
  ) extends Node6_2
  private[laserdisc] final case class NodeOf7_0(
      id: NodeId,
      address: Option[Address7_0],
      flags: Seq[Flag],
      maybeMaster: Option[NodeId],
      ping: NonNegLong,
      pong: NonNegLong,
      configEpoch: NonNegInt,
      link: LinkState,
      slots: Seq[SlotType]
  ) extends Node7_0

  /** The nodes of CLUSTER NODES, CLUSTER REPLICAS and CLUSTER SLAVES */
  sealed trait Nodes6_2 { def nodes: Seq[Node6_2] }

  /** The nodes of CLUSTER NODES, CLUSTER REPLICAS and CLUSTER SLAVES from Redis 7.0 */
  sealed trait Nodes7_0 extends Nodes6_2 { override def nodes: Seq[Node7_0] }

  private[laserdisc] final case class NodesOf6_2(nodes: Seq[Node6_2]) extends Nodes6_2
  private[laserdisc] final case class NodesOf7_0(nodes: Seq[Node7_0]) extends Nodes7_0

  object Nodes {
    // what Redis 6.2 reads of what any version sends
    private[protocol] final def to6_2(n: Node7_0): Node6_2 =
      NodeOf6_2(
        n.id,
        n.address.map(a => AddressOf6_2(a.host, a.port, a.clusterPort)),
        n.flags,
        n.maybeMaster,
        n.ping,
        n.pong,
        n.configEpoch,
        n.link,
        n.slots
      )

    private final val A: Read[String, Option[Address7_0]] = {
      // the hostname, from Redis 7.0, then any other fields, which are left out
      val HPCP = raw"([A-Za-z0-9\-\.]*):(\d+)@(\d+)(?:,|$$)([A-Za-z0-9\-\.]*)(?:,.*)?".r
      val HP   = raw"([A-Za-z0-9\-\.]*):(\d+)".r
      object MaybeHost {
        def unapply(s: String): Option[Option[Host]] = s match {
          case ""      => Some(None)
          case Host(h) => Some(Some(h))
          case _       => None
        }
      }
      Read.instance {
        case ":0@0"                                                            => Right(None)
        case HPCP(MaybeHost(h), ToInt(Port(p)), ToInt(Port(cp)), MaybeHost(n)) => Right(Some(AddressOf7_0(h, p, cp, n)))
        case HP(MaybeHost(h), ToInt(Port(p)))                                  => Right(Some(AddressOf7_0(h, p, p, None)))
        case other => Left(RESPDecErr(s"Unexpected encoding for Address. Was $other"))
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
    private final val ND: Read[String, Node7_0] = {
      val errorS = "Read[String, Node], Error decoding a cluster Node. Error was: "
      _.split(SPACE_CH).toList match {
        case NodeId(id) :: A(Right(a)) :: Fs(Right(fs)) :: MM(Right(mm)) :: ToLong(NonNegLong(ps)) :: ToLong(NonNegLong(pr)) ::
            ToInt(NonNegInt(ce)) :: L(Right(l)) :: Ss(Right(ss)) =>
          Right(NodeOf7_0(id, a, fs, mm, ps, pr, ce, l, ss))
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

    private final def nodesOf(lines: Seq[String]): Either[RESPDecErr, Nodes7_0] =
      lines
        .foldRight[Either[RESPDecErr, (List[Node7_0], Int)]](Right(Nil -> 0)) {
          case (ND(Right(node)), Right((ns, nsl))) => Right((node :: ns) -> (nsl + 1))
          case (ND(Left(e)), Right((_, nsl)))      =>
            Left(RESPDecErr(s"Read[Bulk, Nodes], Error decoding the cluster's node ${nsl + 1}. Error was: $e"))
          case (_, left) => left
        }
        .map(n => NodesOf7_0(n._1))

    // CLUSTER NODES sends one text with a line for each node
    private[protocol] final val nodes7_0: Read[Bulk, Nodes7_0] = Read.instance { case Bulk(s) => nodesOf(s.split(LF_CH).toList) }
    private[protocol] final val nodes6_2: Read[Bulk, Nodes6_2] = nodes7_0.map(n => NodesOf6_2(n.nodes.map(to6_2)))

    // CLUSTER REPLICAS sends an array with a text for each node
    private[protocol] final val nodesOfArr7_0: Read[Arr.Of[Bulk], Nodes7_0] = Read.instance { lines =>
      Read[Arr.Of[Bulk], Seq[String]].read(lines).flatMap(nodesOf)
    }
    private[protocol] final val nodesOfArr6_2: Read[Arr.Of[Bulk], Nodes6_2] = nodesOfArr7_0.map(n => NodesOf6_2(n.nodes.map(to6_2)))
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

  /** A node of CLUSTER SLOTS. `host` is `None` when the server says to use the host you connected to */
  sealed trait SlotNode6_2 {
    def host: Option[Host]
    def port: Port
    def nodeId: NodeId
  }

  /** A node of CLUSTER SLOTS from Redis 7.0, with what else the server knows of the node, like its hostname or IP */
  sealed trait SlotNode7_0 extends SlotNode6_2 {
    def metadata: Map[String, String]
  }

  private[laserdisc] final case class SlotNodeOf6_2(host: Option[Host], port: Port, nodeId: NodeId) extends SlotNode6_2
  private[laserdisc] final case class SlotNodeOf7_0(host: Option[Host], port: Port, nodeId: NodeId, metadata: Map[String, String])
      extends SlotNode7_0

  sealed trait LinkState
  object LinkState {
    case object connected    extends LinkState
    case object disconnected extends LinkState

    implicit final val linkStateShow: Show[LinkState] = Show.instance {
      case `connected`    => "connected"
      case `disconnected` => "disconnected"
    }
  }

  /** Where a node listens. `host` is `None` until the node knows its own address, as on a fresh node */
  sealed trait Address6_2 {
    def host: Option[Host]
    def port: Port
    def clusterPort: Port
  }

  /** Where a node listens, from Redis 7.0: also the hostname the node announces with cluster-announce-hostname, if it does */
  sealed trait Address7_0 extends Address6_2 {
    def hostname: Option[Host]
  }

  private[laserdisc] final case class AddressOf6_2(host: Option[Host], port: Port, clusterPort: Port) extends Address6_2
  private[laserdisc] final case class AddressOf7_0(host: Option[Host], port: Port, clusterPort: Port, hostname: Option[Host])
      extends Address7_0

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

  /** Who serves a range of slots: the master, then the replicas */
  sealed trait SlotInfo6_2 {
    def master: SlotNode6_2
    def replicas: Seq[SlotNode6_2]
  }

  /** Who serves a range of slots, from Redis 7.0, with each node's metadata */
  sealed trait SlotInfo7_0 extends SlotInfo6_2 {
    override def master: SlotNode7_0
    override def replicas: Seq[SlotNode7_0]
  }

  private[laserdisc] final case class SlotInfoOf6_2(master: SlotNode6_2, replicas: Seq[SlotNode6_2]) extends SlotInfo6_2
  private[laserdisc] final case class SlotInfoOf7_0(master: SlotNode7_0, replicas: Seq[SlotNode7_0]) extends SlotInfo7_0

  sealed trait SlotType
  object SlotType {
    final case class Single(slot: Slot)                            extends SlotType
    final case class Range(from: Slot, to: Slot)                   extends SlotType
    final case class ImportingSlot(slot: Slot, fromNodeId: NodeId) extends SlotType
    final case class MigratingSlot(slot: Slot, toNodeId: NodeId)   extends SlotType
  }

  /** CLUSTER SLOTS: who serves each range of slots */
  sealed trait Slots6_2 {
    def slots: Map[SlotType.Range, SlotInfo6_2]
    def infoFor(slot: Slot): Option[SlotInfo6_2] = Slots.find(slots, slot)
  }

  /** CLUSTER SLOTS from Redis 7.0, with each node's metadata */
  sealed trait Slots7_0 extends Slots6_2 {
    override def slots: Map[SlotType.Range, SlotInfo7_0]
    override def infoFor(slot: Slot): Option[SlotInfo7_0] = Slots.find(slots, slot)
  }

  private[laserdisc] final case class SlotsOf6_2(slots: Map[SlotType.Range, SlotInfo6_2]) extends Slots6_2
  private[laserdisc] final case class SlotsOf7_0(slots: Map[SlotType.Range, SlotInfo7_0]) extends Slots7_0

  object Slots {
    import SlotType.Range

    private[protocol] final def find[I](slots: Map[Range, I], slot: Slot): Option[I] =
      slots.collectFirst { case (Range(from, to), info) if slot.value >= from.value && slot.value <= to.value => info }

    // the node of Redis 6.2 and KeyDB: host, port and node id
    private type NodeReply6_2 = Arr.AsTuple[Bulk *: Num *: Bulk *: EmptyTuple]
    // the node of Redis 7.0 and Valkey: also its metadata, with a null host when the server says to use the one you connected to
    private type NodeReply7_0 = Arr.AsTuple[OrNullBulk[Bulk] *: Num *: Bulk *: Arr.OfPairs[Bulk, Bulk] *: EmptyTuple]
    // the first and last slot and the master, then the replicas
    private type Assigned[N] = Arr.AsTupleThen[Num *: Num *: N *: EmptyTuple, N]

    // an empty host, like a null one, means the server doesn't know it, so use the one you connected to
    private implicit final val hostRead: Read[Bulk, Option[Host]] = Read.instancePF("Bulk(Host)") {
      case Bulk("")      => None
      case Bulk(Host(h)) => Some(h)
    }
    private implicit final val portRead: Read[Num, Port]      = Read.instancePF("Num(Port)") { case Num(ToInt(Port(p))) => p }
    private implicit final val nodeIdRead: Read[Bulk, NodeId] = ClusterManagement.nodeId
    // what Redis 6.2 reads of either node, and what Redis 7.0 reads of its own
    private implicit final val node6_2Read: Read[NodeReply6_2, SlotNode6_2] =
      Read[NodeReply6_2, Option[Host] *: Port *: NodeId *: EmptyTuple].map { case host *: port *: id *: _ => SlotNodeOf6_2(host, port, id) }
    private implicit final val node7_0As6_2Read: Read[NodeReply7_0, SlotNode6_2] =
      Read[NodeReply7_0, Option[Host] *: Port *: NodeId *: Map[String, String] *: EmptyTuple].map { case host *: port *: id *: _ =>
        SlotNodeOf6_2(host, port, id)
      }
    private implicit final val node7_0Read: Read[NodeReply7_0, SlotNode7_0] =
      Read[NodeReply7_0, Option[Host] *: Port *: NodeId *: Map[String, String] *: EmptyTuple].map {
        case host *: port *: id *: metadata *: _ =>
          SlotNodeOf7_0(host, port, id, metadata)
      }

    private final def slotsRead[N, SN, I, S](info: (SN, Seq[SN]) => I, all: Map[Range, I] => S)(
        implicit S: RESPSub[N],
        R: Read[N, SN]
    ): Read[Arr.Of[Assigned[N]], S] = {
      implicit val assigned: Read[Assigned[N], (Range, I)] =
        Read[Assigned[N], (Slot *: Slot *: SN *: EmptyTuple, Seq[SN])].map { case (from *: to *: master *: _, replicas) =>
          Range(from, to) -> info(master, replicas)
        }
      Read[Arr.Of[Assigned[N]], Seq[(Range, I)]].map(slots => all(slots.toMap))
    }

    private[protocol] final val anyNodes: Read[Arr.Of[Assigned[OneOf[NodeReply6_2 *: NodeReply7_0 *: EmptyTuple]]], Slots6_2] =
      slotsRead[OneOf[NodeReply6_2 *: NodeReply7_0 *: EmptyTuple], SlotNode6_2, SlotInfo6_2, Slots6_2](SlotInfoOf6_2(_, _), SlotsOf6_2(_))
    private[protocol] final val nodesWithMetadata: Read[Arr.Of[Assigned[NodeReply7_0]], Slots7_0] =
      slotsRead[NodeReply7_0, SlotNode7_0, SlotInfo7_0, Slots7_0](SlotInfoOf7_0(_, _), SlotsOf7_0(_))
  }

  /** The types of the replies every cluster sends */
  trait ClusterTypes {
    final type ClusterFlag              = ClusterP.Flag
    final type ClusterImportingSlotType = ClusterP.SlotType.ImportingSlot
    final type ClusterInfo              = ClusterP.Info
    final type ClusterLinkState         = ClusterP.LinkState
    final type ClusterMigratingSlotType = ClusterP.SlotType.MigratingSlot
    final type ClusterRangeSlotType     = ClusterP.SlotType.Range
    final type ClusterSingleSlotType    = ClusterP.SlotType.Single
    final val ClusterFlag              = ClusterP.Flag
    final val ClusterImportingSlotType = ClusterP.SlotType.ImportingSlot
    final val ClusterInfo              = ClusterP.Info
    final val ClusterLinkState         = ClusterP.LinkState
    final val ClusterMigratingSlotType = ClusterP.SlotType.MigratingSlot
    final val ClusterRangeSlotType     = ClusterP.SlotType.Range
    final val ClusterSingleSlotType    = ClusterP.SlotType.Single
  }

  /** The builders of Redis 6.2's cluster commands */
  trait ClusterBaseTypes extends ClusterTypes {
    final type ClusterFailoverMode = ClusterP.FailoverMode
    final type ClusterResetMode    = ClusterP.ResetMode
    final type ClusterSetSlotMode  = ClusterP.SetSlotMode
    final val ClusterFailoverMode = ClusterP.FailoverMode
    final val ClusterResetMode    = ClusterP.ResetMode
    final val ClusterSetSlotMode  = ClusterP.SetSlotMode
  }
  trait ClusterSlotStatsTypes { final val slotStatsMetric: ClusterManagement.SlotStatsMetricBuilders = ClusterManagement.SlotStatsMetric }
  trait ClusterScanTypes      {
    final type ClusterScan        = ClusterManagement.Scan
    final type ClusterScanCursor  = ClusterManagement.ScanCursor
    final type ClusterScanOptions = ClusterManagement.ScanOptions

    final val ClusterScanCursor: ClusterManagement.ScanCursorBuilders   = ClusterManagement.ScanCursor
    final val ClusterScanOptions: ClusterManagement.ScanOptionsBuilders = ClusterManagement.ScanOptions
  }

  // the same names for laserdisc's own code, which can't read the members a version overrides while it's being built
  private[protocol] object internal extends ClusterBaseTypes with ClusterSlotStatsTypes with ClusterScanTypes
}

trait ClusterCoreP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "CLUSTER HELP",
      "CLUSTER INFO",
      "CLUSTER KEYSLOT",
      "CLUSTER MYID",
      "CLUSTER NODES",
      "CLUSTER SLOTS",
      "READONLY",
      "READWRITE"
    )

  /** The types of this cluster's replies, and the builders of its commands */
  val clustertypes: ClusterP.ClusterTypes = new ClusterP.ClusterTypes {}

  import ClusterP.internal.*

  final val clusterhelp: Single[Seq[String]] = Protocol.help("CLUSTER")

  final val clusterinfo: Single[ClusterInfo] = Protocol("CLUSTER", "INFO").as[Bulk, ClusterInfo]

  final def keyslot(key: Key): Single[Slot] = Protocol("CLUSTER", "KEYSLOT" *: key *: EmptyTuple).as[Num, Slot]

  val nodes: Single[ClusterP.Nodes6_2] = Protocol("CLUSTER", "NODES").using(RESPRead.instance(ClusterP.Nodes.nodes6_2))

  final val readonly: Single[OK] = Protocol("READONLY", EmptyTuple).as[Str, OK]

  final val readwrite: Single[OK] = Protocol("READWRITE", EmptyTuple).as[Str, OK]

  // the nodes of CLUSTER SLOTS as Redis 6.2 and KeyDB send them, or with the metadata servers send from Redis 7.0
  val slots: Single[ClusterP.Slots6_2] = Protocol("CLUSTER", "SLOTS").using(RESPRead.instance(ClusterP.Slots.anyNodes))

  final val myid: Single[NodeId] = Protocol("CLUSTER", "MYID").using(RESPRead.instance(ClusterManagement.nodeIdOfText))
}

trait ClusterBaseP extends ClusterCoreP {
  override val clustertypes: ClusterP.ClusterBaseTypes = new ClusterP.ClusterBaseTypes {}

  import ClusterP.internal.*

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "CLUSTER ADDSLOTS",
      "CLUSTER BUMPEPOCH",
      "CLUSTER COUNT-FAILURE-REPORTS",
      "CLUSTER COUNTKEYSINSLOT",
      "CLUSTER DELSLOTS",
      "CLUSTER FAILOVER: FORCE TAKEOVER",
      "CLUSTER FLUSHSLOTS",
      "CLUSTER FORGET",
      "CLUSTER GETKEYSINSLOT",
      "CLUSTER MEET",
      "CLUSTER REPLICAS",
      "CLUSTER REPLICATE",
      "CLUSTER RESET: HARD SOFT",
      "CLUSTER SAVECONFIG",
      "CLUSTER SET-CONFIG-EPOCH",
      "CLUSTER SETSLOT: IMPORTING MIGRATING NODE STABLE",
      "CLUSTER SLAVES"
    )

  final def addslots(slots: OneOrMore[Slot]): Single[OK]   = Protocol("CLUSTER", "ADDSLOTS" *: slots.value *: EmptyTuple).as[Str, OK]
  final def addslots(slot: Slot, slots: Slot*): Single[OK] =
    addslots(OneOrMore.unsafeFrom(slot :: slots.toList))

  final def countfailurereports(nodeId: NodeId): Single[NonNegInt] =
    Protocol("CLUSTER", "COUNT-FAILURE-REPORTS" *: nodeId *: EmptyTuple).as[Num, NonNegInt]

  final def countkeysinslot(slot: Slot): Single[NonNegInt] =
    Protocol("CLUSTER", "COUNTKEYSINSLOT" *: slot *: EmptyTuple).as[Num, NonNegInt]

  final def delslots(slots: OneOrMore[Slot]): Single[OK]   = Protocol("CLUSTER", "DELSLOTS" *: slots.value *: EmptyTuple).as[Str, OK]
  final def delslots(slot: Slot, slots: Slot*): Single[OK] =
    delslots(OneOrMore.unsafeFrom(slot :: slots.toList))

  final val failover: Single[OK]                            = Protocol("CLUSTER", "FAILOVER").as[Str, OK]
  final def failover(mode: ClusterFailoverMode): Single[OK] = Protocol("CLUSTER", "FAILOVER" *: mode *: EmptyTuple).as[Str, OK]

  final def forget(nodeId: NodeId): Single[OK] = Protocol("CLUSTER", "FORGET" *: nodeId *: EmptyTuple).as[Str, OK]

  final def getkeysinslot(slot: Slot, count: PosInt): Single[Seq[Key]] =
    Protocol("CLUSTER", "GETKEYSINSLOT" *: slot *: count *: EmptyTuple).as[Arr.Of[Bulk], Seq[Key]]

  final def meet(host: Host, port: Port): Single[OK] = Protocol("CLUSTER", "MEET" *: host *: port *: EmptyTuple).as[Str, OK]

  def replicas(nodeId: NodeId): Single[ClusterP.Nodes6_2] =
    Protocol("CLUSTER", "REPLICAS" *: nodeId *: EmptyTuple).using(RESPRead.instance(ClusterP.Nodes.nodesOfArr6_2))

  final def replicate(nodeId: NodeId): Single[OK] = Protocol("CLUSTER", "REPLICATE" *: nodeId *: EmptyTuple).as[Str, OK]

  final def reset(mode: ClusterResetMode): Single[OK] = Protocol("CLUSTER", "RESET" *: mode *: EmptyTuple).as[Str, OK]
  final val reset: Single[OK]                         = reset(ClusterResetMode.soft)

  final val saveconfig: Single[OK] = Protocol("CLUSTER", "SAVECONFIG").as[Str, OK]

  final def setconfigepoch(configEpoch: NonNegInt): Single[OK] =
    Protocol("CLUSTER", "SET-CONFIG-EPOCH" *: configEpoch *: EmptyTuple).as[Str, OK]

  final def setslot(slot: Slot): Single[OK] = Protocol("CLUSTER", "SETSLOT" *: slot *: "STABLE" *: EmptyTuple).as[Str, OK]
  final def setslot(slot: Slot, mode: ClusterSetSlotMode, node: NodeId): Single[OK] =
    Protocol("CLUSTER", "SETSLOT" *: slot *: mode *: node *: EmptyTuple).as[Str, OK]

  def slaves(nodeId: NodeId): Single[ClusterP.Nodes6_2] =
    Protocol("CLUSTER", "SLAVES" *: nodeId *: EmptyTuple).using(RESPRead.instance(ClusterP.Nodes.nodesOfArr6_2))

  /** Gives this node a new config epoch, and replies with the server's message: BUMPED or STILL, with the epoch */
  final val bumpepoch: Single[String] = Protocol("CLUSTER", "BUMPEPOCH").as[Str, String]

  /** Forgets every slot this node serves */
  final val flushslots: Single[OK] = Protocol("CLUSTER", "FLUSHSLOTS").as[Str, OK]
}

/** CLUSTER NODES, REPLICAS and SLAVES with the hostname each node announces, and CLUSTER SLOTS with each node's metadata: Redis 7.0 */
trait ClusterSlotsMetadataP extends ClusterBaseP {
  override val nodes: Single[ClusterP.Nodes7_0] = Protocol("CLUSTER", "NODES").using(RESPRead.instance(ClusterP.Nodes.nodes7_0))
  override val slots: Single[ClusterP.Slots7_0] =
    Protocol("CLUSTER", "SLOTS").using(RESPRead.instance(ClusterP.Slots.nodesWithMetadata))
  override def replicas(nodeId: NodeId): Single[ClusterP.Nodes7_0] =
    Protocol("CLUSTER", "REPLICAS" *: nodeId *: EmptyTuple).using(RESPRead.instance(ClusterP.Nodes.nodesOfArr7_0))
  override def slaves(nodeId: NodeId): Single[ClusterP.Nodes7_0] =
    Protocol("CLUSTER", "SLAVES" *: nodeId *: EmptyTuple).using(RESPRead.instance(ClusterP.Nodes.nodesOfArr7_0))
}

trait ClusterP
    extends ClusterBaseP
    with ClusterSlotsMetadataP
    with ClusterShardsP
    with ClusterMyShardIdP
    with ClusterSlotStatsP
    with ClusterSlotStatsMemoryP
    with ClusterMigrationP
    with ClusterSetSlotTimeoutP
    with ClusterReplicateNoOneP
    with ClusterShardIdP
    with ClusterSlotMigrationsP
    with ClusterScanP {
  override val clustertypes: ClusterP.ClusterBaseTypes with ClusterP.ClusterSlotStatsTypes with ClusterP.ClusterScanTypes =
    new ClusterP.ClusterBaseTypes with ClusterP.ClusterSlotStatsTypes with ClusterP.ClusterScanTypes {}
}
