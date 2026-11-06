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
object ServerP {
  import scala.language.dynamics

  /** The name of a section in INFO's reply: one laserdisc names, or `Other` for one it doesn't, like a module's or KeyDB's */
  sealed trait InfoSectionKey
  object InfoSectionKey {
    final case class Other(name: String) extends InfoSectionKey
  }

  /** A section of INFO in Redis 6.2, or a group of them (`all`, `everything` and `default`) */
  sealed trait InfoSection extends InfoSectionKey
  object InfoSection {
    final type default = default.type

    object server       extends InfoSection
    object clients      extends InfoSection
    object memory       extends InfoSection
    object persistence  extends InfoSection
    object stats        extends InfoSection
    object replication  extends InfoSection
    object cpu          extends InfoSection
    object modules      extends InfoSection
    object commandstats extends InfoSection
    object errorstats   extends InfoSection
    object cluster      extends InfoSection
    object keyspace     extends InfoSection
    object all          extends InfoSection
    object everything   extends InfoSection
    object default      extends InfoSection

    final val latencystats: LatencyStatsSection = new LatencyStatsSection
    final val threads: Redis8Section            = new Redis8Section("threads")
    final val keysizes: Redis8Section           = new Redis8Section("keysizes")
    final val hotkeys: HotKeysSection           = new HotKeysSection
    final val tls: TlsSection                   = new TlsSection

    implicit val defaultShow: Show[default]         = Show.const("default")
    implicit val infoSectionShow: Show[InfoSection] = Show.instance {
      case `server`       => "server"
      case `clients`      => "clients"
      case `memory`       => "memory"
      case `persistence`  => "persistence"
      case `stats`        => "stats"
      case `replication`  => "replication"
      case `cpu`          => "cpu"
      case `modules`      => "modules"
      case `commandstats` => "commandstats"
      case `errorstats`   => "errorstats"
      case `cluster`      => "cluster"
      case `keyspace`     => "keyspace"
      case `all`          => "all"
      case `everything`   => "everything"
      case `default`      => "default"
    }
  }

  /** A section of INFO that a later version added */
  sealed abstract class LaterInfoSection(val name: String) extends InfoSectionKey
  object LaterInfoSection {
    implicit def laterInfoSectionShow[S <: LaterInfoSection]: Show[S] = Show.instance(_.name)
  }

  /** LATENCYSTATS: Redis 7.0 */
  final class LatencyStatsSection private[ServerP] () extends LaterInfoSection("latencystats")

  /** THREADS and KEYSIZES: Redis 8.0 */
  final class Redis8Section private[ServerP] (name: String) extends LaterInfoSection(name)

  /** HOTKEYS: Redis 8.6 */
  final class HotKeysSection private[ServerP] () extends LaterInfoSection("hotkeys")

  /** TLS: Valkey 9.1 */
  final class TlsSection private[ServerP] () extends LaterInfoSection("tls")

  sealed trait ShutdownFlag
  object ShutdownFlag {
    object nosave extends ShutdownFlag
    object save   extends ShutdownFlag

    implicit val shutdownFlagShow: Show[ShutdownFlag] = Show.instance {
      case `nosave` => "nosave"
      case `save`   => "save"
    }
  }

  sealed trait Role
  object Role {
    final case class Client(host: Host, port: Port, lastAcknowledgedReplicationOffset: NonNegLong)

    sealed trait ReplicaStatus
    case object none       extends ReplicaStatus
    case object connect    extends ReplicaStatus
    case object connecting extends ReplicaStatus
    case object handshake  extends ReplicaStatus
    case object sync       extends ReplicaStatus
    case object connected  extends ReplicaStatus
    case object unknown    extends ReplicaStatus

    final case class Master(currentReplicationOffset: NonNegLong, clients: Seq[Client]) extends Role

    /** `masterReplicationOffset` is `None` until the replica has synced with its master */
    final case class Slave(
        masterHost: Host,
        masterPort: Port,
        masterViewReplicaStatus: ReplicaStatus,
        masterReplicationOffset: Option[NonNegLong]
    ) extends Role
    final case class Sentinel(masterNames: Seq[Key]) extends Role

    private type ClientReply   = Arr.AsTuple[Bulk *: Bulk *: Bulk *: EmptyTuple]
    private type MasterReply   = Arr.AsTuple[Bulk *: Num *: Arr.Of[ClientReply] *: EmptyTuple]
    private type SlaveReply    = Arr.AsTuple[Bulk *: Bulk *: Num *: Bulk *: Num *: EmptyTuple]
    private type SentinelReply = Arr.AsTuple[Bulk *: Arr.Of[Bulk] *: EmptyTuple]

    private implicit final val hostRead: Read[Bulk, Host]                = Read.instancePF("Bulk(Host)") { case Bulk(Host(h)) => h }
    private implicit final val bulkPortRead: Read[Bulk, Port]            = Read.instancePF("Bulk(Port)") { case Bulk(ToInt(Port(p))) => p }
    private implicit final val numPortRead: Read[Num, Port]              = Read.instancePF("Num(Port)") { case Num(ToInt(Port(p))) => p }
    private implicit final val offsetRead: Read[Num, Option[NonNegLong]] = Read.numMinusOneIsNone[NonNegLong]
    private implicit final val statusRead: Read[Bulk, ReplicaStatus]     = Read.instancePF("Bulk(ReplicaStatus)") {
      case Bulk("none")       => none
      case Bulk("connect")    => connect
      case Bulk("connecting") => connecting
      case Bulk("handshake")  => handshake
      case Bulk("sync")       => sync
      case Bulk("connected")  => connected
      case Bulk("unknown")    => unknown
    }
    private implicit final val clientRead: Read[ClientReply, Client] =
      Read[ClientReply, Host *: Port *: NonNegLong *: EmptyTuple].map { case host *: port *: offset *: _ => Client(host, port, offset) }

    // a role's reply starts with its name
    private final def named[T, L <: Tuple](name: String)(R: Read[T, String *: L]): Read[T, L] = Read.instance {
      R.read(_).flatMap { case n *: l => if (n == name) Right(l) else Left(RESPDecErr(s"Expected the role $name but was $n")) }
    }
    private implicit final val masterRead: Read[MasterReply, Role] =
      named("master")(Read[MasterReply, String *: NonNegLong *: Seq[Client] *: EmptyTuple]).map { case offset *: clients *: _ =>
        Master(offset, clients)
      }
    private implicit final val slaveRead: Read[SlaveReply, Role] =
      named("slave")(Read[SlaveReply, String *: Host *: Port *: ReplicaStatus *: Option[NonNegLong] *: EmptyTuple]).map {
        case host *: port *: status *: offset *: _ => Slave(host, port, status, offset)
      }
    private implicit final val sentinelRead: Read[SentinelReply, Role] =
      named("sentinel")(Read[SentinelReply, String *: Seq[Key] *: EmptyTuple]).map { case masters *: _ => Sentinel(masters) }

    private[protocol] final val roleRead: Read[OneOf[MasterReply *: SlaveReply *: SentinelReply *: EmptyTuple], Role] =
      Read[OneOf[MasterReply *: SlaveReply *: SentinelReply *: EmptyTuple], Role]
  }

  final class Parameters(private val properties: Map[String, String]) extends AnyVal with Dynamic {
    def selectDynamic[A](field: String)(implicit R: Read[String, A]): Maybe[A] =
      properties
        .get(field)
        .toRight(RESPDecErr(s"no key $field of the provided type found"))
        .flatMap(R.read)
        .widenLeft[Throwable]
  }

  final case class ConnectedClients(clients: Seq[Parameters])
  object ConnectedClients {
    private val KVPair = "(.*)=(.*)".r

    implicit val connectedClientsRead: Read[Bulk, ConnectedClients] = Read.instance { case Bulk(s) =>
      Right(
        ConnectedClients(
          s.split(LF_CH).toIndexedSeq.map { clientData =>
            new Parameters(clientData.split(SPACE_CH).collect { case KVPair(k, v) => k -> v }.toMap)
          }
        )
      )
    }
  }

  final case class Configuration(parameters: Parameters)
  object Configuration {
    implicit val configRead: Read[Arr.OfPairs[Bulk, Bulk], Configuration] =
      Read[Arr.OfPairs[Bulk, Bulk], Map[String, String]].map(kvs => Configuration(new Parameters(kvs)))
  }

  final case class Info(sections: Map[InfoSectionKey, Parameters])
  object Info {
    private def section(name: String): InfoSectionKey = name.toLowerCase match {
      case "server"       => InfoSection.server
      case "clients"      => InfoSection.clients
      case "memory"       => InfoSection.memory
      case "persistence"  => InfoSection.persistence
      case "stats"        => InfoSection.stats
      case "replication"  => InfoSection.replication
      case "cpu"          => InfoSection.cpu
      case "modules"      => InfoSection.modules
      case "commandstats" => InfoSection.commandstats
      case "errorstats"   => InfoSection.errorstats
      case "cluster"      => InfoSection.cluster
      case "keyspace"     => InfoSection.keyspace
      case "latencystats" => InfoSection.latencystats
      case "threads"      => InfoSection.threads
      case "keysizes"     => InfoSection.keysizes
      case "hotkeys"      => InfoSection.hotkeys
      case "tls"          => InfoSection.tls
      case other          => InfoSectionKey.Other(other)
    }

    // sections start with a "# Name" line, followed by "key:value" lines, and are separated by empty lines
    implicit val infoRead: Read[Bulk, Info] = Read.instance { case Bulk(s) =>
      s.split("\r?\n")
        .toList
        .foldLeft[Either[RESPDecErr, List[(InfoSectionKey, List[String])]]](Right(Nil)) {
          case (Right(sections), line) if line.startsWith("# ") => Right((section(line.drop(2).trim) -> Nil) :: sections)
          case (Right(sections), line) if line.trim.isEmpty     => Right(sections)
          case (Right((current, lines) :: sections), line)      => Right((current -> (line :: lines)) :: sections)
          case (Right(Nil), line) => Left(RESPDecErr(s"Read[Bulk, Info], a line came before any section: $line"))
          case (left, _)          => left
        }
        .flatMap { sections =>
          sections.reverse.foldRight[Either[RESPDecErr, List[(InfoSectionKey, Parameters)]]](Right(Nil)) {
            case ((name, lines), Right(read)) =>
              KVPS.read(lines.reverse) match {
                case Right(kvs) => Right((name -> new Parameters(kvs.toMap)) :: read)
                case Left(e)    => Left(RESPDecErr(s"Read[Bulk, Info], error decoding the $name section: ${e.message}"))
              }
            case (_, left) => left
          }
        }
        .map(sections => Info(sections.toMap))
    }
  }

  /** The type of a client, for CLIENT KILL and CLIENT LIST */
  sealed trait ClientType
  object ClientType {
    object normal  extends ClientType
    object master  extends ClientType
    object replica extends ClientType
    object pubsub  extends ClientType

    implicit val clientTypeShow: Show[ClientType] = Show.instance {
      case `normal`  => "normal"
      case `master`  => "master"
      case `replica` => "replica"
      case `pubsub`  => "pubsub"
    }
  }

  /** WRITE and ALL, for CLIENT PAUSE: pause only clients that write, or all of them */
  sealed trait PauseMode
  object PauseMode {
    object write extends PauseMode
    object all   extends PauseMode

    implicit val pauseModeShow: Show[PauseMode] = Show.instance {
      case `write` => "WRITE"
      case `all`   => "ALL"
    }
  }

  /** TIMEOUT and ERROR, for CLIENT UNBLOCK: unblock the client as if its command timed out, or with an error */
  sealed trait UnblockMode
  object UnblockMode {
    object timeout extends UnblockMode
    object error   extends UnblockMode

    implicit val unblockModeShow: Show[UnblockMode] = Show.instance {
      case `timeout` => "TIMEOUT"
      case `error`   => "ERROR"
    }
  }

  /** The filters of CLIENT KILL: a client is closed only if it matches all of them */
  final class ClientFilter private (val params: List[String]) {
    import Show.{hostShow, portShow}

    def id(id: NonNegLong): ClientFilter              = new ClientFilter(params ++ List("ID", id.value.toString))
    def address(host: Host, port: Port): ClientFilter =
      new ClientFilter(params ++ List("ADDR", s"${hostShow.show(host)}:${portShow.show(port)}"))
    def localAddress(host: Host, port: Port): ClientFilter =
      new ClientFilter(params ++ List("LADDR", s"${hostShow.show(host)}:${portShow.show(port)}"))
    def clientType(clientType: ClientType): ClientFilter =
      new ClientFilter(params ++ List("TYPE", ClientType.clientTypeShow.show(clientType)))
    def user(username: String): ClientFilter = new ClientFilter(params ++ List("USER", username))

    /** Whether to spare the client sending the command, which is the default */
    def skipMe(skip: Boolean): ClientFilter = new ClientFilter(params ++ List("SKIPME", if (skip) "yes" else "no"))
  }
  object ClientFilter {
    private final val none = new ClientFilter(Nil)

    def id(id: NonNegLong): ClientFilter                   = none.id(id)
    def address(host: Host, port: Port): ClientFilter      = none.address(host, port)
    def localAddress(host: Host, port: Port): ClientFilter = none.localAddress(host, port)
    def clientType(clientType: ClientType): ClientFilter   = none.clientType(clientType)
    def user(username: String): ClientFilter               = none.user(username)
    def skipMe(skip: Boolean): ClientFilter                = none.skipMe(skip)
  }

  /** NOW and FORCE, for SHUTDOWN: don't wait for replicas to catch up, and shut down even if saving fails */
  final class ShutdownOptions private (val params: List[String]) {
    def now: ShutdownOptions   = new ShutdownOptions(params :+ "NOW")
    def force: ShutdownOptions = new ShutdownOptions(params :+ "FORCE")
  }
  object ShutdownOptions {
    final val now: ShutdownOptions   = new ShutdownOptions(List("NOW"))
    final val force: ShutdownOptions = new ShutdownOptions(List("FORCE"))
  }

  /** What CLIENT INFO replies: the same fields as a line of CLIENT LIST, for the client sending it */
  final case class ClientInfo(parameters: Parameters)
  object ClientInfo {
    implicit val clientInfoRead: Read[Bulk, ClientInfo] = ConnectedClients.connectedClientsRead.flatMap {
      case ConnectedClients(Seq(parameters)) => Read.const(ClientInfo(parameters))
      case other => Read.instance(_ => Left(RESPDecErr(s"Expected the information of one client, but was $other")))
    }
  }

}

/** CLIENT: Redis 6.2 */
trait ClientCommands extends Features {
  import ServerP.{ClientFilter, ClientInfo, ClientType, ConnectedClients, PauseMode, UnblockMode}
  import Show.{hostShow, portShow}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "CLIENT GETNAME",
      "CLIENT HELP",
      "CLIENT ID",
      "CLIENT INFO",
      "CLIENT KILL: ADDR ID LADDR MASTER NORMAL PUBSUB REPLICA SKIPME TYPE USER",
      "CLIENT LIST: ID MASTER NORMAL PUBSUB REPLICA TYPE",
      "CLIENT PAUSE: ALL WRITE",
      "CLIENT SETNAME",
      "CLIENT UNBLOCK: ERROR TIMEOUT",
      "CLIENT UNPAUSE"
    )

  val help: Single.Aux[Seq[String]] = Protocol.help("CLIENT")

  val getname: Single.Aux[Option[ConnectionName]] = Protocol("CLIENT", "GETNAME").as[OrNullBulk[Bulk], Option[ConnectionName]]

  val id: Single.Aux[NonNegLong] = Protocol("CLIENT", "ID").as[Num, NonNegLong]

  val info: Single.Aux[ClientInfo] = Protocol("CLIENT", "INFO").as[Bulk, ClientInfo]

  def kill(host: Host, port: Port): Single.Aux[OK] =
    Protocol("CLIENT", "KILL" :: s"${hostShow.show(host)}:${portShow.show(port)}" :: Nil).as[Str, OK]

  /** Closes the clients that match every filter, and replies how many it closed */
  def kill(filter: ClientFilter): Single.Aux[NonNegLong] = Protocol("CLIENT", "KILL" :: filter.params).as[Num, NonNegLong]

  val list: Single.Aux[ConnectedClients]                         = Protocol("CLIENT", "LIST").as[Bulk, ConnectedClients]
  def list(clientType: ClientType): Single.Aux[ConnectedClients] =
    Protocol("CLIENT", "LIST" *: "TYPE" *: clientType *: EmptyTuple).as[Bulk, ConnectedClients]
  def list(ids: OneOrMore[NonNegLong]): Single.Aux[ConnectedClients] =
    Protocol("CLIENT", "LIST" *: "ID" *: ids.value *: EmptyTuple).as[Bulk, ConnectedClients]

  def pause(milliseconds: PosLong): Single.Aux[OK]                  = Protocol("CLIENT", "PAUSE" *: milliseconds *: EmptyTuple).as[Str, OK]
  def pause(milliseconds: PosLong, mode: PauseMode): Single.Aux[OK] =
    Protocol("CLIENT", "PAUSE" *: milliseconds *: mode *: EmptyTuple).as[Str, OK]

  def setname(connectionName: ConnectionName): Single.Aux[OK] = Protocol("CLIENT", "SETNAME" :: connectionName.value :: Nil).as[Str, OK]

  /** Unblocks a client waiting in a blocking command, and replies whether there was one */
  def unblock(id: NonNegLong): Single.Aux[Boolean]                    = Protocol("CLIENT", "UNBLOCK" *: id *: EmptyTuple).as[Num, Boolean]
  def unblock(id: NonNegLong, mode: UnblockMode): Single.Aux[Boolean] =
    Protocol("CLIENT", "UNBLOCK" *: id *: mode *: EmptyTuple).as[Num, Boolean]

  val unpause: Single.Aux[OK] = Protocol("CLIENT", "UNPAUSE").as[Str, OK]

  val unsetname: Single.Aux[OK] = Protocol("CLIENT", "SETNAME" :: "" :: Nil).as[Str, OK]
}

/** CLIENT NO-EVICT: Redis 7.0 */
trait ClientNoEvictCommands extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLIENT NO-EVICT: OFF ON")

  /** Whether this connection is spared when the server evicts clients to free memory */
  def noevict(on: Boolean): Single.Aux[OK] = Protocol("CLIENT", "NO-EVICT" :: (if (on) "ON" else "OFF") :: Nil).as[Str, OK]
}

/** CLIENT NO-TOUCH and CLIENT SETINFO: Redis 7.2 */
trait ClientNoTouchCommands extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("CLIENT NO-TOUCH: OFF ON", "CLIENT SETINFO: LIB-NAME LIB-VER")

  /** Whether this connection's commands leave the keys' last access time as it was */
  def notouch(on: Boolean): Single.Aux[OK] = Protocol("CLIENT", "NO-TOUCH" :: (if (on) "ON" else "OFF") :: Nil).as[Str, OK]

  /** The name of the library this connection uses, shown by CLIENT LIST */
  def setlibname(name: Key): Single.Aux[OK] = Protocol("CLIENT", "SETINFO" :: "LIB-NAME" :: name.value :: Nil).as[Str, OK]

  /** The version of the library this connection uses, shown by CLIENT LIST */
  def setlibversion(version: Key): Single.Aux[OK] = Protocol("CLIENT", "SETINFO" :: "LIB-VER" :: version.value :: Nil).as[Str, OK]
}

/** CLIENT CAPA: Valkey 8.0 */
trait ClientCapaCommands extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLIENT CAPA")

  /** Tells the server what this connection can handle, like `redirect`. A server takes names it doesn't know, and ignores them */
  def capa(capabilities: OneOrMore[Key]): Single.Aux[OK] = Protocol("CLIENT", "CAPA" :: capabilities.value.map(_.value)).as[Str, OK]
}

/** CLIENT IMPORT-SOURCE: Valkey 8.1 */
trait ClientImportSourceCommands extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLIENT IMPORT-SOURCE: OFF ON")

  /** Whether this connection is an import source. Turning it on needs the server to be in import mode */
  def importsource(on: Boolean): Single.Aux[OK] = Protocol("CLIENT", "IMPORT-SOURCE" :: (if (on) "ON" else "OFF") :: Nil).as[Str, OK]
}

/** CLIENT KILL with MAXAGE: Redis 7.4, Valkey 8.0 */
trait ClientMaxAgeCommands extends Features {
  import ServerP.ClientFilter

  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLIENT KILL: MAXAGE")

  /** Closes the clients that match every filter and have been connected for longer than `seconds` */
  def kill(filter: ClientFilter, seconds: PosLong): Single.Aux[NonNegLong] =
    Protocol("CLIENT", "KILL" *: filter.params *: "MAXAGE" *: seconds *: EmptyTuple).as[Num, NonNegLong]
}

/** CONFIG: Redis 6.2 */
trait ConfigCommands extends Features {
  import ServerP.Configuration

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("CONFIG GET", "CONFIG HELP", "CONFIG RESETSTAT", "CONFIG REWRITE", "CONFIG SET")

  val help: Single.Aux[Seq[String]] = Protocol.help("CONFIG")

  def get(parameter: GlobPattern): Single.Aux[Configuration] =
    Protocol("CONFIG", "GET" :: parameter.value :: Nil).as[Arr.OfPairs[Bulk, Bulk], Configuration]

  val resetstat: Single.Aux[OK] = Protocol("CONFIG", "RESETSTAT").as[Str, OK]

  val rewrite: Single.Aux[OK] = Protocol("CONFIG", "REWRITE").as[Str, OK]

  def set[A: Show](parameter: Key, value: A): Single.Aux[OK] = Protocol("CONFIG", "SET" *: parameter *: value *: EmptyTuple).as[Str, OK]
}

/** CONFIG GET and CONFIG SET with several parameters: Redis 7.0 */
trait ConfigManyCommands extends Features {
  import ServerP.Configuration

  def get(parameters: OneOrMore[GlobPattern]): Single.Aux[Configuration] =
    Protocol("CONFIG", "GET" :: parameters.value.map(_.value)).as[Arr.OfPairs[Bulk, Bulk], Configuration]

  /** Sets every parameter, or none of them if one can't be set */
  def set[A: Show](parameters: OneOrMore[(Key, A)]): Single.Aux[OK] =
    Protocol("CONFIG", "SET" *: parameters.value *: EmptyTuple).as[Str, OK]
}

trait ServerBaseP extends ServerInspectionBaseP {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "BGREWRITEAOF",
      "BGSAVE: SCHEDULE",
      "DBSIZE",
      "FAILOVER: ABORT FORCE TIMEOUT TO",
      "FLUSHALL: ASYNC SYNC",
      "FLUSHDB: ASYNC SYNC",
      "INFO",
      "LASTSAVE",
      "LOLWUT: VERSION",
      "REPLICAOF: NO ONE",
      "ROLE",
      "SAVE",
      "SHUTDOWN: NOSAVE SAVE",
      "SLAVEOF: NO ONE",
      "TIME"
    ) ++ client.supported ++ config.supported ++ latency.supported ++ command.supported ++ acl.supported

  import ServerP.{Info, InfoSection, Role, ShutdownFlag}

  object servertypes {
    final val aclRules         = Acl.AclRules.none
    final val aclSelectorRules = Acl.AclRulesWithSelectors.none
    final val clientFilter     = ServerP.ClientFilter
    final val clientType       = ServerP.ClientType
    final val commandFilter    = ServerInspection.CommandFilter
    final val flushMode        = ClusterManagement.FlushMode
    final val slotStatsMetric  = ClusterManagement.SlotStatsMetric
    final val commandLogType   = ServerInspection.CommandLogType
    final val hotKeysMetrics   = ServerInspection.HotKeysMetrics
    final val hotKeysOptions   = ServerInspection.HotKeysOptions
    final val flag             = ShutdownFlag
    final val info             = InfoSection
    final val pauseMode        = ServerP.PauseMode
    final val shutdownOptions  = ServerP.ShutdownOptions
    final val unblockMode      = ServerP.UnblockMode
  }

  /** A piece of art made by the server, and its version, as text. It's in every server below but KeyDB */
  final val lolwut: Single.Aux[String]                  = Protocol("LOLWUT", EmptyTuple).as[Bulk, String]
  final def lolwut(version: PosInt): Single.Aux[String] = Protocol("LOLWUT", "VERSION" *: version *: EmptyTuple).as[Bulk, String]

  lazy val client: ClientCommands = new ClientCommands {}

  lazy val config: ConfigCommands = new ConfigCommands {}

  lazy val latency: LatencyCommands = new LatencyCommands {}

  lazy val command: CommandCommands = new CommandCommands {}

  lazy val acl: AclCommands = new AclCommands {}

  /** Replies with the server's message, like "Background append only file rewriting started" */
  final val bgrewriteaof: Single.Aux[String] = Protocol("BGREWRITEAOF", EmptyTuple).as[Str, String]

  /** Replies with the server's message, like "Background saving started" */
  final val bgsave: Single.Aux[String] = Protocol("BGSAVE", EmptyTuple).as[Str, String]

  /** BGSAVE that waits for a rewrite of the append only file in progress instead of failing */
  final val bgsaveschedule: Single.Aux[String] = Protocol("BGSAVE", "SCHEDULE").as[Str, String]

  final val dbsize: Single.Aux[NonNegLong] = Protocol("DBSIZE", EmptyTuple).as[Num, NonNegLong]

  /** FAILOVER: hands the primary role to a replica that has caught up, outside a cluster (CLUSTER FAILOVER is `failover`) */
  final val replicafailover: Single.Aux[OK]                        = Protocol("FAILOVER", EmptyTuple).as[Str, OK]
  final def replicafailover(milliseconds: PosLong): Single.Aux[OK] =
    Protocol("FAILOVER", "TIMEOUT" *: milliseconds *: EmptyTuple).as[Str, OK]
  final def replicafailover(host: Host, port: Port): Single.Aux[OK] = Protocol("FAILOVER", "TO" *: host *: port *: EmptyTuple).as[Str, OK]
  final def replicafailover(host: Host, port: Port, milliseconds: PosLong): Single.Aux[OK] =
    Protocol("FAILOVER", "TO" *: host *: port *: "TIMEOUT" *: milliseconds *: EmptyTuple).as[Str, OK]

  /** FAILOVER to that replica even if it hasn't caught up when the time is up */
  final def replicafailoverforce(host: Host, port: Port, milliseconds: PosLong): Single.Aux[OK] =
    Protocol("FAILOVER", "TO" *: host *: port *: "FORCE" *: "TIMEOUT" *: milliseconds *: EmptyTuple).as[Str, OK]

  /** Stops a FAILOVER in progress */
  final val replicafailoverabort: Single.Aux[OK] = Protocol("FAILOVER", "ABORT").as[Str, OK]

  final val flushall: Single.Aux[OK] = Protocol("FLUSHALL", EmptyTuple).as[Str, OK]

  final val flushallasync: Single.Aux[OK] = Protocol("FLUSHALL", "ASYNC").as[Str, OK]

  final val flushallsync: Single.Aux[OK] = Protocol("FLUSHALL", "SYNC").as[Str, OK]

  final val flushdb: Single.Aux[OK] = Protocol("FLUSHDB", EmptyTuple).as[Str, OK]

  final val flushdbasync: Single.Aux[OK] = Protocol("FLUSHDB", "ASYNC").as[Str, OK]

  final val flushdbsync: Single.Aux[OK] = Protocol("FLUSHDB", "SYNC").as[Str, OK]

  final val info: Single.Aux[Info]                       = info(servertypes.info.default)
  final def info(section: InfoSection): Single.Aux[Info] = Protocol("INFO", section).as[Bulk, Info]

  final val lastsave: Single.Aux[NonNegLong] = Protocol("LASTSAVE", EmptyTuple).as[Num, NonNegLong]

  final def replicaof(host: Host, port: Port): Single.Aux[OK] = Protocol("REPLICAOF", host *: port *: EmptyTuple).as[Str, OK]

  final val replicaofnoone: Single.Aux[OK] = Protocol("REPLICAOF", "NO" :: "ONE" :: Nil).as[Str, OK]

  final val role: Single.Aux[Role] = Protocol("ROLE", EmptyTuple).using(RESPRead.instance(Role.roleRead))

  final val save: Single.Aux[OK] = Protocol("SAVE", EmptyTuple).as[Str, OK]

  final val shutdown: Single.Aux[OK]                     = Protocol("SHUTDOWN", EmptyTuple).as[Str, OK]
  final def shutdown(flag: ShutdownFlag): Single.Aux[OK] = Protocol("SHUTDOWN", flag).as[Str, OK]

  final def slaveof(host: Host, port: Port): Single.Aux[OK] = Protocol("SLAVEOF", host *: port *: EmptyTuple).as[Str, OK]

  final val slaveofnoone: Single.Aux[OK] = Protocol("SLAVEOF", "NO" :: "ONE" :: Nil).as[Str, OK]

  final val time: Single.Aux[Time] = Protocol("TIME", EmptyTuple).as[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple], Time]
}

/** SHUTDOWN with NOW, FORCE and ABORT, and INFO with several sections: Redis 7.0 */
trait ServerShutdownAndInfoP extends Features {
  import ServerP.{Info, InfoSection, LatencyStatsSection, ShutdownFlag, ShutdownOptions}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SHUTDOWN: ABORT FORCE NOW")

  final def shutdown(options: ShutdownOptions): Single.Aux[OK]                     = Protocol("SHUTDOWN", options.params).as[Str, OK]
  final def shutdown(flag: ShutdownFlag, options: ShutdownOptions): Single.Aux[OK] =
    Protocol("SHUTDOWN", flag *: options.params *: EmptyTuple).as[Str, OK]

  /** Stops a shutdown that is waiting for replicas to catch up */
  final val shutdownabort: Single.Aux[OK] = Protocol("SHUTDOWN", "ABORT").as[Str, OK]

  final def info(sections: OneOrMore[InfoSection]): Single.Aux[Info] = Protocol("INFO", sections.value).as[Bulk, Info]
  final def info(section: LatencyStatsSection): Single.Aux[Info]     = Protocol("INFO", section).as[Bulk, Info]
}

/** INFO THREADS and INFO KEYSIZES: Redis 8.0 */
trait ServerInfoRedis8P extends Features {
  import ServerP.{Info, Redis8Section}

  final def info(section: Redis8Section): Single.Aux[Info] = Protocol("INFO", section).as[Bulk, Info]
}

/** HOTKEYS and INFO HOTKEYS: Redis 8.6 */
trait ServerHotKeysP extends Features {
  import ServerP.{HotKeysSection, Info}

  override private[laserdisc] def supported: List[String] = super.supported ++ hotkeys.supported

  object hotkeys extends HotKeysCommands

  final def info(section: HotKeysSection): Single.Aux[Info] = Protocol("INFO", section).as[Bulk, Info]
}

/** COMMANDLOG: Valkey 8.1 */
trait ServerCommandLogP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ commandlog.supported

  object commandlog extends CommandLogCommands
}

/** INFO TLS: Valkey 9.1 */
trait ServerInfoTlsP extends Features {
  import ServerP.{Info, TlsSection}

  final def info(section: TlsSection): Single.Aux[Info] = Protocol("INFO", section).as[Bulk, Info]
}

/** BGSAVE CANCEL: Valkey 8.1 */
trait ServerBgSaveCancelP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("BGSAVE: CANCEL")

  /** Stops a BGSAVE in progress, or one scheduled with BGSAVE SCHEDULE */
  final val bgsavecancel: Single.Aux[String] = Protocol("BGSAVE", "CANCEL").as[Str, String]
}

trait ClientAllCommands
    extends ClientCommands
    with ClientNoEvictCommands
    with ClientNoTouchCommands
    with ClientMaxAgeCommands
    with ClientCapaCommands
    with ClientImportSourceCommands
trait ConfigAllCommands  extends ConfigCommands with ConfigManyCommands
trait LatencyAllCommands extends LatencyCommands with LatencyHistogramCommands
trait CommandAllCommands extends CommandCommands with CommandListCommands
trait ModuleAllCommands  extends ModuleCommands with ModuleLoadExCommands
trait AclAllCommands     extends AclCommands with AclSelectorCommands

trait ServerP
    extends ServerBaseP
    with ServerShutdownAndInfoP
    with ServerInfoRedis8P
    with ServerHotKeysP
    with ServerInfoTlsP
    with ServerBgSaveCancelP
    with ServerCommandLogP
    with ServerBackupP {
  override lazy val client: ClientAllCommands   = new ClientAllCommands {}
  override lazy val config: ConfigAllCommands   = new ConfigAllCommands {}
  override lazy val latency: LatencyAllCommands = new LatencyAllCommands {}
  override lazy val command: CommandAllCommands = new CommandAllCommands {}
  override lazy val module: ModuleAllCommands   = new ModuleAllCommands {}
  override lazy val acl: AclAllCommands         = new AclAllCommands {}
}
