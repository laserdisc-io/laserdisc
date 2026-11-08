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

object ServerP {

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

  final class Parameters(private val properties: Map[String, String]) extends AnyVal {
    def get[A](field: String)(implicit R: Read[String, A]): Maybe[A] =
      properties
        .get(field)
        .toRight(RESPDecErr(s"no field $field"))
        .flatMap(R.read)
        .widenLeft[Throwable]
  }

  final case class ConnectedClients(clients: Seq[Parameters])
  object ConnectedClients {
    private val KVPair = "(.*)=(.*)".r

    implicit val connectedClientsRead: Read[Bulk, ConnectedClients] = Read.instance { case Bulk(s) =>
      Right(
        ConnectedClients(
          s.split(LF_CH).toIndexedSeq.filter(_.nonEmpty).map { clientData =>
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
    object slave   extends ClientType
    object pubsub  extends ClientType
    // from Valkey 8.0, where `client.primary` gives it
    private[protocol] object primary extends ClientType

    implicit val clientTypeShow: Show[ClientType] = Show.instance {
      case `normal`  => "normal"
      case `master`  => "master"
      case `replica` => "replica"
      case `slave`   => "slave"
      case `pubsub`  => "pubsub"
      case `primary` => "primary"
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

    def id(id: NonNegLong): ClientFilter              = new ClientFilter(params ++ List("ID", Show[NonNegLong].show(id)))
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

    // from Valkey 8.1, where `client.ids` gives it
    private[protocol] def ids(ids: OneOrMore[NonNegLong]): ClientFilter = new ClientFilter("ID" :: ids.value.map(Show[NonNegLong].show))

    def id(id: NonNegLong): ClientFilter                   = none.id(id)
    def address(host: Host, port: Port): ClientFilter      = none.address(host, port)
    def localAddress(host: Host, port: Port): ClientFilter = none.localAddress(host, port)
    def clientType(clientType: ClientType): ClientFilter   = none.clientType(clientType)
    def user(username: String): ClientFilter               = none.user(username)
    def skipMe(skip: Boolean): ClientFilter                = none.skipMe(skip)
  }

  /** REDIRECT, BCAST, PREFIX, OPTIN, OPTOUT and NOLOOP, for CLIENT TRACKING. Which connection gets the notices, for every key under the
    * prefixes rather than only those read, only for the keys read right after `caching(true)` or all but those after `caching(false)`, and
    * not about this connection's own changes. A server refuses PREFIX without BCAST, and OPTIN with OPTOUT
    */
  final class TrackingOptions private (val params: List[String]) {
    def redirect(id: NonNegLong): TrackingOptions = new TrackingOptions(params ++ List("REDIRECT", Show[NonNegLong].show(id)))
    def bcast: TrackingOptions                    = new TrackingOptions(params :+ "BCAST")
    def prefix(prefix: Key): TrackingOptions      = new TrackingOptions(params ++ List("PREFIX", prefix.value))
    def optin: TrackingOptions                    = new TrackingOptions(params :+ "OPTIN")
    def optout: TrackingOptions                   = new TrackingOptions(params :+ "OPTOUT")
    def noloop: TrackingOptions                   = new TrackingOptions(params :+ "NOLOOP")
  }
  object TrackingOptions {
    def redirect(id: NonNegLong): TrackingOptions = new TrackingOptions(List("REDIRECT", Show[NonNegLong].show(id)))
  }

  /** CLIENT TRACKINGINFO: the connection's tracking flags, where its notices go, if anywhere, and its prefixes */
  final case class TrackingInfo(flags: Seq[String], redirect: Option[NonNegLong], prefixes: Seq[String])

  private[protocol] final val trackingInfo: Read[Record, TrackingInfo] = {
    implicit val redirect: Read[Num, Option[NonNegLong]] = Read.instance {
      case Num(-1L) | Num(0L) => Right(None)
      case other              => Read[Num, NonNegLong].read(other).map(Some(_))
    }
    Read.record("tracking info") { f =>
      for {
        flags    <- f[Arr.Of[Bulk], Seq[String]]("flags")
        target   <- f[Num, Option[NonNegLong]]("redirect")
        prefixes <- f[Arr.Of[Bulk], Seq[String]]("prefixes")
      } yield TrackingInfo(flags, target, prefixes)
    }
  }

  /** The filters of CLIENT KILL and CLIENT LIST on Valkey 9.0: a client must match all of them, and the `not` ones must not match. `flags`
    * and `capabilities` are the letters CLIENT LIST shows for them
    */
  final class ValkeyClientFilter private (val params: List[String]) {
    import Show.{hostShow, portShow}

    private def add(name: String, value: String): ValkeyClientFilter                 = new ValkeyClientFilter(params ++ List(name, value))
    private def hostAndPort(host: Host, port: Port): String                          = s"${hostShow.show(host)}:${portShow.show(port)}"
    private def idList(name: String, ids: OneOrMore[NonNegLong]): ValkeyClientFilter =
      new ValkeyClientFilter(params ++ (name :: ids.value.map(Show[NonNegLong].show)))

    def ids(ids: OneOrMore[NonNegLong]): ValkeyClientFilter      = idList("ID", ids)
    def clientType(clientType: ClientType): ValkeyClientFilter   = add("TYPE", ClientType.clientTypeShow.show(clientType))
    def user(username: String): ValkeyClientFilter               = add("USER", username)
    def address(host: Host, port: Port): ValkeyClientFilter      = add("ADDR", hostAndPort(host, port))
    def localAddress(host: Host, port: Port): ValkeyClientFilter = add("LADDR", hostAndPort(host, port))
    def ip(host: Host): ValkeyClientFilter                       = add("IP", hostShow.show(host))
    def name(name: ConnectionName): ValkeyClientFilter           = add("NAME", name.value)
    def flags(flags: Key): ValkeyClientFilter                    = add("FLAGS", flags.value)
    def libName(name: Key): ValkeyClientFilter                   = add("LIB-NAME", name.value)
    def libVersion(version: Key): ValkeyClientFilter             = add("LIB-VER", version.value)
    def db(index: DbIndex): ValkeyClientFilter                   = add("DB", Show[DbIndex].show(index))
    def capabilities(capabilities: Key): ValkeyClientFilter      = add("CAPA", capabilities.value)

    /** Connected for longer than `seconds` */
    def maxAge(seconds: PosLong): ValkeyClientFilter = add("MAXAGE", Show[PosLong].show(seconds))

    /** Idle for at least `seconds` */
    def idle(seconds: PosLong): ValkeyClientFilter = add("IDLE", Show[PosLong].show(seconds))

    /** Whether to spare the client sending the command */
    def skipMe(skip: Boolean): ValkeyClientFilter = add("SKIPME", if (skip) "yes" else "no")

    def notIds(ids: OneOrMore[NonNegLong]): ValkeyClientFilter      = idList("NOT-ID", ids)
    def notClientType(clientType: ClientType): ValkeyClientFilter   = add("NOT-TYPE", ClientType.clientTypeShow.show(clientType))
    def notUser(username: String): ValkeyClientFilter               = add("NOT-USER", username)
    def notAddress(host: Host, port: Port): ValkeyClientFilter      = add("NOT-ADDR", hostAndPort(host, port))
    def notLocalAddress(host: Host, port: Port): ValkeyClientFilter = add("NOT-LADDR", hostAndPort(host, port))
    def notIp(host: Host): ValkeyClientFilter                       = add("NOT-IP", hostShow.show(host))
    def notName(name: ConnectionName): ValkeyClientFilter           = add("NOT-NAME", name.value)
    def notFlags(flags: Key): ValkeyClientFilter                    = add("NOT-FLAGS", flags.value)
    def notLibName(name: Key): ValkeyClientFilter                   = add("NOT-LIB-NAME", name.value)
    def notLibVersion(version: Key): ValkeyClientFilter             = add("NOT-LIB-VER", version.value)
    def notDb(index: DbIndex): ValkeyClientFilter                   = add("NOT-DB", Show[DbIndex].show(index))
    def notCapabilities(capabilities: Key): ValkeyClientFilter      = add("NOT-CAPA", capabilities.value)
  }
  object ValkeyClientFilter {
    private final val none = new ValkeyClientFilter(Nil)

    def ids(ids: OneOrMore[NonNegLong]): ValkeyClientFilter         = none.ids(ids)
    def clientType(clientType: ClientType): ValkeyClientFilter      = none.clientType(clientType)
    def user(username: String): ValkeyClientFilter                  = none.user(username)
    def address(host: Host, port: Port): ValkeyClientFilter         = none.address(host, port)
    def localAddress(host: Host, port: Port): ValkeyClientFilter    = none.localAddress(host, port)
    def ip(host: Host): ValkeyClientFilter                          = none.ip(host)
    def name(name: ConnectionName): ValkeyClientFilter              = none.name(name)
    def flags(flags: Key): ValkeyClientFilter                       = none.flags(flags)
    def libName(name: Key): ValkeyClientFilter                      = none.libName(name)
    def libVersion(version: Key): ValkeyClientFilter                = none.libVersion(version)
    def db(index: DbIndex): ValkeyClientFilter                      = none.db(index)
    def capabilities(capabilities: Key): ValkeyClientFilter         = none.capabilities(capabilities)
    def maxAge(seconds: PosLong): ValkeyClientFilter                = none.maxAge(seconds)
    def idle(seconds: PosLong): ValkeyClientFilter                  = none.idle(seconds)
    def skipMe(skip: Boolean): ValkeyClientFilter                   = none.skipMe(skip)
    def notIds(ids: OneOrMore[NonNegLong]): ValkeyClientFilter      = none.notIds(ids)
    def notClientType(clientType: ClientType): ValkeyClientFilter   = none.notClientType(clientType)
    def notUser(username: String): ValkeyClientFilter               = none.notUser(username)
    def notAddress(host: Host, port: Port): ValkeyClientFilter      = none.notAddress(host, port)
    def notLocalAddress(host: Host, port: Port): ValkeyClientFilter = none.notLocalAddress(host, port)
    def notIp(host: Host): ValkeyClientFilter                       = none.notIp(host)
    def notName(name: ConnectionName): ValkeyClientFilter           = none.notName(name)
    def notFlags(flags: Key): ValkeyClientFilter                    = none.notFlags(flags)
    def notLibName(name: Key): ValkeyClientFilter                   = none.notLibName(name)
    def notLibVersion(version: Key): ValkeyClientFilter             = none.notLibVersion(version)
    def notDb(index: DbIndex): ValkeyClientFilter                   = none.notDb(index)
    def notCapabilities(capabilities: Key): ValkeyClientFilter      = none.notCapabilities(capabilities)
  }

  /** NOW, FORCE, SAFE and FAILOVER, for SHUTDOWN on Valkey 9.0: the first two as in [[ShutdownOptions]], then have the server refuse a
    * shutdown it considers unsafe, and have a cluster primary fail over to a replica first, which needs cluster mode
    */
  final class ValkeyShutdownOptions private (val params: List[String]) {
    def now: ValkeyShutdownOptions      = new ValkeyShutdownOptions(params :+ "NOW")
    def force: ValkeyShutdownOptions    = new ValkeyShutdownOptions(params :+ "FORCE")
    def safe: ValkeyShutdownOptions     = new ValkeyShutdownOptions(params :+ "SAFE")
    def failover: ValkeyShutdownOptions = new ValkeyShutdownOptions(params :+ "FAILOVER")
  }
  object ValkeyShutdownOptions {
    final val now: ValkeyShutdownOptions      = new ValkeyShutdownOptions(List("NOW"))
    final val force: ValkeyShutdownOptions    = new ValkeyShutdownOptions(List("FORCE"))
    final val safe: ValkeyShutdownOptions     = new ValkeyShutdownOptions(List("SAFE"))
    final val failover: ValkeyShutdownOptions = new ValkeyShutdownOptions(List("FAILOVER"))
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

  /** NOW or FORCE, for Dragonfly's SHUTDOWN, which takes one of them and nothing else */
  final class DragonflyShutdownOption private (val params: List[String])
  trait DragonflyShutdownOptionBuilders {
    def now: DragonflyShutdownOption
    def force: DragonflyShutdownOption
  }
  private[laserdisc] object DragonflyShutdownOption extends DragonflyShutdownOptionBuilders {
    final val now: DragonflyShutdownOption   = new DragonflyShutdownOption(List("NOW"))
    final val force: DragonflyShutdownOption = new DragonflyShutdownOption(List("FORCE"))
  }

  /** What CLIENT INFO replies: the same fields as a line of CLIENT LIST, for the client sending it */
  final case class ClientInfo(parameters: Parameters)
  object ClientInfo {
    implicit val clientInfoRead: Read[Bulk, ClientInfo] = ConnectedClients.connectedClientsRead.flatMap {
      case ConnectedClients(Seq(parameters)) => Read.const(ClientInfo(parameters))
      case other => Read.instance(_ => Left(RESPDecErr(s"Expected the information of one client, but was $other")))
    }
  }

  /** The builders every server takes */
  trait ServerTypes {
    final val clientType = ClientType
    final val flag       = ShutdownFlag
    final val flushMode  = ClusterManagement.FlushMode
    final val info       = InfoSection
    final val pauseMode  = PauseMode
  }

  /** The builders of Redis 6.2's CLIENT and ACL */
  trait ServerBaseTypes extends ServerTypes {
    final val aclRules        = Acl.AclRules.none
    final val clientFilter    = ClientFilter
    final val trackingOptions = TrackingOptions
    final val unblockMode     = UnblockMode
  }
  trait AclSelectorTypes  { final val aclSelectorRules: Acl.AclRulesWithSelectors = Acl.AclRulesWithSelectors.none              }
  trait AclDatabaseTypes  { final val aclDatabaseRules: Acl.AclRulesWithDatabases = Acl.AclRulesWithDatabases.none              }
  trait AclDragonflyTypes { final val aclDragonflyRules: Acl.AclDragonflyRules = Acl.AclDragonflyRules.none                     }
  trait CommandListTypes  { final val commandFilter: ServerInspection.CommandFilterBuilders = ServerInspection.CommandFilter    }
  trait CommandLogTypes   { final val commandLogType: ServerInspection.CommandLogTypeBuilders = ServerInspection.CommandLogType }
  trait HotKeysTypes      {
    final val hotKeysMetrics: ServerInspection.HotKeysMetricsBuilders = ServerInspection.HotKeysMetrics
    final val hotKeysOptions: ServerInspection.HotKeysOptionsBuilders = ServerInspection.HotKeysOptions
  }
  trait ShutdownTypes          { final val shutdownOptions = ShutdownOptions             }
  trait ValkeyShutdownTypes    { final val valkeyShutdownOptions = ValkeyShutdownOptions }
  trait DragonflyShutdownTypes {
    final val dragonflyShutdownOption: DragonflyShutdownOptionBuilders = DragonflyShutdownOption
  }
  trait ValkeyFilterTypes { final val valkeyClientFilter = ValkeyClientFilter }

  /** Every builder, for every server */
  trait AllTypes
      extends ServerBaseTypes
      with AclSelectorTypes
      with AclDatabaseTypes
      with AclDragonflyTypes
      with CommandListTypes
      with CommandLogTypes
      with HotKeysTypes
      with ShutdownTypes
      with ValkeyShutdownTypes
      with DragonflyShutdownTypes
      with ValkeyFilterTypes
}

/** CLIENT without UNBLOCK and CLIENT KILL's filters: Redis 6.2 */
trait ClientCoreCommands extends Features {
  import ServerP.{ClientInfo, ClientType, ConnectedClients, PauseMode}
  import Show.{hostShow, portShow}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "CLIENT GETNAME",
      "CLIENT HELP",
      "CLIENT ID",
      "CLIENT INFO",
      "CLIENT KILL",
      "CLIENT LIST: ID MASTER NORMAL PUBSUB REPLICA TYPE",
      "CLIENT PAUSE: ALL WRITE",
      "CLIENT SETNAME",
      "CLIENT UNPAUSE"
    )

  val help: Single[Seq[String]] = Protocol.help("CLIENT")

  val getname: Single[Option[ConnectionName]] = Protocol("CLIENT", "GETNAME").as[OrNullBulk[Bulk], Option[ConnectionName]]

  val id: Single[NonNegLong] = Protocol("CLIENT", "ID").as[Num, NonNegLong]

  val info: Single[ClientInfo] = Protocol("CLIENT", "INFO").as[Bulk, ClientInfo]

  def kill(host: Host, port: Port): Single[OK] =
    Protocol("CLIENT", "KILL" :: s"${hostShow.show(host)}:${portShow.show(port)}" :: Nil).as[Str, OK]

  val list: Single[ConnectedClients]                         = Protocol("CLIENT", "LIST").as[Bulk, ConnectedClients]
  def list(clientType: ClientType): Single[ConnectedClients] =
    Protocol("CLIENT", "LIST" *: "TYPE" *: clientType *: EmptyTuple).as[Bulk, ConnectedClients]
  def list(ids: OneOrMore[NonNegLong]): Single[ConnectedClients] =
    Protocol("CLIENT", "LIST" *: "ID" *: ids.value *: EmptyTuple).as[Bulk, ConnectedClients]
  def list(id: NonNegLong, ids: NonNegLong*): Single[ConnectedClients] =
    list(OneOrMore.unsafeFrom(id :: ids.toList))

  def pause(milliseconds: PosLong): Single[OK]                  = Protocol("CLIENT", "PAUSE" *: milliseconds *: EmptyTuple).as[Str, OK]
  def pause(milliseconds: PosLong, mode: PauseMode): Single[OK] =
    Protocol("CLIENT", "PAUSE" *: milliseconds *: mode *: EmptyTuple).as[Str, OK]

  def setname(connectionName: ConnectionName): Single[OK] = Protocol("CLIENT", "SETNAME" :: connectionName.value :: Nil).as[Str, OK]

  val unpause: Single[OK] = Protocol("CLIENT", "UNPAUSE").as[Str, OK]

  val unsetname: Single[OK] = Protocol("CLIENT", "SETNAME" :: "" :: Nil).as[Str, OK]
}

/** CLIENT TRACKING, CACHING, GETREDIR and TRACKINGINFO: Redis 6.2. Laserdisc reads protocol 2, so the notices go to another connection,
  * with [[ServerP.TrackingOptions.redirect]]: open it with `RedisSubscriber.identified` and [[invalidations]]. Tracking belongs to the
  * connection, so it's gone after the client reconnects, and a cache kept from these notices can go stale without any notice saying so
  */
trait ClientTrackingCommands extends Features {
  import ServerP.{TrackingInfo, TrackingOptions}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "CLIENT CACHING: NO YES",
      "CLIENT GETREDIR",
      "CLIENT TRACKING: BCAST NOLOOP OFF ON OPTIN OPTOUT PREFIX REDIRECT",
      "CLIENT TRACKINGINFO"
    )

  /** Which keys changed, for the connection that CLIENT TRACKING redirects its notices to */
  val invalidations: Feed[Feed.Invalidation] = Feed.invalidations

  /** Asks for notices when the keys this connection reads change */
  def tracking(options: TrackingOptions): Single[OK] = Protocol("CLIENT", "TRACKING" :: "ON" :: options.params).as[Str, OK]

  val trackingoff: Single[OK] = Protocol("CLIENT", "TRACKING" :: "OFF" :: Nil).as[Str, OK]

  /** With OPTIN or OPTOUT, whether to track the keys of the next command. It only applies to the very next command on the connection, so
    * send them together in the same `send`, or another caller's may come in between
    */
  def caching(on: Boolean): Single[OK] = Protocol("CLIENT", "CACHING" :: (if (on) "YES" else "NO") :: Nil).as[Str, OK]

  /** The connection that gets this one's notices, or `None` when there isn't one */
  val getredir: Single[Option[NonNegLong]] = Protocol("CLIENT", "GETREDIR").using(
    RESPRead.instance(Read.instance[Num, Option[NonNegLong]] {
      case Num(-1L) | Num(0L) => Right(None)
      case other              => Read[Num, NonNegLong].read(other).map(Some(_))
    })
  )

  val trackinginfo: Single[TrackingInfo] = Protocol("CLIENT", "TRACKINGINFO").using(RESPRead.instance(ServerP.trackingInfo))
}

/** CLIENT: Redis 6.2 */
trait ClientCommands extends ClientCoreCommands with ClientTrackingCommands {
  import ServerP.{ClientFilter, UnblockMode}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "CLIENT KILL: ADDR ID LADDR MASTER NORMAL PUBSUB REPLICA SKIPME SLAVE TYPE USER",
      "CLIENT UNBLOCK: ERROR TIMEOUT"
    )

  /** Closes the clients that match every filter, and replies how many it closed */
  def kill(filter: ClientFilter): Single[NonNegLong] = Protocol("CLIENT", "KILL" :: filter.params).as[Num, NonNegLong]

  /** Unblocks a client waiting in a blocking command, and replies whether there was one */
  def unblock(id: NonNegLong): Single[Boolean]                    = Protocol("CLIENT", "UNBLOCK" *: id *: EmptyTuple).as[Num, Boolean]
  def unblock(id: NonNegLong, mode: UnblockMode): Single[Boolean] =
    Protocol("CLIENT", "UNBLOCK" *: id *: mode *: EmptyTuple).as[Num, Boolean]
}

/** CLIENT NO-EVICT: Redis 7.0 */
trait ClientNoEvictCommands extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLIENT NO-EVICT: OFF ON")

  /** Whether this connection is spared when the server evicts clients to free memory */
  def noevict(on: Boolean): Single[OK] = Protocol("CLIENT", "NO-EVICT" :: (if (on) "ON" else "OFF") :: Nil).as[Str, OK]
}

/** CLIENT SETINFO: Redis 7.2 */
trait ClientSetInfoCommands extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLIENT SETINFO: LIB-NAME LIB-VER")

  /** The name of the library this connection uses, shown by CLIENT LIST */
  def setlibname(name: Key): Single[OK] = Protocol("CLIENT", "SETINFO" :: "LIB-NAME" :: name.value :: Nil).as[Str, OK]

  /** The version of the library this connection uses, shown by CLIENT LIST */
  def setlibversion(version: Key): Single[OK] = Protocol("CLIENT", "SETINFO" :: "LIB-VER" :: version.value :: Nil).as[Str, OK]
}

/** CLIENT NO-TOUCH, with CLIENT SETINFO: Redis 7.2 */
trait ClientNoTouchCommands extends ClientSetInfoCommands {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLIENT KILL: NO YES", "CLIENT NO-TOUCH: OFF ON")

  /** Whether this connection's commands leave the keys' last access time as it was */
  def notouch(on: Boolean): Single[OK] = Protocol("CLIENT", "NO-TOUCH" :: (if (on) "ON" else "OFF") :: Nil).as[Str, OK]
}

/** CLIENT CAPA, and the primary client type for CLIENT KILL and CLIENT LIST: Valkey 8.0 */
trait ClientCapaCommands extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLIENT CAPA", "CLIENT KILL: PRIMARY")

  /** Valkey's name for `master`, for wherever a client type is asked for */
  val primary: ServerP.ClientType = ServerP.ClientType.primary

  /** Tells the server what this connection can handle, like `redirect`. A server takes names it doesn't know, and ignores them */
  def capa(capabilities: OneOrMore[Key]): Single[OK]        = Protocol("CLIENT", "CAPA" :: capabilities.value.map(_.value)).as[Str, OK]
  def capa(capability: Key, capabilities: Key*): Single[OK] =
    capa(OneOrMore.unsafeFrom(capability :: capabilities.toList))
}

/** CLIENT IMPORT-SOURCE: Valkey 8.1 */
trait ClientImportSourceCommands extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLIENT IMPORT-SOURCE: OFF ON")

  /** Whether this connection is an import source. Turning it on needs the server to be in import mode */
  def importsource(on: Boolean): Single[OK] = Protocol("CLIENT", "IMPORT-SOURCE" :: (if (on) "ON" else "OFF") :: Nil).as[Str, OK]
}

/** CLIENT KILL with several ids, and CLIENT LIST with the filters of CLIENT KILL: Valkey 8.1 */
trait ClientListFilterCommands extends Features {
  import ServerP.{ClientFilter, ConnectedClients}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("CLIENT LIST: ADDR LADDR MAXAGE NO SKIPME USER YES")

  /** A filter for any of these clients, to go on from with the others */
  def ids(ids: OneOrMore[NonNegLong]): ClientFilter = ClientFilter.ids(ids)

  /** The clients that match every filter */
  def list(filter: ClientFilter): Single[ConnectedClients] = Protocol("CLIENT", "LIST" :: filter.params).as[Bulk, ConnectedClients]

  /** The clients that match every filter and have been connected for longer than `seconds` */
  def list(filter: ClientFilter, seconds: PosLong): Single[ConnectedClients] =
    Protocol("CLIENT", "LIST" *: filter.params *: "MAXAGE" *: seconds *: EmptyTuple).as[Bulk, ConnectedClients]
}

/** CLIENT KILL and CLIENT LIST with Valkey's own filters: Valkey 9.0 */
trait ClientValkeyFilterCommands extends Features {
  import ServerP.{ConnectedClients, ValkeyClientFilter}

  private final val keywords = "CAPA DB FLAGS IDLE IP LIB-NAME LIB-VER NAME NOT-ADDR NOT-CAPA NOT-DB NOT-FLAGS NOT-ID NOT-IP NOT-LADDR " +
    "NOT-LIB-NAME NOT-LIB-VER NOT-NAME NOT-TYPE NOT-USER"

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(s"CLIENT KILL: $keywords", s"CLIENT LIST: $keywords")

  /** Closes the clients that match every filter, and replies how many it closed */
  def kill(filter: ValkeyClientFilter): Single[NonNegLong] = Protocol("CLIENT", "KILL" :: filter.params).as[Num, NonNegLong]

  /** The clients that match every filter */
  def list(filter: ValkeyClientFilter): Single[ConnectedClients] =
    Protocol("CLIENT", "LIST" :: filter.params).as[Bulk, ConnectedClients]
}

/** CLIENT KILL with MAXAGE: Redis 7.4, Valkey 8.0 */
trait ClientMaxAgeCommands extends Features {
  import ServerP.ClientFilter

  override private[laserdisc] def supported: List[String] = super.supported ++ List("CLIENT KILL: MAXAGE")

  /** Closes the clients that match every filter and have been connected for longer than `seconds` */
  def kill(filter: ClientFilter, seconds: PosLong): Single[NonNegLong] =
    Protocol("CLIENT", "KILL" *: filter.params *: "MAXAGE" *: seconds *: EmptyTuple).as[Num, NonNegLong]
}

/** CONFIG: Redis 6.2 */
trait ConfigCommands extends Features {
  import ServerP.Configuration

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("CONFIG GET", "CONFIG HELP", "CONFIG RESETSTAT", "CONFIG REWRITE", "CONFIG SET")

  val help: Single[Seq[String]] = Protocol.help("CONFIG")

  def get(parameter: GlobPattern): Single[Configuration] =
    Protocol("CONFIG", "GET" :: parameter.value :: Nil).as[Arr.OfPairs[Bulk, Bulk], Configuration]

  val resetstat: Single[OK] = Protocol("CONFIG", "RESETSTAT").as[Str, OK]

  val rewrite: Single[OK] = Protocol("CONFIG", "REWRITE").as[Str, OK]

  def set[A: Show](parameter: Key, value: A): Single[OK] = Protocol("CONFIG", "SET" *: parameter *: value *: EmptyTuple).as[Str, OK]
}

/** CONFIG GET and CONFIG SET with several parameters: Redis 7.0 */
trait ConfigManyCommands extends Features {
  import ServerP.Configuration

  def get(parameters: OneOrMore[GlobPattern]): Single[Configuration] =
    Protocol("CONFIG", "GET" :: parameters.value.map(_.value)).as[Arr.OfPairs[Bulk, Bulk], Configuration]
  def get(parameter1: GlobPattern, parameter2: GlobPattern, parameters: GlobPattern*): Single[Configuration] =
    get(OneOrMore.unsafeFrom(parameter1 :: parameter2 :: parameters.toList))

  /** Sets every parameter, or none of them if one can't be set */
  def set[A: Show](parameters: OneOrMore[(Key, A)]): Single[OK] =
    Protocol("CONFIG", "SET" *: parameters.value *: EmptyTuple).as[Str, OK]
}

trait ServerCoreP extends ServerInspectionCoreP with MonitorP {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "BGSAVE: SCHEDULE",
      "DBSIZE",
      "FLUSHALL: ASYNC SYNC",
      "FLUSHDB: ASYNC SYNC",
      "INFO",
      "LASTSAVE",
      "REPLICAOF: NO ONE",
      "SAVE",
      "SHUTDOWN: NOSAVE SAVE",
      "SLAVEOF: NO ONE",
      "TIME"
    )

  import ServerP.{Info, InfoSection, ShutdownFlag}

  /** The builders of this server's commands */
  val servertypes: ServerP.ServerTypes = new ServerP.ServerTypes {}

  /** Replies with the server's message, like "Background saving started" */
  final val bgsave: Single[String] = Protocol("BGSAVE", EmptyTuple).as[Str, String]

  /** BGSAVE that waits for a rewrite of the append only file in progress instead of failing */
  final val bgsaveschedule: Single[String] = Protocol("BGSAVE", "SCHEDULE").as[Str, String]

  final val dbsize: Single[NonNegLong] = Protocol("DBSIZE", EmptyTuple).as[Num, NonNegLong]

  /** FAILOVER to that replica even if it hasn't caught up when the time is up */
  final def replicafailoverforce(host: Host, port: Port, milliseconds: PosLong): Single[OK] =
    Protocol("FAILOVER", "TO" *: host *: port *: "FORCE" *: "TIMEOUT" *: milliseconds *: EmptyTuple).as[Str, OK]

  /** Stops a FAILOVER in progress */
  final val replicafailoverabort: Single[OK] = Protocol("FAILOVER", "ABORT").as[Str, OK]

  final val flushall: Single[OK] = Protocol("FLUSHALL", EmptyTuple).as[Str, OK]

  final val flushallasync: Single[OK] = Protocol("FLUSHALL", "ASYNC").as[Str, OK]

  final val flushallsync: Single[OK] = Protocol("FLUSHALL", "SYNC").as[Str, OK]

  final val flushdb: Single[OK] = Protocol("FLUSHDB", EmptyTuple).as[Str, OK]

  final val flushdbasync: Single[OK] = Protocol("FLUSHDB", "ASYNC").as[Str, OK]

  final val flushdbsync: Single[OK] = Protocol("FLUSHDB", "SYNC").as[Str, OK]

  final val info: Single[Info]                       = Protocol("INFO", EmptyTuple).as[Bulk, Info]
  final def info(section: InfoSection): Single[Info] = Protocol("INFO", section).as[Bulk, Info]

  final val lastsave: Single[NonNegLong] = Protocol("LASTSAVE", EmptyTuple).as[Num, NonNegLong]

  final def replicaof(host: Host, port: Port): Single[OK] = Protocol("REPLICAOF", host *: port *: EmptyTuple).as[Str, OK]

  final val replicaofnoone: Single[OK] = Protocol("REPLICAOF", "NO" :: "ONE" :: Nil).as[Str, OK]

  final val save: Single[OK] = Protocol("SAVE", EmptyTuple).as[Str, OK]

  final val shutdown: Single[OK]                     = Protocol("SHUTDOWN", EmptyTuple).as[Str, OK]
  final def shutdown(flag: ShutdownFlag): Single[OK] = Protocol("SHUTDOWN", flag).as[Str, OK]

  final def slaveof(host: Host, port: Port): Single[OK] = Protocol("SLAVEOF", host *: port *: EmptyTuple).as[Str, OK]

  final val slaveofnoone: Single[OK] = Protocol("SLAVEOF", "NO" :: "ONE" :: Nil).as[Str, OK]

  final val time: Single[Time] =
    Protocol("TIME", EmptyTuple)
      .as[OneOf[Arr.AsTuple[Bulk *: Bulk *: EmptyTuple] *: Arr.AsTuple[Num *: Num *: EmptyTuple] *: EmptyTuple], Time]
}

/** LOLWUT, BGREWRITEAOF, FAILOVER and ROLE, and CLIENT, CONFIG, LATENCY, COMMAND and ACL under their names. Redis 6.2 */
trait ServerBaseP extends ServerCoreP with ServerInspectionBaseP {
  import ServerP.Role

  override val servertypes: ServerP.ServerBaseTypes = new ServerP.ServerBaseTypes {}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "BGREWRITEAOF",
      "FAILOVER: ABORT FORCE TIMEOUT TO",
      "LOLWUT: VERSION",
      "ROLE"
    ) ++ client.supported ++ config.supported ++ latency.supported ++ command.supported ++ acl.supported

  final val role: Single[Role] = Protocol("ROLE", EmptyTuple).using(RESPRead.instance(Role.roleRead))

  /** A piece of art made by the server, and its version, as text. It's in every server below but KeyDB */
  final val lolwut: Single[String]                  = Protocol("LOLWUT", EmptyTuple).as[Bulk, String]
  final def lolwut(version: PosInt): Single[String] = Protocol("LOLWUT", "VERSION" *: version *: EmptyTuple).as[Bulk, String]

  lazy val client: ClientCommands = new ClientCommands {}

  lazy val config: ConfigCommands = new ConfigCommands {}

  lazy val latency: LatencyCommands = new LatencyCommands {}

  lazy val command: CommandCommands = new CommandCommands {}

  lazy val acl: AclCommands = new AclCommands {}

  /** Replies with the server's message, like "Background append only file rewriting started" */
  final val bgrewriteaof: Single[String] = Protocol("BGREWRITEAOF", EmptyTuple).as[Str, String]

  /** FAILOVER: hands the primary role to a replica that has caught up, outside a cluster (CLUSTER FAILOVER is `failover`) */
  final val replicafailover: Single[OK]                        = Protocol("FAILOVER", EmptyTuple).as[Str, OK]
  final def replicafailover(milliseconds: PosLong): Single[OK] =
    Protocol("FAILOVER", "TIMEOUT" *: milliseconds *: EmptyTuple).as[Str, OK]
  final def replicafailover(host: Host, port: Port): Single[OK] = Protocol("FAILOVER", "TO" *: host *: port *: EmptyTuple).as[Str, OK]
  final def replicafailover(host: Host, port: Port, milliseconds: PosLong): Single[OK] =
    Protocol("FAILOVER", "TO" *: host *: port *: "TIMEOUT" *: milliseconds *: EmptyTuple).as[Str, OK]
}

/** SHUTDOWN with SAFE and FAILOVER: Valkey 9.0 */
trait ServerShutdownSafeP extends Features {
  import ServerP.{ShutdownFlag, ValkeyShutdownOptions}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SHUTDOWN: FAILOVER SAFE")

  final def shutdown(options: ValkeyShutdownOptions): Single[OK]                     = Protocol("SHUTDOWN", options.params).as[Str, OK]
  final def shutdown(flag: ShutdownFlag, options: ValkeyShutdownOptions): Single[OK] =
    Protocol("SHUTDOWN", flag *: options.params *: EmptyTuple).as[Str, OK]
}

/** SHUTDOWN with NOW and FORCE: Redis 7.0 */
trait ServerShutdownOptionsP extends Features {
  import ServerP.{ShutdownFlag, ShutdownOptions}

  override private[laserdisc] def supported: List[String] = super.supported ++ List("SHUTDOWN: FORCE NOW")

  final def shutdown(options: ShutdownOptions): Single[OK]                     = Protocol("SHUTDOWN", options.params).as[Str, OK]
  final def shutdown(flag: ShutdownFlag, options: ShutdownOptions): Single[OK] =
    Protocol("SHUTDOWN", flag *: options.params *: EmptyTuple).as[Str, OK]
}

/** SHUTDOWN with NOW or FORCE, one of them only: Dragonfly */
trait ServerShutdownDragonflyP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("SHUTDOWN: FORCE NOW")

  final def shutdown(option: ServerP.DragonflyShutdownOption): Single[OK] = Protocol("SHUTDOWN", option.params).as[Str, OK]
}

/** INFO with several sections, and INFO LATENCYSTATS: Redis 7.0 */
trait ServerInfoManyP extends Features {
  import ServerP.{Info, InfoSection, LatencyStatsSection}

  final def info(sections: OneOrMore[InfoSection]): Single[Info] = Protocol("INFO", sections.value).as[Bulk, Info]
  final def info(section1: InfoSection, section2: InfoSection, sections: InfoSection*): Single[Info] =
    info(OneOrMore.unsafeFrom(section1 :: section2 :: sections.toList))
  final def info(section: LatencyStatsSection): Single[Info] = Protocol("INFO", section).as[Bulk, Info]
}

/** SHUTDOWN with NOW, FORCE and ABORT, and INFO with several sections: Redis 7.0 */
trait ServerShutdownAndInfoP extends ServerShutdownOptionsP with ServerInfoManyP {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("SHUTDOWN: ABORT")

  /** Stops a shutdown that is waiting for replicas to catch up */
  final val shutdownabort: Single[OK] = Protocol("SHUTDOWN", "ABORT").as[Str, OK]
}

/** INFO THREADS and INFO KEYSIZES: Redis 8.0 */
trait ServerInfoRedis8P extends Features {
  import ServerP.{Info, Redis8Section}

  final def info(section: Redis8Section): Single[Info] = Protocol("INFO", section).as[Bulk, Info]
}

/** HOTKEYS and INFO HOTKEYS: Redis 8.6 */
trait ServerHotKeysP extends Features {
  import ServerP.{HotKeysSection, Info}

  override private[laserdisc] def supported: List[String] = super.supported ++ hotkeys.supported

  object hotkeys extends HotKeysCommands

  final def info(section: HotKeysSection): Single[Info] = Protocol("INFO", section).as[Bulk, Info]
}

/** COMMANDLOG: Valkey 8.1 */
trait ServerCommandLogP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ commandlog.supported

  object commandlog extends CommandLogCommands
}

/** INFO TLS: Valkey 9.1 */
trait ServerInfoTlsP extends Features {
  import ServerP.{Info, TlsSection}

  final def info(section: TlsSection): Single[Info] = Protocol("INFO", section).as[Bulk, Info]
}

/** BGSAVE CANCEL: Valkey 8.1 */
trait ServerBgSaveCancelP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("BGSAVE: CANCEL")

  /** Stops a BGSAVE in progress, or one scheduled with BGSAVE SCHEDULE */
  final val bgsavecancel: Single[String] = Protocol("BGSAVE", "CANCEL").as[Str, String]
}

trait ClientAllCommands
    extends ClientCommands
    with ClientNoEvictCommands
    with ClientNoTouchCommands
    with ClientMaxAgeCommands
    with ClientCapaCommands
    with ClientImportSourceCommands
    with ClientListFilterCommands
    with ClientValkeyFilterCommands
trait ConfigAllCommands  extends ConfigCommands with ConfigManyCommands
trait LatencyAllCommands extends LatencyCommands with LatencyHistogramCommands
trait CommandAllCommands extends CommandCommands with CommandListCommands with CommandInfo7_0Commands
trait ModuleAllCommands  extends ModuleCommands with ModuleLoadExCommands with ModuleList7_0Commands
trait AclAllCommands     extends AclCommands with AclDatabaseCommands with AclLog7_2Commands

trait ServerP
    extends ServerBaseP
    with ServerShutdownAndInfoP
    with ServerShutdownSafeP
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

  override val servertypes: ServerP.AllTypes = new ServerP.AllTypes {}

  override val slowlog: SlowLogCommands8_10 = new SlowLogCommands8_10 {}
}
