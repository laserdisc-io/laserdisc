package laserdisc
package protocol

object ServerP {
  import scala.language.dynamics

  sealed trait InfoSection
  final object InfoSection {
    final type default = default.type

    final object server       extends InfoSection
    final object clients      extends InfoSection
    final object memory       extends InfoSection
    final object persistence  extends InfoSection
    final object stats        extends InfoSection
    final object replication  extends InfoSection
    final object cpu          extends InfoSection
    final object commandstats extends InfoSection
    final object cluster      extends InfoSection
    final object keyspace     extends InfoSection
    final object all          extends InfoSection
    final object default      extends InfoSection

    implicit val defaultShow: Show[default] = Show.const("default")
    implicit val infoSectionShow: Show[InfoSection] = Show.instance {
      case `server`       => "server"
      case `clients`      => "clients"
      case `memory`       => "memory"
      case `persistence`  => "persistence"
      case `stats`        => "stats"
      case `replication`  => "replication"
      case `cpu`          => "cpu"
      case `commandstats` => "commandstats"
      case `cluster`      => "cluster"
      case `keyspace`     => "keyspace"
      case `all`          => "all"
      case `default`      => "default"
    }
  }

  sealed trait ShutdownFlag
  final object ShutdownFlag {
    final object nosave extends ShutdownFlag
    final object save   extends ShutdownFlag

    implicit val shutdownFlagShow: Show[ShutdownFlag] = Show.instance {
      case `nosave` => "nosave"
      case `save`   => "save"
    }
  }

  sealed trait Role
  final object Role {
    final case class Client(host: Host, port: Port, lastAcknowledgedReplicationOffset: NonNegLong)

    sealed trait ReplicaStatus
    final case object connect    extends ReplicaStatus
    final case object connecting extends ReplicaStatus
    final case object sync       extends ReplicaStatus
    final case object connected  extends ReplicaStatus

    final case class Master(currentReplicationOffset: NonNegLong, clients: Seq[Client]) extends Role
    final case class Slave(masterHost: Host, masterPort: Port, masterViewReplicaStatus: ReplicaStatus, masterReplicationOffset: NonNegLong)
        extends Role
    final case class Sentinel(masterNames: Seq[Key]) extends Role

    implicit val roleRead: Arr ==> Role = {
      val CR: Arr ==> Client = Read.instancePF {
        case Arr(Bulk(Host(host)) +: Bulk(ToInt(Port(port))) +: Bulk(ToLong(NonNegLong(offset))) +: Seq()) => Client(host, port, offset)
      }
      val RSR: Bulk ==> ReplicaStatus = Read.instancePF {
        case Bulk("connect")    => connect
        case Bulk("connecting") => connecting
        case Bulk("sync")       => sync
        case Bulk("connected")  => connected
      }
      val MR: Arr ==> Seq[Key] = Read[Arr, Seq[Key]]

      Read.instance {
        case Arr(Bulk("master") +: Num(NonNegLong(offset)) +: Arr(v) +: Seq()) =>
          val (vLength, (clients, clientsLength)) = v.foldLeft(0 -> (List.empty[Client] -> 0)) {
            case ((vl, (cs, csl)), CR(Right(client))) => (vl + 1) -> ((client :: cs) -> (csl + 1))
            case ((vl, acc), _)                       => (vl + 1) -> acc
          }
          if (vLength == clientsLength) Right(Master(offset, clients.reverse)) else Left(Err("TODO: FILIPPO"))
        case Arr(
            Bulk("slave") +: Bulk(Host(host)) +: Num(ToInt(Port(port))) +: RSR(Right(replicaStatus)) +: Num(NonNegLong(offset)) +: Seq()
            ) =>
          Right(Slave(host, port, replicaStatus, offset))
        case Arr(Bulk("sentinel") +: MR(Right(masters)) +: Seq()) => Right(Sentinel(masters))
        case _                                                    => Left(Err("TODO: FILIPPO"))
      }
    }
  }

  final class Parameters(private val properties: Map[String, String]) extends AnyVal with Dynamic {
    def selectDynamic[A](field: String)(implicit R: String ==> A): Maybe[A] =
      properties
        .get(field)
        .toRight(Err(s"no key $field of the provided type found"))
        .flatMap(R.read)
        .widenLeft[Throwable]
  }

  final case class ConnectedClients(clients: Seq[Parameters])
  final object ConnectedClients {
    private val KVPair = "(.*)=(.*)".r

    implicit val connectedClientsRead: Bulk ==> ConnectedClients = Read.instancePF {
      case Bulk(s) =>
        ConnectedClients(
          s.split(LF_CH).toIndexedSeq.map { clientData =>
            new Parameters(clientData.split(SPACE_CH).collect { case KVPair(k, v) => k -> v }.toMap)
          }
        )
    }
  }

  final case class Configuration(parameters: Parameters)
  final object Configuration {
    implicit val configRead: Arr ==> Configuration =
      Read[Arr, Seq[(String, String)]].map(kvs => Configuration(new Parameters(kvs.toMap)))
  }

  final case class Info(sections: Map[InfoSection, Parameters])
  final object Info {
    private val ISR: String ==> InfoSection = Read.instancePF {
      case "server"       => InfoSection.server
      case "clients"      => InfoSection.clients
      case "memory"       => InfoSection.memory
      case "persistence"  => InfoSection.persistence
      case "stats"        => InfoSection.stats
      case "replication"  => InfoSection.replication
      case "cpu"          => InfoSection.cpu
      case "commandstats" => InfoSection.commandstats
      case "cluster"      => InfoSection.cluster
      case "keyspace"     => InfoSection.keyspace
    }
    private val KVPair = "(.*):(.*)".r
    private val PR: Seq[String] ==> Parameters = Read.instancePF {
      case ss => new Parameters(ss.collect { case KVPair(k, v) => k -> v }.toMap)
    }

    implicit val infoRead: Bulk ==> Info = Read.instancePF {
      case Bulk(s) =>
        Info(
          s.split(LF * 2)
            .flatMap {
              _.split(LF_CH).toSeq match {
                case ISR(Right(infoSection)) +: PR(Right(parameters)) => Some(infoSection -> parameters)
                case _                                                => None
              }
            }
            .toMap
        )
    }
  }
}

trait ServerP {
  import ServerP.{Configuration, ConnectedClients, Info, InfoSection, Role, ShutdownFlag}
  import shapeless._

  final object servers {
    final val info = InfoSection
    final val flag = ShutdownFlag
  }

  final val bgrewriteaof: Protocol.Aux[OK] = Protocol("BGREWRITEAOF", Nil).as[Str, OK]

  final val bgsave: Protocol.Aux[OK] = Protocol("BGSAVE", Nil).as[Str, OK]

  final object client {
    import Show.{hostShow, portShow}

    val getname: Protocol.Aux[Option[ConnectionName]] = Protocol("CLIENT", "GETNAME").opt[GenBulk].as[ConnectionName]

    //FIXME other variations of kill
    def kill(host: Host, port: Port): Protocol.Aux[OK] =
      Protocol("CLIENT", "KILL" :: s"${hostShow.show(host)}:${portShow.show(port)}" :: Nil).as[Str, OK]

    val list: Protocol.Aux[ConnectedClients] = Protocol("CLIENT", "LIST").as[Bulk, ConnectedClients]

    def pause(milliseconds: PosLong): Protocol.Aux[OK] = Protocol("CLIENT", "PAUSE" :: milliseconds :: HNil).as[Str, OK]

    def setname(connectionName: ConnectionName): Protocol.Aux[OK] = Protocol("CLIENT", "SETNAME" :: connectionName.value :: Nil).as[Str, OK]

    val unsetname: Protocol.Aux[OK] = Protocol("CLIENT", "SETNAME" :: "" :: Nil).as[Str, OK]
  }

  //TODO command?

  final object config {
    def get(parameter: GlobPattern): Protocol.Aux[Configuration] =
      Protocol("CONFIG", "GET" :: parameter.value :: Nil).as[Arr, Configuration]

    val resetstat: Protocol.Aux[OK] = Protocol("CONFIG", "RESETSTAT").as[Str, OK]

    val rewrite: Protocol.Aux[OK] = Protocol("CONFIG", "REWRITE").as[Str, OK]

    def set[A: Show](parameter: Key, value: A): Protocol.Aux[OK] = Protocol("CONFIG", "SET" :: parameter :: value :: HNil).as[Str, OK]
  }

  final val dbsize: Protocol.Aux[NonNegLong] = Protocol("DBSIZE", Nil).as[Num, NonNegLong]

  final val flushall: Protocol.Aux[OK] = Protocol("FLUSHALL", Nil).as[Str, OK]

  final val flushallasync: Protocol.Aux[OK] = Protocol("FLUSHALL", "ASYNC").as[Str, OK]

  final val flushdb: Protocol.Aux[OK] = Protocol("FLUSHDB", Nil).as[Str, OK]

  final val flushdbasync: Protocol.Aux[OK] = Protocol("FLUSHDB", "ASYNC").as[Str, OK]

  final val info: Protocol.Aux[Info]                       = info(servers.info.default)
  final def info(section: InfoSection): Protocol.Aux[Info] = Protocol("INFO", section).as[Bulk, Info]

  final val lastsave: Protocol.Aux[NonNegLong] = Protocol("LASTSAVE", Nil).as[Num, NonNegLong]

  final val role: Protocol.Aux[Role] = Protocol("ROLE", Nil).as[Arr, Role]

  final val save: Protocol.Aux[OK] = Protocol("SAVE", Nil).as[Str, OK]

  final val shutdown: Protocol.Aux[OK]                     = Protocol("SHUTDOWN", Nil).as[Str, OK]
  final def shutdown(flag: ShutdownFlag): Protocol.Aux[OK] = Protocol("SHUTDOWN", flag).as[Str, OK]

  final def slaveof(host: Host, port: Port): Protocol.Aux[OK] = Protocol("SLAVEOF", host :: port :: HNil).as[Str, OK]

  final val slaveofnoone: Protocol.Aux[OK] = Protocol("SLAVEOF", "NO" :: "ONE" :: Nil).as[Str, OK]

  //TODO slowlog? sync?

  final val time: Protocol.Aux[Time] = Protocol("TIME", Nil).as[Arr, Time]
}
