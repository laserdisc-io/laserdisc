package laserdisc
package protocol

object ServerP {
  import Read.==>

  import scala.language.dynamics

  sealed trait InfoSection
  final object InfoSection {
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

    implicit val defaultShow: Show[default.type] = Show.const("default")
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
    final case class Slave(masterHost: Host,
                           masterPort: Port,
                           masterViewReplicationState: ReplicaStatus,
                           masterReplicationOffset: NonNegLong)
        extends Role
    final case class Sentinel(masterNames: Seq[Key]) extends Role

    implicit val roleRead: NonNilArray ==> Role = {
      val ClientRead: NonNilArray ==> Client = Read.instancePF {
        case NonNilArray(
            NonNullBulkString(Host(host)) +: NonNullBulkString(ToInt(Port(port))) +: NonNullBulkString(
              ToLong(NonNegLong(offset))) +: Seq()) =>
          Client(host, port, offset)
      }
      val ReplicaStatusRead: NonNullBulkString ==> ReplicaStatus = Read.instancePF {
        case NonNullBulkString("connect")    => connect
        case NonNullBulkString("connecting") => connecting
        case NonNullBulkString("sync")       => sync
        case NonNullBulkString("connected")  => connected
      }
      val MastersRead: NonNilArray ==> Seq[Key] = Read[NonNilArray, Seq[Key]]
      Read.instance {
        case NonNilArray(NonNullBulkString("master") +: Integer(NonNegLong(offset)) +: NonNilArray(v) +: Seq()) =>
          val (vLength, (clients, clientsLength)) = v.foldLeft(0 -> (List.empty[Client] -> 0)) {
            case ((vl, (cs, csl)), ClientRead(client)) => (vl + 1) -> ((client :: cs) -> (csl + 1))
            case ((vl, acc), _)                        => (vl + 1) -> acc
          }
          if (vLength == clientsLength) Some(Master(offset, clients.reverse)) else None
        case NonNilArray(
            NonNullBulkString("slave") +: NonNullBulkString(Host(host)) +: Integer(ToInt(Port(port))) +: ReplicaStatusRead(
              replicaStatus) +: Integer(NonNegLong(offset)) +: Seq()) =>
          Some(Slave(host, port, replicaStatus, offset))
        case NonNilArray(NonNullBulkString("sentinel") +: MastersRead(masters) +: Seq()) =>
          Some(Sentinel(masters))
        case _ => None
      }
    }
  }

  final class Parameters(private val properties: Map[String, String]) extends AnyVal with Dynamic {
    import RESP.err

    def selectDynamic[A](field: String)(
        implicit R: String ==> A
    ): Maybe[A] = properties.get(field).flatMap(R.read).toRight(err(s"no key $field of the provided type found"))
  }

  final case class ConnectedClients(clients: Seq[Parameters])
  final object ConnectedClients {
    private val KVPair = "(.*)=(.*)".r
    implicit val connectedClientsRead: NonNullBulkString ==> ConnectedClients = Read.instancePF {
      case NonNullBulkString(s) =>
        ConnectedClients {
          s.split('\n').map { clientData =>
            new Parameters(clientData.split(' ').collect { case KVPair(k, v) => k -> v }.toMap)
          }
        }
    }
  }

  final case class Configuration(parameters: Parameters)
  final object Configuration {
    implicit val configRead: NonNilArray ==> Configuration =
      Read[NonNilArray, Seq[(String, String)]].map(kvs => Configuration(new Parameters(kvs.toMap)))
  }

  final case class Info(sections: Map[InfoSection, Parameters])
  final object Info {
    private val InfoSectionRead: String ==> InfoSection = Read.instancePF {
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
    private val ParametersRead: Seq[String] ==> Parameters = Read.instancePF {
      case ss => new Parameters(ss.collect { case KVPair(k, v) => k -> v }.toMap)
    }

    implicit val infoRead: NonNullBulkString ==> Info = Read.instancePF {
      case NonNullBulkString(s) =>
        Info {
          s.split("\n\n")
            .flatMap(_.split("\n").toSeq match {
              case InfoSectionRead(infoSection) +: ParametersRead(parameters) => Some(infoSection -> parameters)
              case _                                                          => None
            })
            .toMap
        }
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

  final val bgrewriteaof: Protocol.Aux[OK] = Protocol("BGREWRITEAOF", Nil).as[SimpleString, OK]

  final val bgsave: Protocol.Aux[OK] = Protocol("BGSAVE", Nil).as[SimpleString, OK]

  final object client {
    val getname: Protocol.Aux[Option[ConnectionName]] =
      Protocol("CLIENT", "GETNAME").asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[ConnectionName]]

    //FIXME other variations of kill
    def kill(host: Host, port: Port): Protocol.Aux[OK] =
      Protocol("CLIENT", "KILL" :: s"${Show.hostShow.show(host)}:${Show.portShow.show(port)}" :: Nil)
        .as[SimpleString, OK]

    val list: Protocol.Aux[ConnectedClients] =
      Protocol("CLIENT", "LIST").as[NonNullBulkString, ConnectedClients]

    def pause(milliseconds: PosLong): Protocol.Aux[OK] =
      Protocol("CLIENT", "PAUSE" :: milliseconds :: HNil).as[SimpleString, OK]

    def setname(connectionName: ConnectionName): Protocol.Aux[OK] =
      Protocol("CLIENT", "SETNAME" :: connectionName.value :: Nil).as[SimpleString, OK]

    val unsetname: Protocol.Aux[OK] = Protocol("CLIENT", "SETNAME" :: "" :: Nil).as[SimpleString, OK]
  }

  //TODO command?

  final object config {
    def get(parameter: GlobPattern): Protocol.Aux[Configuration] =
      Protocol("CONFIG", "GET" :: parameter.value :: Nil).as[NonNilArray, Configuration]

    val resetstat: Protocol.Aux[OK] = Protocol("CONFIG", "RESETSTAT").as[SimpleString, OK]

    val rewrite: Protocol.Aux[OK] = Protocol("CONFIG", "REWRITE").as[SimpleString, OK]

    def set[A: Show](parameter: Key, value: A): Protocol.Aux[OK] =
      Protocol("CONFIG", "SET" :: parameter :: value :: HNil).as[SimpleString, OK]
  }

  final val dbsize: Protocol.Aux[NonNegLong] = Protocol("DBSIZE", Nil).as[Integer, NonNegLong]

  final val flushall: Protocol.Aux[OK] = Protocol("FLUSHALL", Nil).as[SimpleString, OK]

  final val flushallasync: Protocol.Aux[OK] = Protocol("FLUSHALL", "ASYNC").as[SimpleString, OK]

  final val flushdb: Protocol.Aux[OK] = Protocol("FLUSHDB", Nil).as[SimpleString, OK]

  final val flushdbasync: Protocol.Aux[OK] = Protocol("FLUSHDB", "ASYNC").as[SimpleString, OK]

  final val info: Protocol.Aux[Info] = info(servers.info.default)

  final def info(section: InfoSection): Protocol.Aux[Info] = Protocol("INFO", section).as[NonNullBulkString, Info]

  final val lastsave: Protocol.Aux[NonNegLong] = Protocol("LASTSAVE", Nil).as[Integer, NonNegLong]

  final val role: Protocol.Aux[Role] = Protocol("ROLE", Nil).as[NonNilArray, Role]

  final val save: Protocol.Aux[OK] = Protocol("SAVE", Nil).as[SimpleString, OK]

  final val shutdown: Protocol.Aux[OK] = Protocol("SHUTDOWN", Nil).as[SimpleString, OK]

  final def shutdown(flag: ShutdownFlag): Protocol.Aux[OK] = Protocol("SHUTDOWN", flag).as[SimpleString, OK]

  final def slaveof(host: Host, port: Port): Protocol.Aux[OK] =
    Protocol("SLAVEOF", host :: port :: HNil).as[SimpleString, OK]

  final val slaveofnoone: Protocol.Aux[OK] = Protocol("SLAVEOF", "NO" :: "ONE" :: Nil).as[SimpleString, OK]

  //TODO slowlog? sync?

  final val time: Protocol.Aux[Time] = Protocol("TIME", Nil).as[NonNilArray, Time]
}
