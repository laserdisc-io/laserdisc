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
      val CR: Arr ==> Client = Read.instance {
        case Arr(Bulk(Host(host)) +: Bulk(ToInt(Port(port))) +: Bulk(ToLong(NonNegLong(offset))) +: Seq()) =>
          Right(Client(host, port, offset))
        case Arr(other) => Left(RESPDecErr(s"Unexpected role: it should be [host, port, offset] but was $other"))
      }
      val RSR: Bulk ==> ReplicaStatus = Read.instance {
        case Bulk("connect")    => Right(connect)
        case Bulk("connecting") => Right(connecting)
        case Bulk("sync")       => Right(sync)
        case Bulk("connected")  => Right(connected)
        case Bulk(other)        => Left(RESPDecErr(s"Unexpected replica status. Was $other"))
      }
      val MR: Arr ==> Seq[Key] = Read[Arr, Seq[Key]]

      Read.instance {
        case Arr(Bulk("master") +: Num(NonNegLong(offset)) +: Arr(v) +: Seq()) =>
          v.foldRight[RESPDecErr | (List[Client], Int)](Right(Nil -> 0)) {
            case (CR(Right(client)), Right((cs, csl))) => Right((client :: cs) -> (csl + 1))
            case (CR(Left(e)), Right((_, csl)))        => Left(RESPDecErr(s"Arr ==> Role clients error at element ${csl + 1}: ${e.message}"))
            case (_, left)                             => left
          } map (r => Master(offset, r._1))
        case Arr(Bulk("slave") +: Bulk(Host(host)) +: Num(ToInt(Port(port))) +: RSR(Right(status)) +: Num(NonNegLong(offset)) +: Seq()) =>
          Right(Slave(host, port, status, offset))
        case Arr(Bulk("slave") +: Bulk(Host(_)) +: Num(ToInt(Port(_))) +: RSR(Left(e)) +: Num(NonNegLong(_)) +: Seq()) =>
          Left(RESPDecErr(s"Slave replica status read error: $e"))
        case Arr(Bulk("sentinel") +: MR(Right(masters)) +: Seq()) => Right(Sentinel(masters))
        case Arr(Bulk("sentinel") +: MR(Left(e)) +: Seq())        => Left(RESPDecErr(s"Sentinel masters read error: $e"))
        case other                                                => Left(RESPDecErr(s"Unexpected role encoding. Was $other"))
      }
    }
  }

  final class Parameters(private val properties: Map[String, String]) extends AnyVal with Dynamic {
    def selectDynamic[A](field: String)(implicit R: String ==> A): Maybe[A] =
      properties
        .get(field)
        .toRight(RESPDecErr(s"no key $field of the provided type found"))
        .flatMap(R.read)
        .widenLeft[Throwable]
  }

  final case class ConnectedClients(clients: Seq[Parameters])
  final object ConnectedClients {
    private val KVPair = "(.*)=(.*)".r

    implicit val connectedClientsRead: Bulk ==> ConnectedClients = Read.instance {
      case Bulk(s) =>
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
  final object Configuration {
    implicit val configRead: Arr ==> Configuration =
      Read[Arr, Seq[(String, String)]].map(kvs => Configuration(new Parameters(kvs.toMap)))
  }

  final case class Info(sections: Map[InfoSection, Parameters])
  final object Info {
    private val ISR: String ==> InfoSection = Read.instance {
      case "server"       => Right(InfoSection.server)
      case "clients"      => Right(InfoSection.clients)
      case "memory"       => Right(InfoSection.memory)
      case "persistence"  => Right(InfoSection.persistence)
      case "stats"        => Right(InfoSection.stats)
      case "replication"  => Right(InfoSection.replication)
      case "cpu"          => Right(InfoSection.cpu)
      case "commandstats" => Right(InfoSection.commandstats)
      case "cluster"      => Right(InfoSection.cluster)
      case "keyspace"     => Right(InfoSection.keyspace)
      case other          => Left(RESPDecErr(s"Unexpected server info specification. Was $other"))
    }
    private val PR: Seq[String] ==> Parameters =
      KVPS.map(kv => new Parameters(kv.toMap))

    private val IFI: String ==> (InfoSection, Parameters) = {
      _.split(LF_CH).toList match {
        case ISR(Right(infoSection)) :: PR(Right(parameters)) => Right(infoSection -> parameters)
        case ISR(Left(e)) :: _ =>
          Left(RESPDecErr(s"String ==> (InfoSection, Parameters), Error decoding server's info section. Error was: $e"))
        case _ :: PR(Left(e)) =>
          Left(RESPDecErr(s"String ==> (InfoSection, Parameters), Error decoding server's info section parameters. Error was: $e"))
        case other =>
          Left(RESPDecErr(s"Unexpected encoding for server's info section. Expected [info section, [parameter: value]] but was $other"))
      }
    }

    implicit val infoRead: Bulk ==> Info = Read.instance {
      case Bulk(s) =>
        s.split(LF * 2)
          .foldRight[RESPDecErr | (List[(InfoSection, Parameters)], Int)](Right(Nil -> 0)) {
            case (IFI(Right(infoSection)), Right((iss, isl))) => Right((infoSection :: iss) -> (isl + 1))
            case (IFI(Left(e)), Right((_, isl))) =>
              Left(RESPDecErr(s"Bulk ==> Info, Error decoding the server's info section at position ${isl + 1}. Error was: $e"))
            case (_, left) => left
          }
          .map(is => Info(is._1.toMap))
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
