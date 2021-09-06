package laserdisc
package protocol

import org.scalacheck.Prop.forAll

final class ClusterPSpec extends BaseSpec with ClusterP {
  import clustertypes._
  import org.scalacheck.{Arbitrary, Gen}
  import org.scalacheck.Gen._

  private[this] implicit val clusterFailoverModeArb: Arbitrary[ClusterFailoverMode] = Arbitrary {
    Gen.oneOf(ClusterFailoverMode.force, ClusterFailoverMode.takeover)
  }
  private[this] implicit val clusterResetModeArb: Arbitrary[ClusterResetMode] = Arbitrary {
    Gen.oneOf(ClusterResetMode.hard, ClusterResetMode.soft)
  }
  private[this] implicit val clusterSetSlotModeArb: Arbitrary[ClusterSetSlotMode] = Arbitrary {
    Gen.oneOf(ClusterSetSlotMode.importing, ClusterSetSlotMode.migrating, ClusterSetSlotMode.node)
  }

  private[this] final val kvPairPattern                         = KVPairRegex.pattern
  private[this] val infoIsValid: Map[String, String] => Boolean = _.forall { case (k, v) => kvPairPattern.matcher(s"$k:$v").matches }
  private[this] val infoGen: Gen[Map[String, String]] =
    nonEmptyMap(identifier.flatMap(k => alphaStr.map(k -> _))).filter(infoIsValid) :| "info map"
  private[this] implicit val infoShow: Show[Map[String, String]] = Show.instance {
    _.map { case (k, v) => s"$k:$v" }.mkString(CRLF)
  }

  private[this] type RawNode =
    (String, EmptyString | Host, Port, Option[Port], Seq[String], String, Int, Int, Int, String, Seq[String])
  private[this] type RawNodes = List[RawNode]
  private[this] val rawNodeToString: RawNode => String = {
    case (nid, h, p, None, fs, mnid, ps, pr, ce, l, ss) =>
      s"$nid ${h.fold(_.value, _.value)}:$p ${fs.mkString(COMMA)} $mnid $ps $pr $ce $l${ss.mkString(SPACE, SPACE, "")}"
    case (nid, h, p, Some(cp), fs, mnid, ps, pr, ce, l, ss) =>
      s"$nid ${h.fold(_.value, _.value)}:$p@$cp ${fs.mkString(COMMA)} $mnid $ps $pr $ce $l${ss.mkString(SPACE, SPACE, "")}"
  }
  private[this] val nodesIsValid: RawNodes => Boolean = {
    val nid    = NodeIdRegexWit.value
    val ip     = IPv4RegexWit.value
    val domain = Rfc1123HostnameRegexWit.value
    val flags  = raw"noflags|(myself|master|slave|fail\?|fail|handshake|noaddr)(,(myself|master|slave|fail\?|fail|handshake|noaddr))*"
    val link   = raw"(connected|disconnected)"
    val slots  = raw"(\d+|(\d+-\d+)|\[\d+-[><]-$nid\])?(\s(\d+|(\d+-\d+)|\[\d+-[><]-$nid\]))*"
    val R      = raw"^$nid\s($ip|$domain)?:(\d+)(@\d+)?\s($flags)\s(-|$nid)\s\d+\s\d+\s\d+\s$link\s$slots$$".r

    _.map(rawNodeToString).forall { case R(_*) => true; case _ => false }
  }
  private[this] val nodesGen: Gen[RawNodes] = {
    val flag = Gen.oneOf("myself", "master", "slave", "fail?", "fail", "handshake", "noaddr")
    val slotType = Gen.oneOf(
      slotGen.map(_.toString),
      slotGen.flatMap(f => slotGen.map(t => s"$f-$t")),
      slotGen.flatMap(s => nodeIdGen.map(in => s"[$s-<-$in]")),
      slotGen.flatMap(s => nodeIdGen.map(en => s"[$s->-$en]"))
    )
    val rawNode: Gen[RawNode] = for {
      nid   <- nodeIdGen
      h     <- hostOrEmptyGen
      p     <- portGen
      mcp   <- option(portGen)
      fs    <- Gen.oneOf(const("noflags").map(Seq(_)), choose(1, 7).flatMap(containerOfN[Set, String](_, flag).map(_.toSeq)))
      mmnid <- Gen.oneOf(const("-"), nodeIdGen)
      ps    <- nonNegIntGen
      pr    <- nonNegIntGen
      ce    <- nonNegIntGen
      l     <- Gen.oneOf("connected", "disconnected")
      ss    <- choose(1, 20).flatMap(listOfN(_, slotType))
    } yield (nid, h, p, mcp, fs, mmnid, ps, pr, ce, l, ss)

    choose(1, 10).flatMap(listOfN(_, rawNode)) :| "raw nodes info"
  }
  private[this] implicit val nodesShow: Show[RawNodes] = Show.instance {
    _.map(rawNodeToString).mkString(LF)
  }
  private[this] val validateNodes: RawNodes => ClusterNodes => Unit =
    rns =>
      ns =>
        ns.nodes.zip(rns).foreach {
          case (ClusterNode(n0, ClusterAddress(h0, p0, cp0), fs0, m0, ps0, pr0, ce0, l0, ss0), (n, h, p, cp, fs, m, ps, pr, ce, l, ss)) =>
            assertEquals(n0.value, n)
            assertEquals(h0, h.fold(_ => LoopbackHost, identity))
            assertEquals(p0, p)
            assertEquals(cp0, cp.getOrElse(p))
            assertEquals(
              if (fs0.isEmpty) Seq("noflags") else fs0.map(Show[ClusterFlag].show),
              fs
            )
            assertEquals(m0.fold("-")(_.value), m)
            assertEquals(ps0.value, ps)
            assertEquals(pr0.value, pr)
            assertEquals(ce0.value, ce)
            assertEquals(Show[ClusterLinkState].show(l0), l)
            assertEquals(
              ss0.map {
                case ClusterSingleSlotType(s)        => s.toString
                case ClusterRangeSlotType(f, t)      => s"$f-$t"
                case ClusterImportingSlotType(s, in) => s"[$s-<-$in]"
                case ClusterMigratingSlotType(s, mn) => s"[$s->-$mn]"
              },
              ss
            )
        }

  private[this] type RawSlot  = (Slot, Slot, Seq[(EmptyString | Host, Port, Option[NodeId])])
  private[this] type RawSlots = List[RawSlot]
  private[this] val slotsGen: Gen[RawSlots] = (for {
    isOld <- Gen.oneOf(true, false)
    rss <- nonEmptyListOf(
      for {
        fst <- slotGen
        snd <- slotGen
        mrs <- nonEmptyListOf(for {
          h    <- hostOrEmptyGen
          p    <- portGen
          mrid <- if (isOld) const(None) else nodeIdGen.map(s => Some(NodeId.unsafeFrom(s)))
        } yield (h, p, mrid))
      } yield
        if (fst.value < snd.value) (fst, snd, mrs)
        else (snd, fst, mrs)
    )
  } yield rss) :| "raw slots info"
  private[this] val slotsToArr: RawSlots => Arr = ss =>
    Arr(
      ss.map { case (f, t, rs) =>
        Arr(
          Num(f.value.toLong),
          Num(t.value.toLong),
          Arr(rs.toList.map {
            case (h, p, Some(mrid)) => Arr(Bulk(h.fold(_.value, _.value)), Num(p.value.toLong), Bulk(mrid.value))
            case (h, p, None)       => Arr(Bulk(h.fold(_.value, _.value)), Num(p.value.toLong))
          })
        )
      }
    )

  property("The Cluster protocol using addslots roundtrips successfully given one or more slots") {
    forAll { ss: OneOrMore[Slot] =>
      val protocol = addslots(ss)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER") :: Bulk("ADDSLOTS") :: ss.value.map(Bulk(_))))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Cluster protocol using clusterinfo roundtrips successfully using val") {
    forAll(infoGen) { info =>
      val protocol = clusterinfo
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("INFO")))
      protocol.decode(Bulk(info)) onRight (result => info.forall { case (k, v) => result.selectDynamic(k).fold(_ => false, _ == v) })
    }
  }

  property("The Cluster protocol using countfailurereports roundtrips successfully given a nodeId") {
    forAll { (n: NodeId, nni: NonNegInt) =>
      val protocol = countfailurereports(n)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("COUNT-FAILURE-REPORTS"), Bulk(n)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Cluster protocol using countfailurereports roundtrips successfully given a nodeId") {
    forAll { (n: NodeId, nni: NonNegInt) =>
      val protocol = countfailurereports(n)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("COUNT-FAILURE-REPORTS"), Bulk(n)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Cluster protocol using countkeysinslot roundtrips successfully given a slot") {
    forAll { (s: Slot, nni: NonNegInt) =>
      val protocol = countkeysinslot(s)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("COUNTKEYSINSLOT"), Bulk(s)))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The Cluster protocol using delslots roundtrips successfully given one or more slots") {
    forAll { ss: OneOrMore[Slot] =>
      val protocol = delslots(ss)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER") :: Bulk("DELSLOTS") :: ss.value.map(Bulk(_))))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  test("The Cluster protocol using failover roundtrips successfully using val") {
    val protocol = failover
    assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("FAILOVER")))
    assertEquals(protocol.decode(Str(OK.value)), OK)
  }

  property("The Cluster protocol using failover roundtrips successfully given mode") {
    forAll { m: ClusterFailoverMode =>
      val protocol = failover(m)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("FAILOVER"), Bulk(m)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Cluster protocol using forget roundtrips successfully given a nodeId") {
    forAll { n: NodeId =>
      val protocol = forget(n)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("FORGET"), Bulk(n)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Cluster protocol using getkeysinslot roundtrips successfully given a slot and a count") {
    forAll { (s: Slot, pi: PosInt, ks: List[Key]) =>
      val protocol = getkeysinslot(s, pi)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("GETKEYSINSLOT"), Bulk(s), Bulk(pi)))
      assertEquals(protocol.decode(Arr(ks.map(Bulk(_)))), ks)
    }
  }

  property("The Cluster protocol using keyslot roundtrips successfully given a key") {
    forAll { (k: Key, s: Slot) =>
      val protocol = keyslot(k)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("KEYSLOT"), Bulk(k)))
      assertEquals(protocol.decode(Num(s.value.toLong)), s)
    }
  }

  property("The Cluster protocol using meet roundtrips successfully given a host and a port") {
    forAll { (h: Host, p: Port) =>
      val protocol = meet(h, p)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("MEET"), Bulk(h), Bulk(p)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Cluster protocol using nodes roundtrips successfully using val") {
    forAll(nodesGen filter nodesIsValid) { ns =>
      val protocol = nodes
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("NODES")))
      protocol.decode(Bulk(ns)) onRightAll validateNodes(ns)
    }
  }

  property("The Cluster protocol using readonly roundtrips successfully using val") {
    val protocol = readonly
    assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("READONLY")))
    assertEquals(protocol.decode(Str(OK.value)), OK)
  }

  property("The Cluster protocol using readwrite roundtrips successfully using val") {
    val protocol = readwrite
    assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("READWRITE")))
    assertEquals(protocol.decode(Str(OK.value)), OK)
  }

  property("The Cluster protocol using replicas roundtrips successfully given a nodeId") {
    forAll(nodeIdArb.arbitrary, nodesGen filter nodesIsValid) { (n, ns) =>
      val protocol = replicas(n)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("REPLICAS"), Bulk(n)))
      protocol.decode(Bulk(ns)) onRightAll validateNodes(ns)
    }
  }

  property("The Cluster protocol using replicate roundtrips successfully given a nodeId") {
    forAll { n: NodeId =>
      val protocol = replicate(n)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("REPLICATE"), Bulk(n)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Cluster protocol using reset roundtrips successfully using val") {
    val protocol = reset
    assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("RESET"), Bulk("SOFT")))
    assertEquals(protocol.decode(Str(OK.value)), OK)
  }

  property("The Cluster protocol using reset roundtrips successfully given reset mode") {
    forAll { m: ClusterResetMode =>
      val protocol = reset(m)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("RESET"), Bulk(m)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Cluster protocol using saveconfig roundtrips successfully using val") {
    val protocol = saveconfig
    assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("SAVECONFIG")))
    assertEquals(protocol.decode(Str(OK.value)), OK)
  }

  property("The Cluster protocol using setconfigepoch roundtrips successfully given a config epoch") {
    forAll { nni: NonNegInt =>
      val protocol = setconfigepoch(nni)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("SET-CONFIG-EPOCH"), Bulk(nni)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Cluster protocol using setslot roundtrips successfully given a slot") {
    forAll { s: Slot =>
      val protocol = setslot(s)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("SETSLOT"), Bulk(s), Bulk("STABLE")))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Cluster protocol using setslot roundtrips successfully given a slot, mode and a node id") {
    forAll { (s: Slot, m: ClusterSetSlotMode, nid: NodeId) =>
      val protocol = setslot(s, m, nid)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("SETSLOT"), Bulk(s), Bulk(m), Bulk(nid)))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }

  property("The Cluster protocol using slaves roundtrips successfully given a nodeId") {
    forAll(nodeIdArb.arbitrary, nodesGen filter nodesIsValid) { (n, ns) =>
      val protocol = slaves(n)
      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("SLAVES"), Bulk(n)))
      protocol.decode(Bulk(ns)) onRightAll validateNodes(ns)
    }
  }

  property("The Cluster protocol using slots roundtrips successfully using val") {
    forAll(slotsGen) { ss =>
      val protocol = slots
      val ssWithLoopback = ss.map { case (f, t, assigned) =>
        (
          f,
          t,
          assigned.foldRight[List[(String, Port, Option[NodeId])]](Nil) {
            case ((Left(_), p, n), css)  => (LoopbackHost.value, p, n) :: css
            case ((Right(h), p, n), css) => (h.value, p, n) :: css
          }
        )
      }.toSet

      assertEquals(protocol.encode, Arr(Bulk("CLUSTER"), Bulk("SLOTS")))
      protocol
        .decode(slotsToArr(ss))
        .onRightAll(_.slots.foreach {
          case (ClusterRangeSlotType(f, t), ClusterNewSlotInfo(ClusterHostPortNodeId(mh, mp, mid), rs)) =>
            val mrs = (mh.value, mp, Some(mid)) :: rs.foldLeft(List.empty[(String, Port, Option[NodeId])]) {
              case (acc, ClusterHostPortNodeId(h, p, nid)) => acc :+ ((h.value, p, Some(nid)))
            }
            assert(ssWithLoopback.contains((f, t, mrs)))
          case (ClusterRangeSlotType(f, t), ClusterOldSlotInfo(ClusterHostPort(mh, mp), rs)) =>
            val mrs = (mh.value, mp, None) :: rs.foldLeft(List.empty[(String, Port, Option[NodeId])]) { case (acc, ClusterHostPort(h, p)) =>
              acc :+ ((h.value, p, None))
            }
            assert(ssWithLoopback.contains((f, t, mrs)))
        })
    }
  }
}
