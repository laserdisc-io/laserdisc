package laserdisc
package protocol

final class ClusterPSpec extends BaseSpec with ClusterP {
  import clustertypes._
  import org.scalacheck.{Arbitrary, Gen}
  import org.scalacheck.Gen._

  private[this] implicit final val clusterFailoverModeArb: Arbitrary[ClusterFailoverMode] = Arbitrary {
    Gen.oneOf(ClusterFailoverMode.force, ClusterFailoverMode.takeover)
  }
  private[this] implicit final val clusterResetModeArb: Arbitrary[ClusterResetMode] = Arbitrary {
    Gen.oneOf(ClusterResetMode.hard, ClusterResetMode.soft)
  }
  private[this] implicit final val clusterSetSlotModeArb: Arbitrary[ClusterSetSlotMode] = Arbitrary {
    Gen.oneOf(ClusterSetSlotMode.importing, ClusterSetSlotMode.migrating, ClusterSetSlotMode.node)
  }

  private[this] final val kvPairPattern                                = KVPairRegex.pattern
  private[this] final val infoIsValid: Map[String, String] => Boolean  = _.forall { case (k, v) => kvPairPattern.matcher(s"$k:$v").matches }
  private[this] final val infoGen: Gen[Map[String, String]]            = nonEmptyMap(identifier.flatMap(k => alphaStr.map(k -> _))) :| "info map"
  private[this] implicit final val infoShow: Show[Map[String, String]] = Show.instance { _.map { case (k, v) => s"$k:$v" }.mkString(CRLF) }

  private[this] final type RawNode =
    (String, EmptyString | Host, Port, Option[Port], Seq[String], String, Int, Int, Int, String, Seq[String])
  private[this] final type RawNodes = List[RawNode]
  private[this] final val rawNodeToString: RawNode => String = {
    case (nid, h, p, None, fs, mnid, ps, pr, ce, l, ss) =>
      s"$nid ${h.fold(_.value, _.value)}:$p ${fs.mkString(COMMA)} $mnid $ps $pr $ce $l${ss.mkString(SPACE, SPACE, "")}"
    case (nid, h, p, Some(cp), fs, mnid, ps, pr, ce, l, ss) =>
      s"$nid ${h.fold(_.value, _.value)}:$p@$cp ${fs.mkString(COMMA)} $mnid $ps $pr $ce $l${ss.mkString(SPACE, SPACE, "")}"
  }
  private[this] final val nodesIsValid: RawNodes => Boolean = {
    val nid    = NodeIdRegexWit.value
    val ip     = IPv4RegexWit.value
    val domain = Rfc1123HostnameRegexWit.value
    val flags  = raw"noflags|(myself|master|slave|fail\?|fail|handshake|noaddr)(,(myself|master|slave|fail\?|fail|handshake|noaddr))*"
    val link   = raw"(connected|disconnected)"
    val slots  = raw"(\d+|(\d+-\d+)|\[\d+-[><]-$nid\])?(\s(\d+|(\d+-\d+)|\[\d+-[><]-$nid\]))*"
    val R      = raw"^$nid\s($ip|$domain)?:(\d+)(@\d+)?\s($flags)\s(-|$nid)\s\d+\s\d+\s\d+\s$link\s${slots}$$".r

    _.map(rawNodeToString).forall { case R(_*) => true; case _ => false }
  }
  private[this] final val nodesGen: Gen[RawNodes] = {
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
  private[this] implicit final val nodesShow: Show[RawNodes] = Show.instance {
    _.map(rawNodeToString).mkString(LF)
  }
  private[this] final val validateNodes: RawNodes => ClusterNodes => Unit =
    rns =>
      ns =>
        ns.nodes.zip(rns).foreach {
          case (ClusterNode(n0, ClusterAddress(h0, p0, cp0), fs0, m0, ps0, pr0, ce0, l0, ss0), (n, h, p, cp, fs, m, ps, pr, ce, l, ss)) =>
            n0.value shouldBe n
            h0 shouldBe h.fold(_ => LoopbackHost, identity)
            p0 shouldBe p
            cp0 shouldBe cp.getOrElse(p)
            (if (fs0.isEmpty) Seq("noflags") else fs0.map(Show[ClusterFlag].show)) shouldBe fs
            m0.fold("-")(_.value) shouldBe m
            ps0.value shouldBe ps
            pr0.value shouldBe pr
            ce0.value shouldBe ce
            Show[ClusterLinkState].show(l0) shouldBe l
            ss0.map {
              case ClusterSingleSlotType(s)        => s.toString
              case ClusterRangeSlotType(f, t)      => s"$f-$t"
              case ClusterImportingSlotType(s, in) => s"[$s-<-$in]"
              case ClusterMigratingSlotType(s, mn) => s"[$s->-$mn]"
            } shouldBe ss
        }

  private[this] final type RawSlot  = (Slot, Slot, Seq[(EmptyString | Host, Port, Option[NodeId])])
  private[this] final type RawSlots = List[RawSlot]
  private[this] final val slotsGen: Gen[RawSlots] = (for {
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
      } yield if (fst.value < snd.value) (fst, snd, mrs) else (snd, fst, mrs)
    )
  } yield rss) :| "raw slots info"
  private[this] final val slotsToArr: RawSlots => Arr = ss =>
    Arr(
      ss.map {
        case (f, t, rs) =>
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

  "The Cluster protocol" when {
    "using addslots" should {
      "roundtrip successfully" when {
        "given one or more slots" in forAll("slots") { ss: OneOrMore[Slot] =>
          val protocol = addslots(ss)

          protocol.encode shouldBe Arr(Bulk("CLUSTER") :: Bulk("ADDSLOTS") :: ss.value.map(Bulk(_)))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using clusterinfo" should {
      "roundtrip successfully" when {
        "using val" in forAll(infoGen) { info =>
          whenever(infoIsValid(info)) {
            val protocol = clusterinfo

            protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("INFO"))
            protocol.decode(Bulk(info)) onRight (
                result => info.forall { case (k, v) => result.selectDynamic(k).fold(_ => false, _ == v) } shouldBe true
            )
          }
        }
      }
    }

    "using countfailurereports" should {
      "roundtrip successfully" when {
        "given a nodeId" in forAll("node id", "failure reports") { (n: NodeId, nni: NonNegInt) =>
          val protocol = countfailurereports(n)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("COUNT-FAILURE-REPORTS"), Bulk(n))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using countkeysinslot" should {
      "roundtrip successfully" when {
        "given a slot" in forAll("slot", "keys in slot") { (s: Slot, nni: NonNegInt) =>
          val protocol = countkeysinslot(s)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("COUNTKEYSINSLOT"), Bulk(s))
          protocol.decode(Num(nni.value.toLong)) onRight (_ shouldBe nni)
        }
      }
    }

    "using delslots" should {
      "roundtrip successfully" when {
        "given one or more slots" in forAll("slots") { ss: OneOrMore[Slot] =>
          val protocol = delslots(ss)

          protocol.encode shouldBe Arr(Bulk("CLUSTER") :: Bulk("DELSLOTS") :: ss.value.map(Bulk(_)))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using failover" should {
      "roundtrip successfully" when {
        "using val" in {
          val protocol = failover

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("FAILOVER"))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
        "given mode" in forAll("failover mode") { m: ClusterFailoverMode =>
          val protocol = failover(m)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("FAILOVER"), Bulk(m))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using forget" should {
      "roundtrip successfully" when {
        "given a nodeId" in forAll("node id") { n: NodeId =>
          val protocol = forget(n)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("FORGET"), Bulk(n))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using getkeysinslot" should {
      "roundtrip successfully" when {
        "given a slot and a count" in forAll("slot", "count", "keys in slot") { (s: Slot, pi: PosInt, ks: List[Key]) =>
          val protocol = getkeysinslot(s, pi)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("GETKEYSINSLOT"), Bulk(s), Bulk(pi))
          protocol.decode(Arr(ks.map(Bulk(_)))) onRight (_ shouldBe ks)
        }
      }
    }

    "using keyslot" should {
      "roundtrip successfully" when {
        "given a key" in forAll("key", "slot") { (k: Key, s: Slot) =>
          val protocol = keyslot(k)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("KEYSLOT"), Bulk(k))
          protocol.decode(Num(s.value.toLong)) onRight (_ shouldBe s)
        }
      }
    }

    "using meet" should {
      "roundtrip successfully" when {
        "given a host and a port" in forAll("host", "port") { (h: Host, p: Port) =>
          val protocol = meet(h, p)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("MEET"), Bulk(h), Bulk(p))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using nodes" should {
      "roundtrip successfully" when {
        "using val" in forAll(nodesGen) { ns =>
          whenever(nodesIsValid(ns)) {
            val protocol = nodes

            protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("NODES"))
            protocol.decode(Bulk(ns)) onRightAll validateNodes(ns)
          }
        }
      }
    }

    "using readonly" should {
      "roundtrip successfully" when {
        "using val" in {
          val protocol = readonly

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("READONLY"))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using readwrite" should {
      "roundtrip successfully" when {
        "using val" in {
          val protocol = readwrite

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("READWRITE"))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using replicas" should {
      "roundtrip successfully" when {
        "given a nodeId" in forAll(nodeIdArb.arbitrary, nodesGen) { (n, ns) =>
          whenever(nodesIsValid(ns)) {
            val protocol = replicas(n)

            protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("REPLICAS"), Bulk(n))
            protocol.decode(Bulk(ns)) onRightAll validateNodes(ns)
          }
        }
      }
    }

    "using replicate" should {
      "roundtrip successfully" when {
        "given a nodeId" in forAll("node id") { n: NodeId =>
          val protocol = replicate(n)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("REPLICATE"), Bulk(n))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using reset" should {
      "roundtrip successfully" when {
        "using val" in {
          val protocol = reset

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("RESET"), Bulk("SOFT"))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
        "given reset mode" in forAll("reset mode") { m: ClusterResetMode =>
          val protocol = reset(m)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("RESET"), Bulk(m))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using saveconfig" should {
      "roundtrip successfully" when {
        "using val" in {
          val protocol = saveconfig

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("SAVECONFIG"))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using setconfigepoch" should {
      "roundtrip successfully" when {
        "given a config epoch" in forAll("config epoch") { nni: NonNegInt =>
          val protocol = setconfigepoch(nni)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("SET-CONFIG-EPOCH"), Bulk(nni))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using setslot" should {
      "roundtrip successfully" when {
        "given a slot" in forAll("slot") { s: Slot =>
          val protocol = setslot(s)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("SETSLOT"), Bulk(s), Bulk("STABLE"))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
        "given a slot, mode and a node id" in forAll("slot", "mode", "node id") { (s: Slot, m: ClusterSetSlotMode, nid: NodeId) =>
          val protocol = setslot(s, m, nid)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("SETSLOT"), Bulk(s), Bulk(m), Bulk(nid))
          protocol.decode(Str(OK.value)) onRight (_ shouldBe OK)
        }
      }
    }

    "using slaves" should {
      "roundtrip successfully" when {
        "given a nodeId" in forAll(nodeIdArb.arbitrary, nodesGen) { (n, ns) =>
          whenever(nodesIsValid(ns)) {
            val protocol = slaves(n)

            protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("SLAVES"), Bulk(n))
            protocol.decode(Bulk(ns)) onRightAll validateNodes(ns)
          }
        }
      }
    }

    "using slots" should {
      "roundtrip successfully" when {
        "using val" in forAll(slotsGen) { ss =>
          val protocol = slots
          val ssWithLoopback = ss.map {
            case (f, t, assigned) =>
              (f, t, assigned.foldRight[List[(String, Port, Option[NodeId])]](Nil) {
                case ((Left(_), p, n), css)  => (LoopbackHost.value, p, n) :: css
                case ((Right(h), p, n), css) => (h.value, p, n) :: css
              })
          }.toSet

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("SLOTS"))
          protocol.decode(slotsToArr(ss)) onRightAll (_.slots.foreach {
            case (ClusterRangeSlotType(f, t), ClusterNewSlotInfo(ClusterHostPortNodeId(mh, mp, mid), rs)) =>
              val mrs = (mh.value, mp, Some(mid)) :: rs.foldLeft(List.empty[(String, Port, Option[NodeId])]) {
                case (acc, ClusterHostPortNodeId(h, p, nid)) => acc :+ ((h.value, p, Some(nid)))
              }
              ssWithLoopback should contain((f, t, mrs))
            case (ClusterRangeSlotType(f, t), ClusterOldSlotInfo(ClusterHostPort(mh, mp), rs)) =>
              val mrs = (mh.value, mp, None) :: rs.foldLeft(List.empty[(String, Port, Option[NodeId])]) {
                case (acc, ClusterHostPort(h, p)) => acc :+ ((h.value, p, None))
              }
              ssWithLoopback should contain((f, t, mrs))
          })
        }
      }
    }
  }
}
