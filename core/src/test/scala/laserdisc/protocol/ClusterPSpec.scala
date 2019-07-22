package laserdisc
package protocol

final class ClusterPSpec extends BaseSpec with ClusterP {
  import clusters._
  import org.scalacheck.Gen
  import org.scalacheck.Gen._

  private[this] final val kvPairPattern                                = ClusterP.KVPairRegex.pattern
  private[this] final val infoIsValid: Map[String, String] => Boolean  = _.forall { case (k, v) => kvPairPattern.matcher(s"$k:$v").matches }
  private[this] final val infoGen: Gen[Map[String, String]]            = nonEmptyMap(identifier.flatMap(k => alphaStr.map(k -> _))) :| "info map"
  private[this] implicit final val infoShow: Show[Map[String, String]] = Show.instance { _.map { case (k, v) => s"$k:$v" }.mkString(CRLF) }

  private[this] final type RawNode  = (String, String, Int, Option[Int], Seq[String], String, Int, Int, Int, String, Seq[String])
  private[this] final type RawNodes = List[RawNode]
  private[this] final val rawNodeToString: RawNode => String = {
    case (nid, h, p, None, fs, mnid, ps, pr, ce, l, ss) =>
      s"$nid $h:$p ${fs.mkString(COMMA)} $mnid $ps $pr $ce $l${ss.mkString(SPACE, SPACE, "")}"
    case (nid, h, p, Some(cp), fs, mnid, ps, pr, ce, l, ss) =>
      s"$nid $h:$p@$cp ${fs.mkString(COMMA)} $mnid $ps $pr $ce $l${ss.mkString(SPACE, SPACE, "")}"
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
    val rawNode = for {
      nid   <- nodeIdGen
      h     <- Gen.oneOf(const(""), hostGen)
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

    choose(1, 10).flatMap(listOfN(_, rawNode)) :| "nodes"
  }
  private[this] implicit final val nodesShow: Show[RawNodes] = Show.instance {
    _.map(rawNodeToString).mkString(LF)
  }

  "The Cluster protocol" when {

    "using addslots" should {

      "roundtrip successfully" when {
        "given one or more slots" in forAll("slots") { (ss: OneOrMore[Slot]) =>
          val protocol = addslots(ss)

          protocol.encode shouldBe Arr(Bulk("CLUSTER") :: Bulk("ADDSLOTS") :: ss.value.map(Bulk(_)))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
      }
    }

    "using clusterinfo" should {

      "roundtrip successfully" when {
        "using val" in forAll(infoGen) { info =>
          whenever(infoIsValid(info)) {
            val protocol = clusterinfo

            protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("INFO"))
            val result = protocol.decode(Bulk(info)).right.value
            info.forall { case (k, v) => result.selectDynamic(k).right.value == v } shouldBe true
          }
        }
      }
    }

    "using countfailurereports" should {

      "roundtrip successfully" when {
        "given a nodeId" in forAll("node id", "return value") { (n: NodeId, nni: NonNegInt) =>
          val protocol = countfailurereports(n)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("COUNT-FAILURE-REPORTS"), Bulk(n))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
      }
    }

    "using countkeysinslot" should {

      "roundtrip successfully" when {
        "given a slot" in forAll("slot", "return value") { (s: Slot, nni: NonNegInt) =>
          val protocol = countkeysinslot(s)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("COUNTKEYSINSLOT"), Bulk(s))
          protocol.decode(Num(nni.value.toLong)).right.value shouldBe nni
        }
      }
    }

    "using delslots" should {

      "roundtrip successfully" when {
        "given one or more slots" in forAll("slots") { (ss: OneOrMore[Slot]) =>
          val protocol = delslots(ss)

          protocol.encode shouldBe Arr(Bulk("CLUSTER") :: Bulk("DELSLOTS") :: ss.value.map(Bulk(_)))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
      }
    }

    "using failover" should {

      "roundtrip successfully" when {
        "using val" in {
          val protocol = failover

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("FAILOVER"))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
        "given failover mode FORCE" in {
          val protocol = failover(failoverMode.force)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("FAILOVER"), Bulk("FORCE"))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
        "given failover mode TAKEOVER" in {
          val protocol = failover(failoverMode.takeover)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("FAILOVER"), Bulk("TAKEOVER"))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
      }
    }

    "using forget" should {

      "roundtrip successfully" when {
        "given a nodeId" in forAll("node id") { (n: NodeId) =>
          val protocol = forget(n)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("FORGET"), Bulk(n))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
      }
    }

    "using getkeysinslot" should {

      "roundtrip successfully" when {
        "given a slot and a count" in forAll("slot", "count", "return value") { (s: Slot, pi: PosInt, ks: List[Key]) =>
          val protocol = getkeysinslot(s, pi)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("GETKEYSINSLOT"), Bulk(s), Bulk(pi))
          protocol.decode(Arr(ks.map(Bulk(_)))).right.value shouldBe ks
        }
      }
    }

    "using keyslot" should {

      "roundtrip successfully" when {
        "given a key" in forAll("key", "return value") { (k: Key, s: Slot) =>
          val protocol = keyslot(k)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("KEYSLOT"), Bulk(k))
          protocol.decode(Num(s.value.toLong)).right.value shouldBe s
        }
      }
    }

    "using meet" should {

      "roundtrip successfully" when {
        "given a host and a port" in forAll("host", "port") { (h: Host, p: Port) =>
          val protocol = meet(h, p)

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("MEET"), Bulk(h), Bulk(p))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
      }
    }

    "using nodes" should {

      "roundtrip successfully" when {
        "using val" in forAll(nodesGen) { ns =>
          whenever(nodesIsValid(ns)) {
            val protocol = nodes

            protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("NODES"))
            protocol.decode(Bulk(ns)).right.value.clusterNodes.zip(ns).map {
              case (ClusterP.ClusterNode(n0, ClusterP.NodeAddress(h0, p0, cp0), fs0, m0, ps0, pr0, ce0, l0, ss0),
                    (n, h, p, cp, fs, m, ps, pr, ce, l, ss)) =>
                n0.value shouldBe n
                h0.value shouldBe (if (h.isEmpty) LoopbackEqWit.value else h)
                p0.value shouldBe p
                cp0.value shouldBe cp.getOrElse(p)
                (if (fs0.isEmpty) Seq("noflags") else fs0.map(Show[ClusterP.Flag].show)) shouldBe fs
                m0.fold("-")(_.value) shouldBe m
                ps0.value shouldBe ps
                pr0.value shouldBe pr
                ce0.value shouldBe ce
                Show[ClusterP.LinkState].show(l0) shouldBe l
                ss0.map {
                  case ClusterP.SlotType.Single(s)            => s.toString
                  case ClusterP.SlotType.Range(f, t)          => s"$f-$t"
                  case ClusterP.SlotType.ImportingSlot(s, in) => s"[$s-<-$in]"
                  case ClusterP.SlotType.MigratingSlot(s, mn) => s"[$s->-$mn]"
                } shouldBe ss
            }
          }
        }
      }
    }

    "using readonly" should {

      "roundtrip successfully" when {
        "using val" in {
          val protocol = readonly

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("READONLY"))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
      }
    }

    "using readwrite" should {

      "roundtrip successfully" when {
        "using val" in {
          val protocol = readwrite

          protocol.encode shouldBe Arr(Bulk("CLUSTER"), Bulk("READWRITE"))
          protocol.decode(Str(OK.value)).right.value shouldBe OK
        }
      }
    }
  }
}
