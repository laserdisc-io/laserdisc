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
package fs2

import cats.effect.IO
import laserdisc.auto.*
import laserdisc.protocol.ClusterP
import laserdisc.protocol.ClusterP.{Flag, LinkState, SlotType}

import scala.annotation.tailrec

final class RedisClusterClientSpec extends ClusterClientSpec(7100, "redis", redis_7_0.cluster) {
  test("a redis cluster client reads the hostname a node announces, with the nodes and the slots") {
    import redis_7_0.config
    hostname(config.set(OneOrMore.unsafeFrom(List(Key("cluster-announce-hostname") -> "my.host.example"))))
  }
}

final class KeyDbClusterClientSpec extends ClusterClientSpec(7110, "keyDb", redis_6_2.cluster)

final class ValkeyClusterClientSpec extends ClusterClientSpec(7120, "valkey", valkey_7_2.cluster) {
  test("a valkey cluster client reads the hostname a node announces, with the nodes and the slots") {
    import valkey_7_2.config
    hostname(config.set(OneOrMore.unsafeFrom(List(Key("cluster-announce-hostname") -> "my.host.example"))))
  }
}

// a master with all the slots, its replica, another master with no slots, and a node that's on its own
abstract class ClusterClientSpec(p: Port, dest: String, cluster: Cluster6_2) extends LaserdiscFs2Suite(p) {
  import cluster.*

  private val replicaPort = Port.unsafeFrom(p.value + 1)
  private val otherPort   = Port.unsafeFrom(p.value + 2)
  private val freshPort   = Port.unsafeFrom(p.value + 3)
  private val everySlot   = OneOrMore.unsafeFrom((0 to 16383).toList.map(Slot.unsafeFrom))

  private def result[A](on: String, sent: IO[Maybe[A]]): A =
    sent.unsafeRunSync()(using runtime).fold(e => fail(s"$dest $on replied with an error: ${e.getMessage}"), identity)

  private def send[A](protocol: Protocol.Aux[A]): A               = result("master", client.send(protocol))
  private def sendTo[A](port: Port)(protocol: Protocol.Aux[A]): A =
    result(s"on $port", RedisClient[IO].to("127.0.0.1", port).use(_.send(protocol)))

  private def waitUntil(what: String)(done: =>Boolean): Unit = {
    @tailrec def retry(attempts: Int): Unit =
      if (!done)
        if (attempts == 0) fail(s"$dest: $what didn't happen")
        else {
          Thread.sleep(100L)
          retry(attempts - 1)
        }
    retry(300)
  }

  private def seenBy(port: Port): Seq[ClusterP.Node] = sendTo(port)(nodes).nodes

  // what's already been done is skipped, as the servers outlive the tests
  private lazy val ids: (NodeId, NodeId, NodeId) = {
    val (master, replica, other) = (send(myid), sendTo(replicaPort)(myid), sendTo(otherPort)(myid))

    if (send(nodes).nodes.size < 3) {
      assertEquals(send(meet("127.0.0.1", replicaPort)), OK)
      assertEquals(send(meet("127.0.0.1", otherPort)), OK)
    }
    waitUntil("the nodes meeting") {
      List(p, replicaPort, otherPort).forall(seenBy(_).map(_.id).toSet == Set(master, replica, other))
    }
    if (send(nodes).nodes.find(_.id == master).exists(_.slots.isEmpty)) assertEquals(send(addslots(everySlot)), OK)
    if (seenBy(replicaPort).find(_.id == replica).exists(!_.flags.contains(Flag.replica))) {
      waitUntil("the replica learning who has the slots") {
        seenBy(replicaPort).find(_.id == master).exists(_.flags.contains(Flag.master))
      }
      assertEquals(sendTo(replicaPort)(replicate(master)), OK)
    }
    // pongs are timestamps, which is what's made the replies of real clusters different from a fresh node's
    waitUntil("the nodes hearing from each other") {
      val others = send(nodes).nodes.filter(_.id != master)
      others.size == 2 && others.forall(n => n.pong.value > 0L && n.link == LinkState.connected) &&
      others.find(_.id == replica).exists(_.flags.contains(Flag.replica))
    }
    waitUntil("the master seeing its replica") {
      send(slots).infoFor(Slot.unsafeFrom(0)).exists(_.replicas.map(_.nodeId) == Seq(replica))
    }

    (master, replica, other)
  }

  test(s"a cluster client reads the nodes of a cluster, with when they were last heard from and where they are") {
    val (master, replica, other) = ids
    val all                      = send(nodes).nodes.map(n => n.id -> n).toMap

    assertEquals(all.keySet, Set(master, replica, other))
    assertEquals(all(master).flags.toSet, Set[ClusterP.Flag](Flag.myself, Flag.master))
    assertEquals(all(master).slots, Seq[SlotType](SlotType.Range(Slot.unsafeFrom(0), Slot.unsafeFrom(16383))))
    assertEquals(all(master).maybeMaster, None)
    assertEquals(all(replica).flags, Seq[ClusterP.Flag](Flag.replica))
    assertEquals(all(replica).maybeMaster, Some(master))
    assertEquals(all(other).flags, Seq[ClusterP.Flag](Flag.master))

    val milliseconds2020 = 1577836800000L
    assert(all(replica).pong.value > milliseconds2020, s"pong was ${all(replica).pong}")

    List(master -> p, replica -> replicaPort, other -> otherPort).foreach { case (id, port) =>
      val address = all(id).address.getOrElse(fail(s"no address for $id"))
      assertEquals(address.host, Some[Host]("127.0.0.1"))
      assertEquals(address.port, port)
      assertEquals(address.clusterPort.value, port.value + 10000)
    }
  }

  test("a cluster client reads a node that hasn't met any other as having no host") {
    val fresh   = sendTo(freshPort)(nodes).nodes
    val address = fresh.headOption.flatMap(_.address).getOrElse(fail(s"no address in $fresh"))

    assertEquals(fresh.size, 1)
    assertEquals(address.host, None)
    assertEquals(address.port, freshPort)
  }

  test("a cluster client reads the replicas of a master") {
    val (master, replica, other) = ids

    assertEquals(send(replicas(master)).nodes.map(_.id), Seq(replica))
    assertEquals(send(slaves(master)).nodes.map(_.id), Seq(replica))
    assertEquals(send(replicas(master)).nodes.flatMap(_.maybeMaster), Seq(master))
    assertEquals(send(replicas(other)).nodes, Seq.empty[ClusterP.Node])
  }

  test("a cluster client reads who serves the slots") {
    val (master, replica, _) = ids
    val info                 = send(slots).infoFor(Slot.unsafeFrom(12345)).getOrElse(fail("no one serves the slot"))

    assertEquals(info.master.nodeId, master)
    assertEquals(info.master.port, p)
    assertEquals(info.master.host, Some[Host]("127.0.0.1"))
    assertEquals(info.replicas.map(_.nodeId), Seq(replica))
    assertEquals(info.replicas.map(_.port), Seq(replicaPort))
  }

  test("a cluster client reads the state of the cluster") {
    ids
    waitUntil("the cluster being ready")(send(clusterinfo).cluster_state[String] == Right("ok"))
    assertEquals(send(clusterinfo).cluster_known_nodes[String], Right("3"))
    assertEquals(send(clusterinfo).cluster_slots_assigned[String], Right("16384"))
  }

  // sets the hostname on the master with the command given, then reads it back
  protected final def hostname(set: Protocol.Aux[OK]): Unit = {
    val (master, _, _) = ids
    val name           = "my.host.example"

    assertEquals(send(set), OK)
    assertEquals(send(nodes).nodes.find(_.id == master).flatMap(_.address).flatMap(_.hostname), Some(Host.unsafeFrom(name)))
    assertEquals(send(slots).infoFor(Slot.unsafeFrom(0)).map(_.master.metadata.get("hostname")), Some(Some(name)))
  }
}
