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

final class ClusterManagementPSpec extends BaseSpec with ClusterP with ServerBackupP {
  import ClusterManagement.*

  private def bulks(ss: String*): Arr = Arr(ss.map(Bulk(_)).toList)
  private val ranges                  = OneOrMore.unsafeFrom(List(Slot(0) -> Slot(8000), Slot(8001) -> Slot(16383)))
  private val nodeId                  = NodeId("ce876dfaff2bd8f5e0bb0492031638025a16d583")
  private val otherNodeId             = NodeId("f58728d7657ee5029ef63532d343b982738a1eda")

  test("The Cluster protocol using myid, bumpepoch and the slot range commands sends and reads what Redis does") {
    assertEquals(myid.decode(Bulk(nodeId.value)), nodeId)
    assertEquals(bumpepoch.decode(Str("BUMPED 1")), "BUMPED 1")
    assertEquals(addslotsrange(ranges).encode, bulks("CLUSTER", "ADDSLOTSRANGE", "0", "8000", "8001", "16383"))
    assertEquals(delslotsrange(ranges).encode, bulks("CLUSTER", "DELSLOTSRANGE", "0", "8000", "8001", "16383"))
    assertEquals(trimslots(ranges).encode, bulks("TRIMSLOTS", "RANGES", "2", "0", "8000", "8001", "16383"))
  }

  test("The Cluster protocol using shards reads Redis's shards and Valkey's, which have an id") {
    def node(ip: String) = Arr(
      bulks("id", nodeId.value, "port").elements ::: Num(6379L) :: bulks(
        "ip",
        ip,
        "endpoint",
        ip,
        "role",
        "master",
        "replication-offset"
      ).elements :::
        Num(0L) :: bulks("health", "online").elements
    )
    val redis  = Arr(Arr(Bulk("slots"), Arr(Num(0L), Num(16383L)), Bulk("nodes"), Arr(node("172.18.0.3"))))
    val valkey = Arr(
      Arr(
        Bulk("slots"),
        Arr(Num(0L), Num(16383L)),
        Bulk("nodes"),
        Arr(node("")),
        Bulk("id"),
        Bulk("4638e2605f114ba2283c979cb2b36add1200d95c")
      )
    )
    val expected = ShardNode(nodeId.value, "172.18.0.3", Some("172.18.0.3"), "master", "online", 0L, Some(6379), None, None)
    laserdisc.redis_7_0.shards.decode(redis).onRight(_ == Seq(ShardOf7_0(Seq(Slot(0) -> Slot(16383)), Seq(expected))))
    // Valkey 9.1's import reads each shard's id, which it always sends
    shards.decode(valkey).onRight(_.map(_.id) == Seq("4638e2605f114ba2283c979cb2b36add1200d95c"))
    assert(shards.decode(redis).isLeft)
  }

  test("The Cluster protocol using shards reads an endpoint sent as null or empty as the host you connected to") {
    def withEndpoint(endpoint: RESP) = Arr(
      Arr(
        Bulk("slots"),
        Arr(Nil),
        Bulk("nodes"),
        Arr(
          Arr(
            List[RESP](
              Bulk("id"),
              Bulk(nodeId.value),
              Bulk("ip"),
              Bulk("10.0.0.1"),
              Bulk("endpoint"),
              endpoint,
              Bulk("role"),
              Bulk("master")
            ) ++
              List[RESP](Bulk("replication-offset"), Num(0L), Bulk("health"), Bulk("online"))
          )
        )
      )
    )
    val v = laserdisc.redis_7_0
    v.shards.decode(withEndpoint(NullBulk)).onRight(_.flatMap(_.nodes.map(_.endpoint)) == Seq(None))
    v.shards.decode(withEndpoint(Bulk(""))).onRight(_.flatMap(_.nodes.map(_.endpoint)) == Seq(None))
    v.shards.decode(withEndpoint(Bulk("?"))).onRight(_.flatMap(_.nodes.map(_.endpoint)) == Seq(Some("?")))
  }

  test("The Cluster protocol using links reads each link") {
    val reply = Arr(
      Arr(
        bulks("direction", "to", "node", nodeId.value, "create-time").elements ::: Num(1793585007295L) :: bulks(
          "events",
          "r",
          "send-buffer-allocated"
        ).elements :::
          Num(48L) :: Bulk("send-buffer-used") :: Num(48L) :: Nil
      )
    )
    links.decode(reply).onRight(_ == Seq(ClusterLink("to", nodeId.value, 1793585007295L, "r", 48L, 48L)))
  }

  test("The Cluster protocol using slotstats sends the range or the order, and reads each slot's figures") {
    assertEquals(slotstats(Slot(0), Slot(10)).encode, bulks("CLUSTER", "SLOT-STATS", "SLOTSRANGE", "0", "10"))
    assertEquals(
      slotstats(SlotStatsMetric.networkBytesIn, PosInt(2), Direction.desc).encode,
      bulks("CLUSTER", "SLOT-STATS", "ORDERBY", "network-bytes-in", "LIMIT", "2", "DESC")
    )
    val reply = Arr(Arr(Num(15495L), Arr(Bulk("key-count"), Num(1L), Bulk("cpu-usec"), Num(7L))))
    slotstats(SlotStatsMetric.keyCount).decode(reply).onRight(_ == Seq(SlotStats(Slot(15495), Map("key-count" -> 1L, "cpu-usec" -> 7L))))
  }

  test("The Cluster protocol using slotstatsbymemory sends the order, and reads each slot's figures") {
    assertEquals(slotstatsbymemory.encode, bulks("CLUSTER", "SLOT-STATS", "ORDERBY", "memory-bytes"))
    assertEquals(slotstatsbymemory(PosInt(2)).encode, bulks("CLUSTER", "SLOT-STATS", "ORDERBY", "memory-bytes", "LIMIT", "2"))
    assertEquals(slotstatsbymemory(Direction.asc).encode, bulks("CLUSTER", "SLOT-STATS", "ORDERBY", "memory-bytes", "ASC"))
    assertEquals(
      slotstatsbymemory(PosInt(2), Direction.desc).encode,
      bulks("CLUSTER", "SLOT-STATS", "ORDERBY", "memory-bytes", "LIMIT", "2", "DESC")
    )
    val reply = Arr(Arr(Num(0L), Arr(Bulk("key-count"), Num(1L), Bulk("memory-bytes"), Num(56L))))
    slotstatsbymemory.decode(reply).onRight(_ == Seq(SlotStats(Slot(0), Map("key-count" -> 1L, "memory-bytes" -> 56L))))
  }

  test("The Cluster protocol using Redis's migration and Valkey's slot migrations sends and reads them") {
    assertEquals(migration.importslots(ranges).encode, bulks("CLUSTER", "MIGRATION", "IMPORT", "0", "8000", "8001", "16383"))
    assertEquals(migration.cancel(Key("t1")).encode, bulks("CLUSTER", "MIGRATION", "CANCEL", "ID", "t1"))
    assertEquals(
      migration.status.decode(Arr(Arr(Bulk("id"), Bulk("t1"), Bulk("state"), Bulk("running")))),
      Seq(SlotMigration(Map("id" -> "t1", "state" -> "running")))
    )
    assertEquals(
      migrateslots(ranges, nodeId).encode,
      bulks("CLUSTER", "MIGRATESLOTS", "SLOTSRANGE", "0", "8000", "8001", "16383", "NODE", nodeId.value)
    )
    assertEquals(
      migrateslots(OneOrMore.unsafeFrom(List(ranges -> nodeId, OneOrMore.unsafeFrom(List(Slot(9) -> Slot(9))) -> otherNodeId))).encode,
      bulks(
        "CLUSTER",
        "MIGRATESLOTS",
        "SLOTSRANGE",
        "0",
        "8000",
        "8001",
        "16383",
        "NODE",
        nodeId.value,
        "SLOTSRANGE",
        "9",
        "9",
        "NODE",
        otherNodeId.value
      )
    )
    assertEquals(getslotmigrations.decode(Arr(Nil)), Seq.empty)
    assertEquals(flushslot(Slot(1), FlushMode.async).encode, bulks("CLUSTER", "FLUSHSLOT", "1", "ASYNC"))
    assertEquals(replicatenoone.encode, bulks("CLUSTER", "REPLICATE", "NO", "ONE"))
    assertEquals(setslot(Slot(1), NonNegLong(100L)).encode, bulks("CLUSTER", "SETSLOT", "1", "STABLE", "TIMEOUT", "100"))
  }

  test("The Server protocol using backup reads the status") {
    val reply =
      Arr(Bulk("state"), Bulk("snapshotting"), Bulk("error"), Bulk(""), Bulk("start_time"), Num(1793584993L), Bulk("end_time"), Num(0L))
    assertEquals(backup.status.decode(reply), BackupStatus("snapshotting", "", 1793584993L, 0L))
    assertEquals(backup.list.decode(Arr(Nil)), Seq.empty)
  }
}
