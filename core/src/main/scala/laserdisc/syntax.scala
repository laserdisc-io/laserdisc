package laserdisc

object cluster     extends protocol.ClusterP
object connection  extends protocol.ConnectionP
object geo         extends protocol.GeoP
object hashmaps    extends protocol.HashP
object hyperloglog extends protocol.HyperLogLogP
object keys        extends protocol.KeyP
object lists       extends protocol.ListP { object blocking extends protocol.BListP }
object publish     extends protocol.PublishP
object server      extends protocol.ServerP
object sets        extends protocol.SetP
object sortedsets  extends protocol.SortedSetP
object strings     extends protocol.StringP
object transaction extends protocol.TransactionP

object all
    extends protocol.ClusterP
    with protocol.ConnectionP
    with protocol.GeoP
    with protocol.HashP
    with protocol.HyperLogLogP
    with protocol.KeyP
    with protocol.ListP
    with protocol.PublishP
    with protocol.ServerP
    with protocol.SetP
    with protocol.SortedSetP
    with protocol.StringP
    with protocol.TransactionP {
  final object blocking extends protocol.BListP
}
