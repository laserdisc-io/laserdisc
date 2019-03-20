package laserdisc

object connection  extends protocol.ConnectionP
object geo         extends protocol.AllGeoP
object hashmaps    extends protocol.AllHashP
object hyperloglog extends protocol.AllHyperLogLogP
object keys        extends protocol.AllKeyP
object lists       extends protocol.AllListP { object blocking extends protocol.AllBListP }
object publish     extends protocol.PublishP
object server      extends protocol.ServerP
object sets        extends protocol.AllSetP
object show        extends protocol.ShowSyntax
object sortedsets  extends protocol.AllSortedSetP
object strings     extends protocol.AllStringP

object all
    extends protocol.ConnectionP
    with protocol.AllGeoP
    with protocol.AllHashP
    with protocol.AllHyperLogLogP
    with protocol.AllKeyP
    with protocol.AllListP
    with protocol.PublishP
    with protocol.ServerP
    with protocol.AllSetP
    with protocol.ShowSyntax
    with protocol.AllSortedSetP
    with protocol.AllStringP {
  final object blocking extends protocol.AllBListP
}
