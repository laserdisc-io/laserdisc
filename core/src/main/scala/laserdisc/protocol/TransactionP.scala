package laserdisc
package protocol

trait TransactionP {
  //TODO: discard? exec/multi?

  final val unwatch: Protocol.Aux[OK] = Protocol("UNWATCH", Nil).as[Str, OK]

  final def watch(keys: OneOrMoreKeys): Protocol.Aux[OK] = Protocol("WATCH", keys.value).as[Str, OK]
}
