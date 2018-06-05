package laserdisc
package protocol

trait HyperLogLogP {
  final def pfadd(key: Key, elements: OneOrMoreKeys): Protocol.Aux[Boolean] =
    Protocol("PFADD", key :: elements.value).as[Integer, Boolean]

  final def pfcount(keys: OneOrMoreKeys): Protocol.Aux[NonNegInt] =
    Protocol("PFCOUNT", keys.value).as[Integer, NonNegInt]

  final def pfmerge(sourceKeys: TwoOrMoreKeys, destinationKey: Key): Protocol.Aux["OK"] =
    Protocol("PFMERGE", destinationKey :: sourceKeys.value).as[SimpleString, "OK"]
}

trait AllHyperLogLogP extends HyperLogLogP with HyperLogLogPExtra
