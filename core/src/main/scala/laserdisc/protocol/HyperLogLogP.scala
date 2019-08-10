package laserdisc
package protocol

trait HyperLogLogBaseP {
  final def pfadd(key: Key, elements: OneOrMoreKeys): Protocol.Aux[Boolean] = Protocol("PFADD", key :: elements.value).as[Num, Boolean]

  final def pfcount(keys: OneOrMoreKeys): Protocol.Aux[NonNegInt] = Protocol("PFCOUNT", keys.value).as[Num, NonNegInt]

  final def pfmerge(sourceKeys: TwoOrMoreKeys, destinationKey: Key): Protocol.Aux[OK] =
    Protocol("PFMERGE", destinationKey :: sourceKeys.value).as[Str, OK]
}

trait HyperLogLogP extends HyperLogLogBaseP with HyperLogLogExtP
