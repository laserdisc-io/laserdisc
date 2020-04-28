package laserdisc
package protocol

import org.scalacheck.Prop.forAll

abstract class HyperLogLogPSpec extends BaseSpec with HyperLogLogP {

  property("The HyperLogLog protocol using pfadd roundtrips successfully given key and elements") {
    forAll { (k: Key, es: OneOrMoreKeys, b: Boolean) =>
      val protocol = pfadd(k, es)
      assertEquals(protocol.encode, Arr(Bulk("PFADD") :: Bulk(k) :: es.value.map(Bulk(_))))
      assertEquals(protocol.decode(boolToNum(b)), b)
    }
  }

  property("The HyperLogLog protocol using pfcount roundtrips successfully given keys") {
    forAll { (ks: OneOrMoreKeys, nni: NonNegInt) =>
      val protocol = pfcount(ks)
      assertEquals(protocol.encode, Arr(Bulk("PFCOUNT") :: ks.value.map(Bulk(_))))
      assertEquals(protocol.decode(Num(nni.value.toLong)), nni)
    }
  }

  property("The HyperLogLog protocol using pfmerge roundtrips successfully given two or more source keys and a destination key") {
    forAll { (sks: TwoOrMoreKeys, dk: Key) =>
      val protocol = pfmerge(sks, dk)
      assertEquals(protocol.encode, Arr(Bulk("PFMERGE") :: Bulk(dk) :: sks.value.map(Bulk(_))))
      assertEquals(protocol.decode(Str(OK.value)), OK)
    }
  }
}
