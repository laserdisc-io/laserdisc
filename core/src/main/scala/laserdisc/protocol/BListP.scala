package laserdisc
package protocol

trait BListBaseP {
  import shapeless._

  final def blpop[A: Bulk ==> *](keys: OneOrMoreKeys, seconds: NonNegInt): Protocol.Aux[Option[KV[A]]] =
    Protocol("BLPOP", keys.value :: seconds :: HNil).opt[GenArr].as[KV[A]]

  final def brpop[A: Bulk ==> *](keys: OneOrMoreKeys, seconds: NonNegInt): Protocol.Aux[Option[KV[A]]] =
    Protocol("BRPOP", keys.value :: seconds :: HNil).opt[GenArr].as[KV[A]]

  final def brpoplpush[A: Bulk ==> *](source: Key, destination: Key): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source :: destination :: 0 :: HNil).opt[GenBulk].as[A]
  final def brpoplpush[A: Bulk ==> *](source: Key, destination: Key, timeout: PosInt): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source :: destination :: timeout :: HNil).opt[GenBulk].as[A]
}

trait BListP extends BListBaseP with BListExtP
