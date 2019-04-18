package laserdisc
package protocol

trait BListP {
  import shapeless._

  final def blpop[A: Bulk ==> ?](keys: OneOrMoreKeys, seconds: NonNegInt): Protocol.Aux[Option[KV[A]]] =
    Protocol("BLPOP", keys.value :: seconds :: HNil).asC[NilArr :+: Arr :+: CNil, Option[KV[A]]]

  final def brpop[A: Bulk ==> ?](keys: OneOrMoreKeys, seconds: NonNegInt): Protocol.Aux[Option[KV[A]]] =
    Protocol("BRPOP", keys.value :: seconds :: HNil).asC[NilArr :+: Arr :+: CNil, Option[KV[A]]]

  final def brpoplpush[A: Bulk ==> ?](source: Key, destination: Key): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source :: destination :: 0 :: HNil)
      .asC[NullBulk :+: Bulk :+: CNil, Option[A]]

  final def brpoplpush[A: Bulk ==> ?](
      source: Key,
      destination: Key,
      timeout: PosInt
  ): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source :: destination :: timeout :: HNil)
      .asC[NullBulk :+: Bulk :+: CNil, Option[A]]
}

trait AllBListP extends BListP with BListPExtra
