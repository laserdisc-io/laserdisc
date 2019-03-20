package laserdisc
package protocol

trait BListP {
  import shapeless._

  final def blpop[A: NonNullBulkString ==> ?](keys: OneOrMoreKeys, seconds: NonNegInt): Protocol.Aux[Option[KV[A]]] =
    Protocol("BLPOP", keys.value :: seconds :: HNil).asC[NilArray :+: NonNilArray :+: CNil, Option[KV[A]]]

  final def brpop[A: NonNullBulkString ==> ?](keys: OneOrMoreKeys, seconds: NonNegInt): Protocol.Aux[Option[KV[A]]] =
    Protocol("BRPOP", keys.value :: seconds :: HNil).asC[NilArray :+: NonNilArray :+: CNil, Option[KV[A]]]

  final def brpoplpush[A: NonNullBulkString ==> ?](source: Key, destination: Key): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source :: destination :: 0 :: HNil)
      .asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[A]]

  final def brpoplpush[A: NonNullBulkString ==> ?](
      source: Key,
      destination: Key,
      timeout: PosInt
  ): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source :: destination :: timeout :: HNil)
      .asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[A]]
}

trait AllBListP extends BListP with BListPExtra
