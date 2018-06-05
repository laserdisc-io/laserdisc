package laserdisc
package protocol

trait BListP {
  import Read.==>
  import shapeless._

  final def blpop[A](keys: OneOrMoreKeys, seconds: NonNegInt)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Option[KV[A]]] =
    Protocol("BLPOP", keys.value :: seconds :: HNil).asC[NilArray :+: NonNilArray :+: CNil, Option[KV[A]]]

  final def brpop[A](keys: OneOrMoreKeys, seconds: NonNegInt)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Option[KV[A]]] =
    Protocol("BRPOP", keys.value :: seconds :: HNil).asC[NilArray :+: NonNilArray :+: CNil, Option[KV[A]]]

  final def brpoplpush[A](source: Key, destination: Key)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source :: destination :: 0 :: HNil)
      .asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[A]]

  final def brpoplpush[A](source: Key, destination: Key, timeout: PosInt)(
      implicit ev: NonNullBulkString ==> A
  ): Protocol.Aux[Option[A]] =
    Protocol("BRPOPLPUSH", source :: destination :: timeout :: HNil)
      .asC[NullBulkString :+: NonNullBulkString :+: CNil, Option[A]]
}

trait AllBListP extends BListP with BListPExtra
