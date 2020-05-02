package laserdisc
package protocol

import shapeless._

sealed trait Request {
  def command: String
  def parameters: Seq[GenBulk]
}

sealed trait Response { type A }

sealed trait ProtocolCodec[A] extends Any {
  def encode(protocol: Protocol): RESP
  def decode(resp: RESP): Maybe[A]
}

sealed trait Protocol extends Request with Response { self =>
  def codec: ProtocolCodec[A]

  final def encode: RESP                 = codec.encode(this)
  final def decode(resp: RESP): Maybe[A] = codec.decode(resp)

  final def map[B](f: A => B): Protocol.Aux[B] = new Protocol {
    override type A = B
    override def codec: ProtocolCodec[B] = new ProtocolCodec[B] {
      override def encode(protocol: Protocol): RESP = self.codec.encode(protocol)
      override def decode(resp: RESP): Maybe[B]     = self.codec.decode(resp).map(f(_))
    }
    override def command: String          = self.command
    override def parameters: Seq[GenBulk] = self.parameters
  }

  override final def toString: String = s"Protocol($command)"
}

object Protocol {
  final type Aux[A0] = Protocol { type A = A0 }

  final class RESPProtocolCodec[A, B](val R: RESPRead.Aux[A, B]) extends AnyVal with ProtocolCodec[B] {
    override def encode(value: Protocol): RESP = Arr(Bulk(value.command), value.parameters: _*)
    override def decode(resp: RESP): Maybe[B]  = R.read(resp)
  }

  final class PartiallyAppliedOptionalProtocol[L, A](private val underlying: PartiallyAppliedProtocol[L]) extends AnyVal {
    def as[B](implicit R: RESPRead.Aux[A, Option[B]]): Protocol.Aux[Option[B]] = underlying.asC(R)
  }

  sealed abstract class PartiallyAppliedProtocol[L: RESPParamWrite](
      private[this] final val command: String,
      private[this] final val l: L
  ) { self =>

    /**
      * We request evidence that the [[RESP]] sent back by Redis can be deserialized
      * into an instance of a B.
      *
      * Ensuring this is the case here guarantees that no instance of a [[Protocol]]
      * can '''ever''' be constructed unless proven that the response we eventually
      * will get back can be deserialized into the expected type B.
      *
      * @tparam A The sum/co-product type of response(s) expected from Redis
      * @tparam B The type we expect to convert an A into
      *
      * @return A fully-fledged [[Protocol]] for the provided [[Request]]/[[Response]]
      *         pair
      */
    final def asC[A, B](implicit R: RESPRead.Aux[A, B]): Protocol.Aux[B] = new Protocol {
      override final type A = B
      override final val codec: ProtocolCodec[B]  = new RESPProtocolCodec(R)
      override final val command: String          = self.command
      override final val parameters: Seq[GenBulk] = RESPParamWrite[L].write(l)
    }

    final def as[A, B](implicit ev: A <:!< Coproduct, R: RESPRead.Aux[A :+: CNil, B]): Protocol.Aux[B] = asC(R)

    final def opt[A](implicit ev: A <:!< Coproduct, gen: Generic[A]) = new PartiallyAppliedOptionalProtocol[L, gen.Repr](self)

    final def using[A <: Coproduct, B](R: RESPRead.Aux[A, B]): Protocol.Aux[B] = asC(R)
  }

  /** The only way a [[Protocol]] can be instantiated is through this partial application.
    *
    * This apply method requires the caller to provide the type of request parameters L this
    * [[Protocol]] expects to deal with when encoding the request parameters into a [[RESP]]
    * [[GenArr]] instance to send to Redis.
    *
    *
    */
  final def apply[L: RESPParamWrite](cmd: String, l: L): PartiallyAppliedProtocol[L] = new PartiallyAppliedProtocol(cmd, l) {}
}
