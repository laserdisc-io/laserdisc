package laserdisc
package protocol

trait PublishP {
  import shapeless._

  //TODO work out all pub/sub system properly

  final def publish[A: Show](channel: Key, value: A): Protocol.Aux[NonNegInt] =
    Protocol("PUBLISH", channel :: value :: HNil).as[Num, NonNegInt]
}
