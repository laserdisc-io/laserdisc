package laserdisc
package protocol

trait ConnectionP {
  private[this] implicit final val pongRead: Str ==> PONG = Read.instance {
    case Str(PONG.`value`) => Right(PONG)
    case Str(other)        => Left(RESPDecErr(s"Wrong PONG encoding: was $other"))
  }

  final def auth(password: Key): Protocol.Aux[OK] = Protocol("AUTH", password).as[Str, OK]

  final def echo[A: Show: Bulk ==> *](message: A): Protocol.Aux[A] = Protocol("ECHO", message).as[Bulk, A]

  final val ping: Protocol.Aux[PONG]                               = Protocol("PING", Nil).as[Str, PONG]
  final def ping[A: Show: Bulk ==> *](message: A): Protocol.Aux[A] = Protocol("PING", message).as[Bulk, A]

  final val quit: Protocol.Aux[OK] = Protocol("QUIT", Nil).as[Str, OK]

  final def select(index: DbIndex): Protocol.Aux[OK] = Protocol("SELECT", index).as[Str, OK]

  final def swapdb(index1: DbIndex, index2: DbIndex): Protocol.Aux[OK] = Protocol("SWAPDB", index1 :: index2 :: Nil).as[Str, OK]
}
