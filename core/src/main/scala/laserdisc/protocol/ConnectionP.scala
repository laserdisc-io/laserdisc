package laserdisc
package protocol

trait ConnectionP {
  import Read.==>

  private[this] implicit final val pongRead: SimpleString ==> PONG = Read.instancePF {
    case SimpleString("PONG") => PONG
  }

  final def auth(password: Key): Protocol.Aux[OK] = Protocol("AUTH", password).as[SimpleString, OK]

  final def echo[A: Show: NonNullBulkString ==> ?](message: A): Protocol.Aux[A] =
    Protocol("ECHO", message).as[NonNullBulkString, A]

  final val ping: Protocol.Aux[PONG] = Protocol("PING", Nil).as[SimpleString, PONG]

  final def ping[A: Show: NonNullBulkString ==> ?](message: A): Protocol.Aux[A] =
    Protocol("PING", message).as[NonNullBulkString, A]

  final val quit: Protocol.Aux[OK] = Protocol("QUIT", Nil).as[SimpleString, OK]

  final def select(index: DbIndex): Protocol.Aux[OK] = Protocol("SELECT", index).as[SimpleString, OK]

  final def swapdb(index1: DbIndex, index2: DbIndex): Protocol.Aux[OK] =
    Protocol("SWAPDB", index1 :: index2 :: Nil).as[SimpleString, OK]
}
