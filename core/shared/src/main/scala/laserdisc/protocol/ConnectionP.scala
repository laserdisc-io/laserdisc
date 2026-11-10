/*
 * Copyright (c) 2018-2026 LaserDisc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package laserdisc
package protocol

import Protocol.Single

trait ConnectionBaseP extends Features {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "AUTH",
      "ECHO",
      "HELLO: AUTH SETNAME",
      "PING",
      "QUIT",
      "SELECT"
    )

  private implicit final val pongRead: Read[Str, PONG] = Read.instance {
    case Str(pong) if pong == PONG.value => Right(PONG)
    case Str(other)                      => Left(RESPDecErr(s"Wrong PONG encoding: was $other"))
  }

  final def auth(password: Key): Single[OK] = Protocol("AUTH", password).as[Str, OK]

  final def echo[A: Show: Read[Bulk, _]](message: A): Single[A] = Protocol("ECHO", message).as[Bulk, A]

  /** What the server says about itself and this connection, optionally logging in and naming the connection first. It always asks for
    * protocol 2, the one laserdisc reads
    */
  val hello: Single[ServerInspection.HelloCore]                               = helloWith(Nil, helloCore)
  def hello(username: Key, password: Key): Single[ServerInspection.HelloCore] =
    helloWith(List("AUTH", username.value, password.value), helloCore)
  def hello(name: ConnectionName): Single[ServerInspection.HelloCore] = helloWith(List("SETNAME", name.value), helloCore)
  def hello(username: Key, password: Key, name: ConnectionName): Single[ServerInspection.HelloCore] =
    helloWith(List("AUTH", username.value, password.value, "SETNAME", name.value), helloCore)

  private final lazy val helloCore = RESPRead.instance(ServerInspection.helloCore)

  protected final def helloWith[H](params: List[String], read: RESPRead[H]): Single[H] = Protocol("HELLO", "2" :: params).using(read)

  final val ping: Single[PONG]                                  = Protocol("PING", EmptyTuple).as[Str, PONG]
  final def ping[A: Show: Read[Bulk, _]](message: A): Single[A] = Protocol("PING", message).as[Bulk, A]

  final val quit: Single[OK] = Protocol("QUIT", EmptyTuple).as[Str, OK]

  final def select(index: DbIndex): Single[OK] = Protocol("SELECT", index).as[Str, OK]
}

trait ConnectionSwapDbP extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("SWAPDB")

  final def swapdb(index1: DbIndex, index2: DbIndex): Single[OK] = Protocol("SWAPDB", index1 :: index2 :: Nil).as[Str, OK]
}

/** HELLO with the server's modules: Redis 6.2 */
trait ConnectionHello6_2P extends ConnectionBaseP {
  import ServerInspection.Hello6_2

  private final lazy val read6_2 = RESPRead.instance(ServerInspection.hello6_2)

  override val hello: Single[Hello6_2]                               = helloWith(Nil, read6_2)
  override def hello(username: Key, password: Key): Single[Hello6_2] =
    helloWith(List("AUTH", username.value, password.value), read6_2)
  override def hello(name: ConnectionName): Single[Hello6_2]                               = helloWith(List("SETNAME", name.value), read6_2)
  override def hello(username: Key, password: Key, name: ConnectionName): Single[Hello6_2] =
    helloWith(List("AUTH", username.value, password.value, "SETNAME", name.value), read6_2)
}

/** HELLO with each module's path and arguments: Redis 7.0 */
trait ConnectionHello7_0P extends ConnectionHello6_2P {
  import ServerInspection.Hello7_0

  private final lazy val read7_0 = RESPRead.instance(ServerInspection.hello7_0)

  override val hello: Single[Hello7_0]                               = helloWith(Nil, read7_0)
  override def hello(username: Key, password: Key): Single[Hello7_0] =
    helloWith(List("AUTH", username.value, password.value), read7_0)
  override def hello(name: ConnectionName): Single[Hello7_0]                               = helloWith(List("SETNAME", name.value), read7_0)
  override def hello(username: Key, password: Key, name: ConnectionName): Single[Hello7_0] =
    helloWith(List("AUTH", username.value, password.value, "SETNAME", name.value), read7_0)
}

trait ConnectionP extends ConnectionBaseP with ConnectionSwapDbP with ConnectionHello7_0P
