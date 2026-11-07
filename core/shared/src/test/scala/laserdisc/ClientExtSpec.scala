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

import laserdisc.auto.*
import laserdisc.redis_6_2.*

import scala.concurrent.duration.*

/** The overloads of `send` taking each command on its own give each one's reply, in the order they were passed */
final class ClientExtSpec extends munit.FunSuite {
  private type Id[A] = A

  private implicit val idFunctor: Functor[Id] = new Functor[Id] {
    override final def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  }

  private implicit val noCommands: Handler.Aux[Id, Unit, EmptyTuple, EmptyTuple] = new Handler[Id, Unit, EmptyTuple] {
    override final type LOut = EmptyTuple
    override final def apply(env: Unit, in: EmptyTuple): EmptyTuple = EmptyTuple
  }

  // replies to each command with its position, from the last one
  private implicit def command[A, T <: Tuple, O <: Tuple](
      implicit T: Handler.Aux[Id, Unit, T, O]
  ): Handler.Aux[Id, Unit, Protocol.Aux[A] *: T, Maybe[A] *: O] =
    new Handler[Id, Unit, Protocol.Aux[A] *: T] {
      override final type LOut = Maybe[A] *: O
      override final def apply(env: Unit, in: Protocol.Aux[A] *: T): Maybe[A] *: O = in match {
        case protocol *: rest => protocol.decodeReplies(Seq(Bulk("1"))) *: T(env, rest)
      }
    }

  private val client: Client[Id, Unit] = new Client[Id, Unit] {
    override final def send[In <: Tuple, Out <: Tuple](in: In, timeout: FiniteDuration)(
        implicit handler: Handler.Aux[Id, Unit, In, Out]
    ): Out =
      handler((), in)
  }

  private val p                     = get[Int]("k")
  private val r: Maybe[Option[Int]] = Right(Some(1))

  //format: off
  test("send gives the replies of 2 to 8 commands") {
    assertEquals(client.send(p, p), (r, r))
    assertEquals(client.send(p, p, 1.second), (r, r))
    assertEquals(client.send(p, p, p), (r, r, r))
    assertEquals(client.send(p, p, p, 1.second), (r, r, r))
    assertEquals(client.send(p, p, p, p), (r, r, r, r))
    assertEquals(client.send(p, p, p, p, 1.second), (r, r, r, r))
    assertEquals(client.send(p, p, p, p, p), (r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, 1.second), (r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p), (r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, 1.second), (r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p), (r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r))
  }
  test("send gives the replies of 9 to 15 commands") {
    assertEquals(client.send(p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
  }
  test("send gives the replies of 16 to 22 commands") {
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
    assertEquals(client.send(p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, 1.second), (r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r))
  }
  //format: on
}
