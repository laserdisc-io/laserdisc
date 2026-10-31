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
package fs2

import cats.effect.IO
import cats.syntax.foldable.*
import munit.CatsEffectSuite

import scala.concurrent.duration.*

final class Fs2ProtocolHandlerSpec extends CatsEffectSuite {
  private val num: Protocol.Aux[Long]   = Protocol("NUM", EmptyTuple).as[Num, Long]
  private val str: Protocol.Aux[String] = Protocol("STR", EmptyTuple).as[Str, String]

  test("The fs2 protocol handler queues the requests of a send together, in order, and collects their responses") {
    val handler =
      implicitly[RedisHandler.Aux[IO, Protocol.Aux[Long] *: Protocol.Aux[String] *: EmptyTuple, Maybe[Long] *: Maybe[String] *: EmptyTuple]]

    for {
      queue <- Queue.unbounded[IO, Batch[IO]]
      fiber <- handler(queue -> 1.second, num *: str *: EmptyTuple).start
      batch <- queue.take
      _     <- batch.requests.traverse_(r => r.callback(r.protocol.decodeReplies(Seq(if (r.protocol eq num) Num(42L) else Str("ok")))))
      out   <- fiber.joinWithNever
    } yield {
      assertEquals(batch.requests.map(_.protocol.frames), Vector(Seq(Arr(Bulk("NUM"))), Seq(Arr(Bulk("STR")))))
      assertEquals(out, (Right(42L): Maybe[Long]) *: (Right("ok"): Maybe[String]) *: EmptyTuple)
    }
  }

  test("The fs2 protocol handler fails with RequestTimedOut and withdraws the requests when no response arrives in time") {
    val handler = implicitly[RedisHandler.Aux[IO, Protocol.Aux[Long] *: EmptyTuple, Maybe[Long] *: EmptyTuple]]

    for {
      queue     <- Queue.unbounded[IO, Batch[IO]]
      out       <- handler(queue -> 50.millis, num *: EmptyTuple).attempt
      batch     <- queue.take
      canBeSent <- batch.claim.complete(())
    } yield {
      assertEquals(out, Left(RequestTimedOut(num)))
      assert(!canBeSent, "a batch given up on should not be sent")
    }
  }

  test("The fs2 protocol handler names the first request without a response when it times out") {
    val handler =
      implicitly[RedisHandler.Aux[IO, Protocol.Aux[Long] *: Protocol.Aux[String] *: EmptyTuple, Maybe[Long] *: Maybe[String] *: EmptyTuple]]

    for {
      queue <- Queue.unbounded[IO, Batch[IO]]
      fiber <- handler(queue -> 200.millis, num *: str *: EmptyTuple).attempt.start
      batch <- queue.take
      _     <- batch.claim.complete(())
      first = batch.requests.head
      _   <- first.callback(first.protocol.decodeReplies(Seq(Num(42L))))
      out <- fiber.joinWithNever
    } yield assertEquals(out, Left(RequestTimedOut(str)))
  }

  test("The fs2 protocol handler can be inferred correctly") {
    implicitly[
      RedisHandler.Aux[
        IO,
        Protocol.Aux[OK] *: EmptyTuple,
        Maybe[OK] *: EmptyTuple
      ]
    ]

    implicitly[
      Handler.Aux[
        IO,
        Env[IO],
        Protocol.Aux[OK] *: EmptyTuple,
        Maybe[OK] *: EmptyTuple
      ]
    ]

    implicitly[
      Handler.Aux[
        IO,
        Env[IO],
        // format: off
        Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[Option[PosInt]] *:
          Protocol.Aux[Option[PosInt]] *:
          EmptyTuple,
        Maybe[OK] *:
          Maybe[OK] *:
          Maybe[Option[PosInt]] *:
          Maybe[Option[PosInt]] *:
          EmptyTuple
        // format: on
      ]
    ]

    implicitly[
      Handler.Aux[
        IO,
        Env[IO],
        // format: off
        Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[OK] *:
          Protocol.Aux[Option[PosInt]] *:
          Protocol.Aux[Option[PosInt]] *:
          Protocol.Aux[Option[PosInt]] *:
          Protocol.Aux[Option[PosInt]] *:
          Protocol.Aux[Option[PosInt]] *:
          Protocol.Aux[Option[PosInt]] *:
          Protocol.Aux[Option[PosInt]] *:
          Protocol.Aux[Option[PosInt]] *:
          Protocol.Aux[Option[PosInt]] *:
          Protocol.Aux[Option[PosInt]] *:
          EmptyTuple,
        Maybe[OK] *:
          Maybe[OK] *:
          Maybe[OK] *:
          Maybe[OK] *:
          Maybe[OK] *:
          Maybe[OK] *:
          Maybe[OK] *:
          Maybe[OK] *:
          Maybe[OK] *:
          Maybe[OK] *:
          Maybe[OK] *:
          Maybe[OK] *:
          Maybe[Option[PosInt]] *:
          Maybe[Option[PosInt]] *:
          Maybe[Option[PosInt]] *:
          Maybe[Option[PosInt]] *:
          Maybe[Option[PosInt]] *:
          Maybe[Option[PosInt]] *:
          Maybe[Option[PosInt]] *:
          Maybe[Option[PosInt]] *:
          Maybe[Option[PosInt]] *:
          Maybe[Option[PosInt]] *:
          EmptyTuple
        // format: on
      ]
    ]
  }
}
