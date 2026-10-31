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

import cats.effect.*
import cats.syntax.traverse.*
import laserdisc.all.*
import laserdisc.auto.*
import laserdisc.fs2.LaserdiscFs2ClientSpec.ClientSpecIo

import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.duration.*

final class RedisClientSpec  extends ClientSpecIo(6379, "redis")
final class KeyDbClientSpec  extends ClientSpecIo(6380, "keyDb")
final class ValkeyClientSpec extends ClientSpecIo(6381, "valkey")

object LaserdiscFs2ClientSpec {
  abstract class ClientSpecIo(p: Port, dest: String) extends LaserdiscFs2ClientSpec(p, dest)
}

sealed abstract class LaserdiscFs2ClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  private def preset(cl: RedisClient[IO], payload: String): IO[Unit] =
    cl.send(set(key, payload)).flatMap(_ => async.unit)

  private def run[A]: IO[A] => A = _.unsafeRunSync()(using runtime)

  private val async = Async[IO]

  private val cancellationError = IO.raiseError(new Exception("Test failed. Requests interrupted"))

  private final val key: Key = "test-key"
  private final val text     = "test text"
  private final val correct  = "correct"

  private final val payloadSize        = 1000
  private final val requestsInParallel = 500
  private final val requestsInSequence = 50

  override val munitTimeout: FiniteDuration = 2.minutes

  test(s"an fs2 $dest client handles correctly hundreds of read requests in parallel for a large bulk text payload") {
    val payload = List.fill(payloadSize)(text).mkString(" - ")

    def requests(cl: RedisClient[IO]): IO[List[String]] =
      collection.parallel.immutable.ParSeq
        .range(0, requestsInParallel)
        .map(_ => async.start(cl.send(get[String](key))))
        .map(ioFib => ioFib.flatMap(_.joinWith(cancellationError).attempt))
        .map(
          _.map(
            _.fold(
              _ => "",
              _.fold(_ => "", _.getOrElse(""))
            )
          )
        )
        .toList
        .sequence

    val responses = run(
      preset(client, payload) *> requests(client)
    )

    assertEquals(responses.size, requestsInParallel)
    assertAllEqual(responses, payload)
  }

  test(s"an fs2 $dest client handles correctly some read requests in a row for a bulk text payload") {
    val payload = List.fill(payloadSize)(text).mkString(" - ")

    def requests(cl: RedisClient[IO]): IO[List[String]] =
      (1 to requestsInSequence)
        .map(_ => cl.send(get[String](key)))
        .map(
          _.map(_.fold(_ => "", _.getOrElse("")))
        )
        .toList
        .sequence
        .attempt
        .map(_.fold(_ => Nil, identity))

    val responses = run(
      preset(client, payload) *> requests(client)
    )

    assertEquals(responses.size, requestsInSequence)
    assertAllEqual(responses, payload)
  }

  test(s"an fs2 $dest client handles correctly hundreds of read requests in parallel for a small bulk text payload") {
    val payload = "test text"

    def requests(cl: RedisClient[IO]): IO[List[String]] =
      ParSeq
        .range(0, requestsInParallel)
        .map(_ => async.start(cl.send(get[String](key))))
        .map(ioFib => ioFib.flatMap(_.joinWith(cancellationError).attempt))
        .map(
          _.map(
            _.fold(
              _ => "",
              _.fold(
                _ => "",
                _.getOrElse("")
              )
            )
          )
        )
        .toList
        .sequence

    val responses = run(
      preset(client, payload) *> requests(client)
    )

    assertEquals(responses.size, requestsInParallel)
    assertAllEqual(responses, payload)
  }

  test(s"an fs2 $dest client handles correctly hundreds of read requests in parallel for a null bulk payload") {
    val key: Key = "non-existent-test-key"

    def requests(cl: RedisClient[IO]): IO[List[String]] =
      ParSeq
        .range(0, requestsInParallel)
        .map(_ => async.start(cl.send(get[String](key))))
        .map(ioFib => ioFib.flatMap(_.joinWith(cancellationError).attempt))
        .map(
          _.map(
            _.fold(
              _ => "",
              _.fold(_ => "", _.getOrElse(correct))
            )
          )
        )
        .toList
        .sequence

    val responses = (run compose requests)(client)

    assertEquals(responses.size, requestsInParallel)
    assertAllEqual(responses, correct)
  }

  test(s"an fs2 $dest client handles correctly hundreds of read requests in parallel for an array payload") {
    implicit val listStringShow: Show[List[String]] = _ mkString COMMA

    val key: Key = "test-key-list"
    val bulk     = List.fill(payloadSize)(text).mkString(" - ")

    def cleanup(cl: RedisClient[IO]): IO[Unit] =
      cl.send(lists.lrem(key, 0L, bulk)).flatMap(_ => async.unit)

    def preset(cl: RedisClient[IO]): IO[Unit] =
      (1 to requestsInSequence).map(_ => cl.send(lists.rpush(key, bulk :: Nil))).toList.sequence.flatMap(_ => async.unit)

    def requests(cl: RedisClient[IO]): IO[List[String]] =
      ParSeq
        .range(0, requestsInParallel)
        .map(_ => async.start(cl.send(lists.lrange[String](key, 0L, Index(payloadSize)))))
        .map(ioFib => ioFib.flatMap(_.joinWith(cancellationError).attempt))
        .map(
          _.map(
            _.fold(
              _ => Nil,
              _.fold(_ => Nil, _.toList)
            )
          )
        )
        .toList
        .sequence
        .map(_.flatten)

    val responses = run(
      cleanup(client) *> preset(client) *> requests(client)
    )

    assertEquals(responses.size, requestsInSequence * requestsInParallel)
    assertAllEqual(responses, bulk)
  }

  test(s"an fs2 $dest client runs the commands of a multi and returns their results") {
    val k: Key = "multi-key"
    val out    = run(client.send(multi(set(k, "1"), incr[Long](k), get[String](k))))

    assertEquals(out, Right(Some((Right(OK), Right(2L), Right(Some("2"))))))
  }

  test(s"an fs2 $dest client returns None for a multi when a watched key changes") {
    val k: Key = "multi-watched-key"

    val out = run(
      RedisClient[IO].to("127.0.0.1", p).use { other =>
        for {
          _   <- client.send(watch(OneOrMoreKeys.unsafeFrom(List(k))))
          _   <- other.send(set(k, "changed"))
          out <- client.send(multi(set(k, "mine")))
          now <- client.send(get[String](k))
        } yield out -> now
      }
    )

    assertEquals(out, Right(None) -> Right(Some("changed")))
  }

  test(s"an fs2 $dest client returns the reason when a command of a multi is rejected") {
    val k: Key = "multi-rejected-key"
    val bad    = Protocol("NOSUCHCOMMAND", EmptyTuple).as[Str, OK]

    run(client.send(multi(set(k, "1"), bad), get[String](k))) match {
      case (Left(Err(message)), unchanged) =>
        assert(message.contains("NOSUCHCOMMAND") || message.contains("nosuchcommand"), message)
        assertEquals(unchanged, Right(None))
      case other => fail(s"expected the rejection, got $other")
    }
  }

  test(s"an fs2 $dest client returns each command's own failure inside a multi") {
    val k: Key = "multi-failing-key"

    run(client.send(multi(set(k, "not a number"), incr[Long](k)))) match {
      case Right(Some((Right(OK), Left(_: Err)))) => ()
      case other                                  => fail(s"expected OK and a failed incr, got $other")
    }
  }
}
