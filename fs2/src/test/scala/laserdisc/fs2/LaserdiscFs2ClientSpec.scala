package laserdisc
package fs2

import cats.effect._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import laserdisc.all._
import laserdisc.auto._
import laserdisc.fs2.LaserdiscFs2ClientSpec.ClientSpecIo

import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.duration._

final class RedisClientSpec extends ClientSpecIo(6379, "redis")
final class KeyDbClientSpec extends ClientSpecIo(6380, "keyDb")

object LaserdiscFs2ClientSpec {
  abstract class ClientSpecIo(p: Port, dest: String) extends LaserdiscFs2ClientSpec(p, dest)
}

sealed abstract class LaserdiscFs2ClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  private def preset(cl: RedisClient[IO], payload: String): IO[Unit] =
    cl.send(set(key, payload)) >>= (_ => async.unit)

  private def run[A]: IO[A] => A = _.unsafeRunSync()(runtime)

  private val async = Async[IO]

  private val cancellationError = IO.raiseError(new Exception("Test failed. Requests interrupted"))

  private[this] final val key: Key = "test-key"
  private[this] final val text     = "test text"
  private[this] final val correct  = "correct"

  private[this] final val payloadSize        = 1000
  private[this] final val requestsInParallel = 500
  private[this] final val requestsInSequence = 50

  override val munitTimeout = 2.minutes

  test(s"an fs2 $dest client handles correctly hundreds of read requests in parallel for a large bulk text payload") {
    val payload = List.fill(payloadSize)(text).mkString(" - ")

    def requests(cl: RedisClient[IO]): IO[List[String]] =
      collection.parallel.immutable.ParSeq
        .range(0, requestsInParallel)
        .map(_ => async.start(cl.send(get[String](key))))
        .map(ioFib => ioFib >>= (_.joinWith(cancellationError).attempt))
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
        .map(ioFib => ioFib >>= (_.joinWith(cancellationError).attempt))
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
        .map(ioFib => ioFib >>= (_.joinWith(cancellationError).attempt))
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
      cl.send(lists.lrem(key, 0L, bulk)) >>= (_ => async.unit)

    def preset(cl: RedisClient[IO]): IO[Unit] =
      (1 to requestsInSequence).map(_ => cl.send(lists.rpush(key, bulk :: Nil))).toList.sequence >>= (_ => async.unit)

    def requests(cl: RedisClient[IO]): IO[List[String]] =
      ParSeq
        .range(0, requestsInParallel)
        .map(_ => async.start(cl.send(lists.lrange[String](key, 0L, Index(payloadSize)))))
        .map(ioFib => ioFib >>= (_.joinWith(cancellationError).attempt))
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
}
