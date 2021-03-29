package laserdisc
package fs2

import java.util.concurrent.ForkJoinPool

import cats.effect._
import cats.effect.syntax.effect._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import laserdisc.all._
import laserdisc.auto._
import laserdisc.fs2.LaserdiscFs2ClientSpec.ClientSpecIo

import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor

final class RedisClientSpec extends ClientSpecIo(6379, "redis")
final class KeyDbClientSpec extends ClientSpecIo(6380, "keyDb")

object LaserdiscFs2ClientSpec {
  private[this] val ec: ExecutionContext = fromExecutor(new ForkJoinPool())

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)
  implicit val timer: Temporal[IO]            = IO.timer(ec)

  abstract class ClientSpecIo(p: Port, dest: String) extends LaserdiscFs2ClientSpec[IO](p, dest)
}

sealed abstract class LaserdiscFs2ClientSpec[F[_]: ContextShift: Temporal: ConcurrentEffect](p: Port, dest: String)
    extends LaserdiscFs2Suite[F](p) {

  def run[A]: F[A] => A = _.toIO.unsafeRunSync()

  val concurrent = Concurrent[F]

  private[this] final val key: Key = "test-key"
  private[this] final val text     = "test text"
  private[this] final val correct  = "correct"

  test(s"an fs2 $dest client handles correctly hundreds of read requests in parallel for a large bulk text payload") {
    val payload = List.fill(1000)(text).mkString(" - ")

    def preset(cl: RedisClient[F]): F[Unit] =
      cl.send(set(key, payload)) >>= (_ => concurrent.unit)

    def requests(cl: RedisClient[F]): F[List[String]] =
      (collection.parallel.immutable.ParSeq.range(0, 300) map { _ =>
        concurrent.start(cl.send(get[String](key)): F[Maybe[Option[String]]])
      } map { ioFib =>
        ioFib >>= (_.join.attempt)
      } map {
        _.map(
          _.fold(
            _ => "",
            _.fold(
              _ => "",
              _.getOrElse("")
            )
          )
        )
      }).toList.sequence

    val responses = run(
      preset(client) *> requests(client)
    )

    assertEquals(responses.size, 300)
    assertAllEqual(responses, payload)
  }

  test(s"an fs2 $dest client handles correctly some read requests in a row for a bulk text payload") {
    val payload = List.fill(1000)(text).mkString(" - ")

    def preset(cl: RedisClient[F]): F[Unit] =
      cl.send(set(key, payload)) >>= (_ => concurrent.unit)

    def requests(cl: RedisClient[F]): F[List[String]] =
      ((1 to 50) map { _ =>
        cl.send(get[String](key)) map (
          _.fold(
            _ => "",
            _.getOrElse("")
          )
        )
      }).toList.sequence.attempt map (
        _.fold(
          _ => Nil,
          identity
        )
      )

    val responses = run(
      preset(client) *> requests(client)
    )

    assertEquals(responses.size, 50)
    assertAllEqual(responses, payload)
  }

  test(s"an fs2 $dest client handles correctly hundreds of read requests in parallel for a small bulk text payload") {
    val payload = "test text"

    def preset(cl: RedisClient[F]): F[Unit] =
      cl.send(set(key, payload)) >>= (_ => concurrent.unit)

    def requests(cl: RedisClient[F]): F[List[String]] =
      (ParSeq.range(0, 1000) map { _ =>
        concurrent.start(cl.send(get[String](key)))
      } map { ioFib =>
        ioFib >>= (_.join.attempt)
      } map {
        _.map(
          _.fold(
            _ => "",
            _.fold(
              _ => "",
              _.getOrElse("")
            )
          )
        )
      }).toList.sequence

    val responses = run(
      preset(client) *> requests(client)
    )

    assertEquals(responses.size, 1000)
    assertAllEqual(responses, payload)
  }

  test(s"an fs2 $dest client handles correctly hundreds of read requests in parallel for a null bulk payload") {
    val key: Key = "non-existent-test-key"

    def requests(cl: RedisClient[F]): F[List[String]] =
      (ParSeq.range(0, 1000) map { _ =>
        concurrent.start(cl.send(get[String](key)))
      } map { ioFib =>
        ioFib >>= (_.join.attempt)
      } map {
        _.map(
          _.fold(
            _ => "",
            _.fold(
              _ => "",
              _.getOrElse(correct)
            )
          )
        )
      }).toList.sequence

    val responses = (run compose requests)(client)

    assertEquals(responses.size, 1000)
    assertAllEqual(responses, correct)
  }

  test(s"an fs2 $dest client handles correctly hundreds of read requests in parallel for an array payload") {
    implicit val listStringShow: Show[List[String]] = _ mkString COMMA

    val key: Key = "test-key-list"
    val bulk     = List.fill(1000)(text).mkString(" - ")

    def cleanup(cl: RedisClient[F]): F[Unit] =
      cl.send(lists.lrem(key, 0L, bulk)) >>= (_ => concurrent.unit)

    def preset(cl: RedisClient[F]): F[Unit] =
      (1 to 100).map(_ => cl.send(lists.rpush(key, bulk :: Nil))).toList.sequence >>= (_ => concurrent.unit)

    def requests(cl: RedisClient[F]): F[List[String]] =
      (ParSeq.range(0, 50) map { _ =>
        concurrent.start(cl.send(lists.lrange[String](key, 0L, 1000L)))
      } map { ioFib =>
        ioFib >>= (_.join.attempt)
      } map {
        _.map(
          _.fold(
            _ => Nil,
            _.fold(
              _ => Nil,
              _.toList
            )
          )
        )
      }).toList.sequence map (_.flatten)

    val responses = run(
      cleanup(client) *> preset(client) *> requests(client)
    )

    assertEquals(responses.size, 50 * 100)
    assertAllEqual(responses, bulk)
  }
}
