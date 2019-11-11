package laserdisc
package fs2

import java.util.concurrent.ForkJoinPool

import cats.effect._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import laserdisc.auto._
import log.effect.LogWriter
import log.effect.fs2.SyncLogWriter.noOpLog
import org.scalatest.{Matchers, WordSpecLike}

import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor

final class RedisClientSpec extends RedisClientBaseSpec[IO] {
  private[this] val ec: ExecutionContext = fromExecutor(new ForkJoinPool())

  override implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)
  override implicit val timer: Timer[IO]               = IO.timer(ec)
  override implicit val concurrent: Concurrent[IO]     = IO.ioConcurrentEffect
  override implicit val logger: LogWriter[IO]          = noOpLog[IO]

  override def run[A]: IO[A] => A = _.unsafeRunSync()
}

abstract class RedisClientBaseSpec[F[_]] extends WordSpecLike with Matchers {
  implicit def contextShift: ContextShift[F]
  implicit def timer: Timer[F]
  implicit def concurrent: Concurrent[F]
  implicit def logger: LogWriter[F]

  def run[A]: F[A] => A

  private[this] final val key: Key = "test-key"
  private[this] final val text     = "test text"
  private[this] final val correct  = "correct"

  def clientUnderTest: Resource[F, RedisClient[F]] =
    RedisClient.toNode("127.0.0.1", 6379)

  "an fs2 redis client" should {
    import cats.instances.list.catsStdInstancesForList

    "handle correctly hundreds of read requests in parallel for a large bulk text payload" in {
      val payload = (1 to 1000).toList map (_ => text) mkString " - "

      def preset(cl: RedisClient[F]): F[Unit] =
        cl.send(strings.set(key, payload)) >>= (_ => concurrent.unit)

      def requests(cl: RedisClient[F]): F[List[String]] =
        (collection.parallel.immutable.ParSeq.range(0, 300) map { _ =>
          concurrent.start { cl.send(strings.get[String](key)): F[Maybe[Option[String]]] }
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

      val responses =
        (run[List[String]] compose clientUnderTest.use) { cl =>
          preset(cl) *> requests(cl)
        }

      responses.size should be(300)
      responses map (_ should be(payload))
    }

    "handle correctly some read requests in a row for a bulk text payload" in {
      val payload = (1 to 1000).toList map (_ => text) mkString " - "

      def preset(cl: RedisClient[F]): F[Unit] =
        cl.send(strings.set(key, payload)) >>= (_ => concurrent.unit)

      def requests(cl: RedisClient[F]): F[List[String]] =
        ((1 to 50) map { _ =>
          cl.send(strings.get[String](key)) map (
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

      val responses =
        (run[List[String]] compose clientUnderTest.use) { cl =>
          preset(cl) *> requests(cl)
        }

      responses.size should be(50)
      responses map (_ should be(payload))
    }

    "handle correctly hundreds of read requests in parallel for a small bulk text payload" in {
      val payload = "test text"

      def preset(cl: RedisClient[F]): F[Unit] =
        cl.send(strings.set(key, payload)) >>= (_ => concurrent.unit)

      def requests(cl: RedisClient[F]): F[List[String]] =
        (ParSeq.range(0, 1000) map { _ =>
          concurrent.start { cl.send(strings.get[String](key)) }
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

      val responses =
        (run[List[String]] compose clientUnderTest.use) { cl =>
          preset(cl) *> requests(cl)
        }

      responses.size should be(1000)
      responses map (_ should be(payload))
    }

    "handle correctly hundreds of read requests in parallel for a null bulk payload" in {
      val key: Key = "non-existent-test-key"

      def requests(cl: RedisClient[F]): F[List[String]] =
        (ParSeq.range(0, 1000) map { _ =>
          concurrent.start { cl.send(strings.get[String](key)) }
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

      val responses =
        (run[List[String]] compose clientUnderTest.use) { cl =>
          requests(cl)
        }

      responses.size should be(1000)
      responses map (_ should be(correct))
    }

    "handle correctly hundreds of read requests in parallel for an array payload" in {
      implicit val listStringShow: Show[List[String]] = _ mkString COMMA

      val key: Key = "test-key-list"
      val bulk     = (1 to 1000).toList map (_ => text) mkString " - "

      def cleanup(cl: RedisClient[F]): F[Unit] =
        cl.send(lists.lrem(key, 0L, bulk)) >>= (_ => concurrent.unit)

      def preset(cl: RedisClient[F]): F[Unit] =
        (1 to 100).map(_ => cl.send(lists.rpush(key, bulk :: Nil))).toList.sequence >>= (_ => concurrent.unit)

      def requests(cl: RedisClient[F]): F[List[String]] =
        (ParSeq.range(0, 50) map { _ =>
          concurrent.start { cl.send(lists.lrange[String](key, 0L, 1000L)) }
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

      val responses =
        (run[List[String]] compose clientUnderTest.use) { cl =>
          cleanup(cl) *> preset(cl) *> requests(cl)
        }

      responses.size should be(50 * 100)
      responses map (_ should be(bulk))
    }
  }
}
