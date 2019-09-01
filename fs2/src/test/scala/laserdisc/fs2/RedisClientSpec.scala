package laserdisc
package fs2

import java.util.concurrent.ForkJoinPool

import cats.Monad
import cats.effect._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import laserdisc.auto._
import log.effect.fs2.Fs2LogWriter.noOpLogStreamF
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor

/**
  * This test will be enabled back when the docker support
  * for the CI tests will be complete
  */
final class RedisClientSpec extends WordSpecLike with Matchers with BeforeAndAfterAll {

  override def beforeAll(): Unit = super.beforeAll()
  override def afterAll(): Unit  = super.afterAll()

  implicit val ec: ExecutionContext           = fromExecutor(new ForkJoinPool())
  implicit val timer: Timer[IO]               = IO.timer(ec)
  implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)

  private[this] final val testKey  = Key("test-key")
  private[this] final val testText = "test text"
  private[this] final val correct  = "correct"

  def clientUnderTest[F[_]: ContextShift: Timer](implicit F: ConcurrentEffect[F]): Stream[F, RedisClient[F]] =
    noOpLogStreamF >>= { implicit log =>
      RedisClient[F](Set(RedisAddress("127.0.0.1", 6379)))(Blocker[F])
    }

  "an fs2 redis client" should {

    import cats.instances.list.catsStdInstancesForList

    "handle correctly hundreds of read requests in parallel for a large bulk text payload" in {

      val testPayload = (1 to 1000).toList map (_ => testText) mkString " - "

      def testPreset[F[_]](cl: RedisClient[F])(implicit F: Monad[F]): F[Unit] =
        cl.send1(strings.set(testKey, testPayload)) >>= (_ => F.unit)

      def testRequests[F[_]](cl: RedisClient[F])(implicit F: Concurrent[F]): F[List[String]] =
        (collection.parallel.immutable.ParSeq.range(0, 300) map { _ =>
          F.start { cl.send1(strings.get[String](testKey)) }
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

      val testResponses =
        (clientUnderTest[IO] evalMap { cl =>
          testPreset(cl) *> testRequests(cl)
        }).compile.last
          .map(
            _ getOrElse Nil
          )
          .unsafeRunSync()

      testResponses.size should be(300)
      testResponses map (_ should be(testPayload))
    }

    "handle correctly some read requests in a row for a bulk text payload" in {

      val testPayload = (1 to 1000).toList map (_ => testText) mkString " - "

      def testPreset[F[_]](cl: RedisClient[F])(implicit F: Monad[F]): F[Unit] =
        cl.send1(strings.set(testKey, testPayload)) >>= (_ => F.unit)

      def testRequests[F[_]](cl: RedisClient[F])(implicit F: Concurrent[F]): F[List[String]] =
        ((1 to 50) map { _ =>
          cl.send1(strings.get[String](testKey)) map (
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

      val testResponses =
        (clientUnderTest[IO] evalMap { cl =>
          testPreset(cl) *> testRequests(cl)
        }).compile.last
          .map(
            _ getOrElse Nil
          )
          .unsafeRunSync()

      testResponses.size should be(50)
      testResponses map (_ should be(testPayload))
    }

    "handle correctly hundreds of read requests in parallel for a small bulk text payload" in {

      val testPayload = "test text"

      def testPreset[F[_]](cl: RedisClient[F])(implicit F: Monad[F]): F[Unit] =
        cl.send1(strings.set(testKey, testPayload)) >>= (_ => F.unit)

      def testRequests[F[_]](cl: RedisClient[F])(implicit F: Concurrent[F]): F[List[String]] =
        (collection.parallel.immutable.ParSeq.range(0, 1000) map { _ =>
          F.start { cl.send1(strings.get[String](testKey)) }
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

      val testResponses =
        (clientUnderTest[IO] evalMap { cl =>
          testPreset(cl) *> testRequests(cl)
        }).compile.last
          .map(
            _ getOrElse Nil
          )
          .unsafeRunSync()

      testResponses.size should be(1000)
      testResponses map (_ should be(testPayload))
    }

    "handle correctly hundreds of read requests in parallel for a null bulk payload" in {

      val testKey = Key("non-existent-test-key")

      def testRequests[F[_]](cl: RedisClient[F])(implicit F: Concurrent[F]): F[List[String]] =
        (collection.parallel.immutable.ParSeq.range(0, 1000) map { _ =>
          F.start { cl.send1(strings.get[String](testKey)) }
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

      val testResponses =
        (clientUnderTest[IO] evalMap (cl => testRequests(cl))).compile.last
          .map(
            _ getOrElse Nil
          )
          .unsafeRunSync()

      testResponses.size should be(1000)
      testResponses map (_ should be(correct))
    }

    "handle correctly hundreds of read requests in parallel for an array payload" in {

      implicit object myShow1 extends Show[List[String]] {
        final def show(a: List[String]): String = a mkString COMMA
      }

      val testKey  = Key("test-key-list")
      val testBulk = (1 to 1000).toList map (_ => testText) mkString " - "

      def testCleanup[F[_]](cl: RedisClient[F])(implicit F: Monad[F]): F[Unit] =
        cl.send1(lists.lrem(testKey, 0L, testBulk)) >>= (_ => F.unit)

      def testPreset[F[_]](cl: RedisClient[F])(implicit F: Monad[F]): F[Unit] =
        (1 to 100).map(_ => cl.send1(lists.rpush(testKey, testBulk :: Nil))).toList.sequence >>= (_ => F.unit)

      def testRequests[F[_]](cl: RedisClient[F])(implicit F: Concurrent[F]): F[List[String]] =
        (collection.parallel.immutable.ParSeq.range(0, 50) map { _ =>
          F.start { cl.send1(lists.lrange[String](testKey, 0L, 1000L)) }
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

      val testResponses =
        (clientUnderTest[IO] evalMap { cl =>
          testCleanup(cl) *> testPreset(cl) *> testRequests(cl)
        }).compile.last
          .map(
            _ getOrElse Nil
          )
          .unsafeRunSync()

      testResponses.size should be(50 * 100)
      testResponses map (_ should be(testBulk))
    }
  }
}
