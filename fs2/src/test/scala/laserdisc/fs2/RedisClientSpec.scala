package laserdisc.fs2

import java.nio.channels.AsynchronousChannelGroup
import java.nio.channels.AsynchronousChannelGroup.withThreadPool
import java.util.concurrent.Executors.newFixedThreadPool
import java.util.concurrent.ForkJoinPool

import cats.Monad
import cats.effect.{Concurrent, Effect, IO}
import cats.instances.list._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import fs2.{Scheduler, Stream}
import laserdisc.protocol.Show
import laserdisc.{Key, lists, strings}
import log.effect.fs2.Fs2LogWriter.noOpLogStream
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.ExecutionContext

/**
  * This test will be enabled back when the docker support
  * for the CI tests will be complete
  */
final class RedisClientSpec extends WordSpecLike with Matchers with BeforeAndAfterAll {

  override def beforeAll(): Unit = super.beforeAll()
  override def afterAll(): Unit = super.afterAll()

  val asyncChannelGroup: AsynchronousChannelGroup =
        withThreadPool(newFixedThreadPool(8))

  val redisClientPool: ExecutionContext =
    ExecutionContext.fromExecutor(new ForkJoinPool())

  def clientUnderTest[F[_]](implicit F: Effect[F]): Stream[F, RedisClient[F]] =
    (noOpLogStream[F] zip Scheduler[F](corePoolSize = 4)) flatMap {
      case (l, sch) => RedisClient[F](Set(RedisAddress("127.0.0.1", 6379)))(F, l, asyncChannelGroup, redisClientPool, sch)
    }

  "an fs2 redis client" ignore {

    "handle correctly hundreds of read requests in parallel for a large bulk text payload" in {

      val testKey     = Key.unsafeFrom("test-key")
      val testPayload = (1 to 1000).toList map (_ => "test text") mkString " - "

      def testPreset[F[_]](cl: RedisClient[F])(implicit F: Monad[F]): F[Unit] =
        cl.send1(strings.set(testKey, testPayload)) flatMap (_ => F.unit)

      def testRequests[F[_]](cl: RedisClient[F])(implicit F: Concurrent[F]): F[List[String]] =
        (((1 to 300) map { _ =>
          F.start { cl.send1(strings.get[String](testKey)) }
        }).par map { ioFib =>
          ioFib flatMap (_.join.attempt)
        } map {
          _.map(
            _.fold(_ => "", _.fold(_ => "", _.getOrElse("")))
          )
        }).toList.sequence

      val testResponses =
        (clientUnderTest[IO] evalMap {
          cl => testPreset(cl) *> testRequests(cl)
        }).compile.last map (
          _ getOrElse Nil
        ) unsafeRunSync()

      testResponses.size should be (300)
      testResponses map (_ should be (testPayload))
    }

    "handle correctly some read requests in a row for a bulk text payload" in {

      val testKey     = Key.unsafeFrom("test-key")
      val testPayload = (1 to 1000).toList map (_ => "test text") mkString " - "

      def testPreset[F[_]](cl: RedisClient[F])(implicit F: Monad[F]): F[Unit] =
        cl.send1(strings.set(testKey, testPayload)) flatMap (_ => F.unit)

      def testRequests[F[_]](cl: RedisClient[F])(implicit F: Concurrent[F]): F[List[String]] =
        ((1 to 50) map { _ =>
           cl.send1(strings.get[String](testKey)) map (
             _.fold(_ => "", _.getOrElse(""))
           )
        }).toList.sequence.attempt map (
          _.fold(_ => Nil, identity)
        )

      val testResponses =
        (clientUnderTest[IO] evalMap {
          cl => testPreset(cl) *> testRequests(cl)
        }).compile.last map (
          _ getOrElse Nil
        ) unsafeRunSync()

      testResponses.size should be (50)
      testResponses map (_ should be (testPayload))
    }

    "handle correctly hundreds of read requests in parallel for a small bulk text payload" in {

      val testKey     = Key.unsafeFrom("test-key")
      val testPayload = "test text"

      def testPreset[F[_]](cl: RedisClient[F])(implicit F: Monad[F]): F[Unit] =
        cl.send1(strings.set(testKey, testPayload)) flatMap (_ => F.unit)

      def testRequests[F[_]](cl: RedisClient[F])(implicit F: Concurrent[F]): F[List[String]] =
        (((1 to 1000) map { _ =>
          F.start { cl.send1(strings.get[String](testKey)) }
        }).par map { ioFib =>
          ioFib flatMap (_.join.attempt)
        } map {
          _.map(
            _.fold(_ => "", _.fold(_ => "", _.getOrElse("")))
          )
        }).toList.sequence

      val testResponses =
        (clientUnderTest[IO] evalMap {
          cl => testPreset(cl) *> testRequests(cl)
        }).compile.last map (
          _ getOrElse Nil
        ) unsafeRunSync()

      testResponses.size should be (1000)
      testResponses map (_ should be (testPayload))
    }

    "handle correctly hundreds of read requests in parallel for a null bulk payload" in {

      val testKey = Key.unsafeFrom("non-existent-test-key")

      def testRequests[F[_]](cl: RedisClient[F])(implicit F: Concurrent[F]): F[List[String]] =
        (((1 to 1000) map { _ =>
          F.start { cl.send1(strings.get[String](testKey)) }
        }).par map { ioFib =>
          ioFib flatMap (_.join.attempt)
        } map {
          _.map(
            _.fold(_ => "", _.fold(_ => "", _.getOrElse("Correct")))
          )
        }).toList.sequence

      val testResponses =
        (clientUnderTest[IO] evalMap (cl => testRequests(cl))).compile.last map (
          _ getOrElse Nil
        ) unsafeRunSync()

      testResponses.size should be (1000)
      testResponses map (_ should be ("Correct"))
    }

    "handle correctly hundreds of read requests in parallel for an array payload" in {

      implicit object myShow1 extends Show[List[String]] {
        final def show(a: List[String]): String = a mkString ","
      }

      val testKey  = Key.unsafeFrom("test-key-list")
      val testBulk = (1 to 1000).toList map (_ => "test text") mkString " - "

      def testCleanup[F[_]](cl: RedisClient[F])(implicit F: Monad[F]): F[Unit] =
        cl.send1(lists.lrem(testKey, 0L, testBulk)) flatMap (_ => F.unit)

      def testPreset[F[_]](cl: RedisClient[F])(implicit F: Monad[F]): F[Unit] =
        (1 to 100).map(_ => cl.send1(lists.rpush(testKey, testBulk :: Nil))).toList.sequence flatMap (_ => F.unit)

      def testRequests[F[_]](cl: RedisClient[F])(implicit F: Concurrent[F]): F[List[String]] =
        (((1 to 50) map { _ =>
          F.start { cl.send1(lists.lrange[String](testKey, 0L, 1000L)) }
        }).par map { ioFib =>
          ioFib flatMap (_.join.attempt)
        } map {
          _.map(
            _.fold(_ => Nil, _.fold(_ => Nil, _.toList))
          )
        }).toList.sequence map (_.flatten)

      val testResponses =
        (clientUnderTest[IO] evalMap {
          cl => testCleanup(cl) *> testPreset(cl) *> testRequests(cl)
        }).compile.last map (
          _ getOrElse Nil
        ) unsafeRunSync()

      testResponses.size should be (50 * 100)
      testResponses map (_ should be (testBulk))
    }
  }
}