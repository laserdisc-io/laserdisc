package laserdisc
package fs2

import java.util.UUID
import java.util.concurrent.{Executors, TimeUnit}

import cats.Parallel
import cats.effect.syntax.concurrent._
import cats.effect.{Concurrent, ContextShift, IO, Resource, Timer}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.parallel._
import laserdisc.all._
import laserdisc.auto._
import laserdisc.fs2.Setup.{CatsSetUp, JedisSetUp, ZioSetUp}
import log.effect.fs2.SyncLogWriter
import log.effect.zio.ZioLogWriter
import log.effect.{LogLevels, LogWriter}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import redis.clients.jedis.Jedis

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.fromExecutor

class ParallelLoadBench() {
  @Benchmark
  @OperationsPerInvocation(40)
  def laserdiscParallelLoadCats1(setUp: CatsSetUp, bh: Blackhole): Unit = {
    import setUp._

    val run = setUp.case1(setUp.laserdiscClient)
    val res = run.unsafeRunSync()

    bh.consume(res)
  }

  @Benchmark
  @OperationsPerInvocation(40)
  def laserdiscParallelLoadZio1(setUp: ZioSetUp, bh: Blackhole): Unit = {
    import zio.interop.catz._
    import zio.interop.catz.implicits._

    val run = setUp.case1(setUp.laserdiscClient)
    val res = setUp.runtime.unsafeRunSync(run)

    bh.consume(res)
  }

  /**
    * This test cannot run at the moment as Jedis uses
    * an hardcoded RedisOutputStream buffer size of 8192
    */
  @Benchmark
  @OperationsPerInvocation(40)
  def laserdiscParallelLoadJedis1(setUp: JedisSetUp, bh: Blackhole): Unit = {
    import setUp._

    val run = setUp.case1[IO](setUp.jedisClient)
    val res = run.unsafeRunSync()

    bh.consume(res)
  }
}

object Setup {

  @State(Scope.Benchmark)
  class CatsSetUp extends TestCases {
    private[this] val commandsService           = Executors.newFixedThreadPool(8)
    private[this] val ec: ExecutionContext      = fromExecutor(commandsService)
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)
    implicit val timer: Timer[IO]               = IO.timer(ec)
    implicit val logWriter: LogWriter[IO]       = SyncLogWriter.consoleLogUpToLevel(LogLevels.Error)

    val resource = RedisClient.to("localhost", 6379)

    var laserdiscClient: RedisClient[IO] = _
    var clientCleanUp: IO[Unit]          = _

    @Setup(Level.Trial)
    def setup(): Unit =
      resource.allocated
        .map {
          case (rc, cu) =>
            laserdiscClient = rc
            clientCleanUp = cu
        }
        .unsafeRunSync()

    @TearDown(Level.Trial)
    def tearDown(): Unit =
      (clientCleanUp >>
        IO.delay(commandsService.shutdown()) >>
        IO.delay(commandsService.awaitTermination(2, TimeUnit.SECONDS))).unsafeRunSync
  }

  @State(Scope.Benchmark)
  class ZioSetUp extends TestCases {
    import zio.Task
    import zio.internal.Platform
    import zio.interop.catz._
    import zio.interop.catz.implicits._

    val runtime = zio.Runtime.unsafeFromLayer(zio.ZEnv.live, Platform.benchmark)

    implicit val logWriter: LogWriter[Task] = ZioLogWriter.consoleLogUpToLevel(LogLevels.Error)

    val resource = RedisClient.to[Task]("localhost", 6379)

    var laserdiscClient: RedisClient[Task] = _
    var clientCleanUp: Task[Unit]          = _

    @Setup(Level.Trial)
    def setup(): Unit =
      runtime.unsafeRunSync(
        resource.allocated
          .map {
            case (rc, cu) =>
              laserdiscClient = rc
              clientCleanUp = cu
          }
      )

    @TearDown(Level.Trial)
    def tearDown(): Unit =
      runtime.unsafeRunSync(clientCleanUp)
  }

  @State(Scope.Benchmark)
  class JedisSetUp extends JedisTestCases {
    import org.apache.commons.pool2.impl.GenericObjectPoolConfig
    import redis.clients.jedis.{Jedis, JedisPool}

    private[this] val commandsService           = Executors.newFixedThreadPool(8)
    private[this] val ec: ExecutionContext      = fromExecutor(commandsService)
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)
    implicit val timer: Timer[IO]               = IO.timer(ec)

    var jedisClient: Jedis = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      val poolConfig = new GenericObjectPoolConfig
      poolConfig.setMaxTotal(8)
      poolConfig.setMinIdle(0)
      poolConfig.setMaxIdle(0)

      Resource
        .fromAutoCloseable[IO, Jedis](
          IO.delay(new JedisPool(poolConfig, "localhost", 6379).getResource)
        )
        .allocated
        .map { case (jc, _) => jedisClient = jc }
        .unsafeRunSync()
    }

    @TearDown(Level.Trial)
    def tearDown(): Unit =
      (IO.delay(commandsService.shutdown()) >>
        IO.delay(commandsService.awaitTermination(2, TimeUnit.SECONDS))).unsafeRunSync
  }
}

private[fs2] trait TestCases extends Values {

  final def case1[F[_]: Concurrent: ContextShift: Timer: Parallel](cl: RedisClient[F]) =
    for {
      r1 <- (cl.send(rpush("key11", list1)) >>
          cl.send(set("key12", string1)) >>
          cl.send(set("key13", 17)) >>
          cl.send(mset(multi1)) >>
          cl.send(lrange("key11", 0L, 100L))).start
      r2 <- (cl.send(set("key21", string2)) >>
          cl.send(rpush("key22", list2)) >>
          cl.send(lrange("key22", 0L, 100L)) >>
          cl.send(set("key23", 17)) >>
          cl.send(mset(multi2))).start
      r3 <- (cl.send(rpush("key31", list3)) >>
          cl.send(set("key32", string3)) >>
          cl.send(mset(multi3)) >>
          cl.send(lrange("key31", 0L, 100L)) >>
          cl.send(set("key33", 17))).start
      r4 <- (cl.send(set("key41", string4)) >>
          cl.send(set("key42", 17)) >>
          cl.send(rpush("key43", list4)) >>
          cl.send(lrange("key42", 0L, 100L)) >>
          cl.send(mset(multi4))).start
      r5 <- (cl.send(rpush("key51", list5)) >>
          cl.send(set("key52", string5)) >>
          cl.send(set("key53", 17)) >>
          cl.send(mset(multi5)) >>
          cl.send(lrange("key51", 0L, 100L))).start
      r6 <- (cl.send(set("key61", string6)) >>
          cl.send(rpush("key62", list6)) >>
          cl.send(lrange("key62", 0L, 100L)) >>
          cl.send(set("key63", 17)) >>
          cl.send(mset(multi6))).start
      r7 <- (cl.send(set("key71", string7)) >>
          cl.send(set("key72", 17)) >>
          cl.send(rpush("key73", list7)) >>
          cl.send(mset(multi7)) >>
          cl.send(lrange("key73", 0L, 100L))).start
      r8 <- (cl.send(set("key81", string8)) >>
          cl.send(set("key82", 17)) >>
          cl.send(rpush("key83", list8)) >>
          cl.send(lrange("key82", 0L, 100L)) >>
          cl.send(mset(multi8))).start
      j <- (r1.join, r2.join, r3.join, r4.join, r5.join, r6.join, r7.join, r8.join).parTupled
    } yield j
}

private[fs2] trait JedisTestCases extends Values {
  final def case1[F[_]: Concurrent: ContextShift: Timer: Parallel](cl: Jedis) =
    for {
      r1 <- Concurrent[F].delay {
        cl.rpush("key11", list1Str)
        cl.set("key12", string1)
        cl.set("key13", "17")
        cl.mset(multi1Str)
        cl.lrange("key11", 0L, 100L)
      }.start
      r2 <- Concurrent[F].delay {
        cl.set("key21", string2)
        cl.rpush("key22", list2Str)
        cl.lrange("key22", 0L, 100L)
        cl.set("key23", "17")
        cl.mset(multi2Str)
      }.start
      r3 <- Concurrent[F].delay {
        cl.rpush("key31", list3Str)
        cl.set("key32", string3)
        cl.mset(multi3Str)
        cl.lrange("key31", 0L, 100L)
        cl.set("key33", "17")
      }.start
      r4 <- Concurrent[F].delay {
        cl.set("key41", string4)
        cl.set("key42", "17")
        cl.rpush("key43", list4Str)
        cl.lrange("key42", 0L, 100L)
        cl.mset(multi4Str)
      }.start
      r5 <- Concurrent[F].delay {
        cl.rpush("key51", list5Str)
        cl.set("key52", string5)
        cl.set("key53", "17")
        cl.mset(multi5Str)
        cl.lrange("key51", 0L, 100L)
      }.start
      r6 <- Concurrent[F].delay {
        cl.set("key61", string6)
        cl.rpush("key62", list6Str)
        cl.lrange("key62", 0L, 100L)
        cl.set("key63", "17")
        cl.mset(multi6Str)
      }.start
      r7 <- Concurrent[F].delay {
        cl.set("key71", string7)
        cl.set("key72", "17")
        cl.rpush("key73", list7Str)
        cl.mset(multi7Str)
        cl.lrange("key73", 0L, 100L)
      }.start
      r8 <- Concurrent[F].delay {
        cl.set("key81", string8)
        cl.set("key82", "17")
        cl.rpush("key83", list8Str)
        cl.lrange("key82", 0L, 100L)
        cl.mset(multi8Str)
      }.start
      j <- (r1.join, r2.join, r3.join, r4.join, r5.join, r6.join, r7.join, r8.join).parTupled
    } yield j
}

private[fs2] trait Values {
  private[this] def generateList(n: Int): List[String] =
    (1 to n).map(_ => UUID.randomUUID().toString).toList

  private[this] def generateString(n: Int): String =
    (1 to n).map(_ => UUID.randomUUID().toString).mkString(" ")

  private[this] def generateMulti(n: Int): OneOrMore[(Key, Int)] =
    OneOrMore.unsafeFrom(
      (1 to n).map(c => (Key.unsafeFrom(s"keyM$c"), UUID.randomUUID().hashCode())).toList
    )

  val list1 = generateList(200)
  val list2 = generateList(100)
  val list3 = generateList(200)
  val list4 = generateList(100)
  val list5 = generateList(200)
  val list6 = generateList(100)
  val list7 = generateList(200)
  val list8 = generateList(100)

  val string1 = generateString(50)
  val string2 = generateString(40)
  val string3 = generateString(50)
  val string4 = generateString(40)
  val string5 = generateString(50)
  val string6 = generateString(40)
  val string7 = generateString(50)
  val string8 = generateString(40)

  val multi1 = generateMulti(20)
  val multi2 = generateMulti(10)
  val multi3 = generateMulti(20)
  val multi4 = generateMulti(10)
  val multi5 = generateMulti(20)
  val multi6 = generateMulti(10)
  val multi7 = generateMulti(20)
  val multi8 = generateMulti(10)

  val list1Str = list1.mkString(" ")
  val list2Str = list2.mkString(" ")
  val list3Str = list3.mkString(" ")
  val list4Str = list4.mkString(" ")
  val list5Str = list5.mkString(" ")
  val list6Str = list6.mkString(" ")
  val list7Str = list7.mkString(" ")
  val list8Str = list8.mkString(" ")

  val multi1Str = multi1.map { case (k, v) => s"$k $v" }.mkString(" ")
  val multi2Str = multi2.map { case (k, v) => s"$k $v" }.mkString(" ")
  val multi3Str = multi3.map { case (k, v) => s"$k $v" }.mkString(" ")
  val multi4Str = multi4.map { case (k, v) => s"$k $v" }.mkString(" ")
  val multi5Str = multi5.map { case (k, v) => s"$k $v" }.mkString(" ")
  val multi6Str = multi6.map { case (k, v) => s"$k $v" }.mkString(" ")
  val multi7Str = multi7.map { case (k, v) => s"$k $v" }.mkString(" ")
  val multi8Str = multi8.map { case (k, v) => s"$k $v" }.mkString(" ")
}
